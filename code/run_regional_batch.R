# ==============================================================================
# run_regional_batch.R - Parallel execution of all parameter sets
# ==============================================================================
# Usage: source('code/run_regional_batch.R')
#
# This script runs regional simulations for ALL parameter sets in parallel.
# It is called by run_analysis.R as part of the main pipeline.
#
# Prerequisites:
#   - setup.R must have been sourced (loads dependencies and config)
#   - parameters.R must have been sourced (creates pars_list)
#
# Inputs (from environment):
#   - pars_list: List of parameter sets from parameters.R
#   - default_pars: Default parameters from config.R
#   - sim_settings: Simulation settings from config.R
#   - paths: File paths from config.R
#
# Outputs:
#   - One CSV file per parameter set: output/regional_sim_{parset_name}.csv
# ==============================================================================

# Check prerequisites
if (!exists("pars_list")) {
  stop("pars_list not found. Please run source('code/parameters.R') first.")
}

cat("  Total parameter sets:", length(pars_list), "\n")

# Import data once (skip if already loaded)
if (!exists("naws_data")) {
  source('code/import_naws.R')
}
if (!exists("acs_data_regional")) {
  source('code/import_acs.R')
}

# Precompute household state table (constant across all parameter sets)
household_states <- generate_household_state_table(
  n_min = 1,
  n_max = default_pars$max_hh_size,
  crowding = TRUE
)

# Set up parallel processing
if (sim_settings$use_parallel) {
  future::plan(future::multisession)
  cat("  Using parallel processing\n\n")
} else {
  cat("  Using sequential processing\n\n")
}

# Track timing
start_time <- Sys.time()

# Define function to run a single parameter set
run_single_parset <- function(pars, household_states, acs_data_regional, naws_data,
                               sim_settings, paths) {
  # Each worker needs to load packages and source files to get the odin model
  # (compiled odin models can't be passed between R processes)
  library(tidyverse)
  library(odin)
  source('code/utils.R')
  source('code/epimodels.R')

  n_states <- nrow(household_states)

  epidf_indiv_full <- tibble()

  for (region in 1:6) {
    # Create the ic joiners
    ic_joiner_C <- acs_data_regional %>%
      filter(REGION6 == region) %>%
      make_ic_joiner(fold_diff = pars$crowding_fold_diff)

    ic_joiner_A <- naws_data %>%
      filter(REGION6 == region) %>%
      make_ic_joiner(fold_diff = pars$crowding_fold_diff)

    # Adjust the ic joiners to reflect initial infected
    ic_joiner_C_inf <- ic_joiner_C %>%
      mutate(frac = pars$init_prev * frac * hh_size) %>%
      mutate(y = y + 1, x = x - 1)
    ic_joiner_C$frac <- ic_joiner_C$frac - ic_joiner_C_inf$frac
    ic_joiner_C <- bind_rows(ic_joiner_C, ic_joiner_C_inf)

    ic_joiner_A_inf <- ic_joiner_A %>%
      mutate(frac = pars$init_prev * frac * hh_size) %>%
      mutate(y = y + 1, x = x - 1)
    ic_joiner_A$frac <- ic_joiner_A$frac - ic_joiner_A_inf$frac
    ic_joiner_A <- bind_rows(ic_joiner_A, ic_joiner_A_inf)

    # Create the initial conditions
    init_C <- household_states %>%
      left_join(ic_joiner_C, by = c("x", "y", "z", "hh_size", "crowded")) %>%
      arrange(state_index) %>%
      replace_na(list(frac = 0)) %>%
      pull(frac)

    init_A <- household_states %>%
      left_join(ic_joiner_A, by = c("x", "y", "z", "hh_size", "crowded")) %>%
      arrange(state_index) %>%
      replace_na(list(frac = 0)) %>%
      pull(frac)

    pop_cty <- acs_data_regional %>%
      filter(REGION6 == region) %>%
      pull(population) %>%
      first()

    prop_ag <- acs_data_regional %>%
      filter(REGION6 == region) %>%
      pull(prop_ag_workers) %>%
      first()

    pop_C <- pop_cty * (1 - prop_ag)
    pop_A <- pop_cty * prop_ag

    # Initialize model
    mod <- household_model_twopop_crowding$new(
      n_states = n_states,
      x = household_states$x,
      y = household_states$y,
      z = household_states$z,
      hh_size = household_states$hh_size,
      crowded = household_states$crowded,
      rec_index = household_states$rec_index,
      inf_index = household_states$inf_index,
      init_C = init_C,
      init_A = init_A,
      gamma = pars$gamma,
      tau = pars$tau,
      tau_boost = pars$tau_boost,
      beta = pars$beta,
      eps = pars$eps,
      pop_C = pop_C,
      pop_A = pop_A
    )

    # Simulate
    times <- seq(0, sim_settings$t_max, by = sim_settings$t_step)
    out <- as_tibble(data.frame(mod$run(times)))

    epidf_indiv <- format_output_indiv(out, household_states)
    epidf_indiv_full <- bind_rows(epidf_indiv_full, mutate(epidf_indiv, REGION6 = region))
  }

  # Add parameter metadata
  epidf_indiv_full <- epidf_indiv_full %>%
    mutate(
      parset = pars$parset,
      parset_name = pars$parset_name,
      sens_type = pars$sens_type,
      sens_value = pars$sens_value
    )

  # Save output
  output_file <- paste0(paths$regional_output_prefix, pars$parset_name, ".csv")
  write_csv(epidf_indiv_full, file = output_file)


  return(pars$parset_name)
}

# Run all parameter sets in parallel
results <- future_lapply(pars_list, function(pars) {
  run_single_parset(
    pars, household_states, acs_data_regional, naws_data,
    sim_settings, paths
  )
}, future.seed = TRUE)

elapsed <- difftime(Sys.time(), start_time, units = "mins")
cat("  Completed", length(results), "parameter sets in", round(elapsed, 1), "minutes\n")
cat("  Output files:\n")
for (name in unlist(results)) {
  cat("    -", paste0(paths$regional_output_prefix, name, ".csv"), "\n")
}

# ==============================================================================
# Generate diagnostic plots for each parameter set
# ==============================================================================

cat("\n  Generating diagnostic plots...\n")

for (parset_name in unlist(results)) {
  # Load the output file
  output_file <- paste0(paths$regional_output_prefix, parset_name, ".csv")
  epidf <- read_csv(output_file, show_col_types = FALSE)

  # Create infection curve plot (faceted by region)
  fig <- epidf %>%
    pivot_longer(c("S_indiv", "I_indiv", "R_indiv")) %>%
    mutate(name = substr(name, 1, 1)) %>%
    filter(name == "I") %>%
    ggplot(aes(x = t, y = value, col = subpop)) +
    geom_line(alpha = 1, linewidth = 0.8) +
    geom_hline(yintercept = 0.005, lty = "dashed", alpha = 0.2) +
    labs(
      x = "Time (days)",
      y = "Proportion Infected",
      col = "Subpopulation",
      title = paste0("Infection curves: ", parset_name)
    ) +
    theme_minimal() +
    facet_wrap(~factor(REGION6), nrow = 2)

  # Save the plot
  fig_file <- paste0(paths$figures_dir, "/regional_I_", parset_name, ".pdf")
  ggsave(fig_file, fig, width = 10, height = 6)
  cat("    -", fig_file, "\n")
}

cat("  Diagnostic plots complete.\n")
