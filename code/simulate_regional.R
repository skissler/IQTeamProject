# ==============================================================================
# simulate_regional.R - Regional Epidemic Simulations
# ==============================================================================
# Runs the household-structured epidemic model for each NAWS region, using
# region-specific household size distributions and crowding rates.
#
# Requires: pars object (from parameters.R or run_analysis.R loop)
# Outputs: regional_sim_{parset_name}.csv in output directory
# ==============================================================================

# Load dependencies (skip if already loaded via run_analysis.R)
if (!exists("paths")) {
  source('code/setup.R')
}

# Import data (skip if already loaded)
if (!exists("naws_data")) {
  source('code/import_naws.R')
}
if (!exists("acs_data_regional")) {
  source('code/import_acs.R')
}

# ==============================================================================
# Run the Simulation
# ==============================================================================

run_regional_sim <- function(pars, acs_data_regional, naws_data) {
with(as.list(pars), {

# Load household state definitions
household_states <- generate_household_state_table(n_min = 1, n_max = max_hh_size, crowding = TRUE)
n_states <- nrow(household_states)

epidf_indiv_full <- tibble()

for (region in 1:n_regions) {

  # Create the ic joiners
  ic_joiner_C <- acs_data_regional %>%
    filter(REGION6 == region) %>%
    make_ic_joiner(fold_diff = crowding_fold_diff)

  ic_joiner_A <- naws_data %>%
    filter(REGION6 == region) %>%
    make_ic_joiner(fold_diff = crowding_fold_diff)

  # Backward compatibility: derive per-subpop init_prev if not provided
  if (!exists("init_prev_C")) init_prev_C <- init_prev
  if (!exists("init_prev_A")) init_prev_A <- init_prev

  # Adjust the ic joiners to reflect initial infected
  ic_joiner_C_inf <- ic_joiner_C %>%
    mutate(frac = init_prev_C * frac * hh_size) %>%
    mutate(y = y + 1, x = x - 1)
  ic_joiner_C$frac <- ic_joiner_C$frac - ic_joiner_C_inf$frac
  ic_joiner_C <- bind_rows(ic_joiner_C, ic_joiner_C_inf)

  ic_joiner_A_inf <- ic_joiner_A %>%
    mutate(frac = init_prev_A * frac * hh_size) %>%
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
    gamma = gamma,
    tau = tau,
    tau_boost = tau_boost,
    beta = beta,
    eps = eps,
    pop_C = pop_C,
    pop_A = pop_A
  )

  # Simulate
  times <- seq(0, sim_settings$t_max, by = sim_settings$t_step)
  out <- as_tibble(data.frame(mod$run(times)))

  epidf_indiv <- format_output_indiv(out, household_states)
  epidf_indiv_full <- bind_rows(epidf_indiv_full, mutate(epidf_indiv, REGION6 = region))
}

return(epidf_indiv_full)
})
}

epidf_indiv_full <- run_regional_sim(pars, acs_data_regional, naws_data)

# ==============================================================================
# Save Output
# ==============================================================================

# Use parset_name if available (new format), fall back to parset number (legacy)
output_suffix <- if (!is.null(pars$parset_name)) pars$parset_name else as.character(pars$parset)
output_file <- paste0(paths$regional_output_prefix, output_suffix, ".csv")

# Add parameter metadata to output
epidf_indiv_full <- epidf_indiv_full %>%
  mutate(
    parset = pars$parset,
    parset_name = if (!is.null(pars$parset_name)) pars$parset_name else paste0("parset_", pars$parset),
    sens_type = if (!is.null(pars$sens_type)) pars$sens_type else "legacy",
    sens_value = if (!is.null(pars$sens_value)) pars$sens_value else NA_real_
  )

write_csv(epidf_indiv_full, file = output_file)
cat("Saved:", output_file, "\n")

# ==============================================================================
# Diagnostic Plots (optional, for interactive use)
# ==============================================================================

fig_indiv_full_I_regionfacet <- epidf_indiv_full %>%
  pivot_longer(c("S_indiv", "I_indiv", "R_indiv")) %>%
  mutate(name = substr(name, 1, 1)) %>%
  filter(name == "I") %>%
  ggplot(aes(x = t, y = value, col = subpop)) +
    geom_line(alpha = 1, linewidth = 0.8) +
    geom_hline(yintercept = 0.005, lty = "dashed", alpha = 0.2) +
    labs(x = "Time", y = "Proportion Infected", col = "Subpopulation",
         title = paste0("Parameter set: ", output_suffix)) +
    theme_minimal() +
    facet_wrap(~factor(REGION6), nrow = 2)

fig_indiv_full_R_regionfacet <- epidf_indiv_full %>%
  pivot_longer(c("S_indiv", "I_indiv", "R_indiv")) %>%
  mutate(name = substr(name, 1, 1)) %>%
  filter(name == "R") %>%
  ggplot(aes(x = t, y = value, col = subpop)) +
    geom_line(alpha = 1, linewidth = 0.8) +
    geom_hline(yintercept = 0.005, lty = "dashed", alpha = 0.2) +
    labs(x = "Time", y = "Proportion Recovered", col = "Subpopulation",
         title = paste0("Parameter set: ", output_suffix)) +
    theme_minimal() +
    facet_wrap(~factor(REGION6), nrow = 2)

fig_rel_inf <- epidf_indiv_full %>%
  select(t, subpop, I_indiv, REGION6) %>%
  pivot_wider(names_from = "subpop", values_from = "I_indiv") %>%
  mutate(rel_inf = A / C) %>%
  ggplot(aes(x = t, y = rel_inf, group = REGION6)) +
    geom_line(alpha = 0.8, linewidth = 1) +
    geom_hline(yintercept = 1, lty = "dashed", col = "lightgrey") +
    expand_limits(y = 0.5) +
    labs(title = paste0("Relative infection rate (A/C): ", output_suffix)) +
    theme_classic()
