# ==============================================================================
# calibrate_model.R - National-Level Model Calibration
# ==============================================================================
#
# Purpose:
#   Calibrates the between-household transmission rate (beta) to achieve target
#   R0 values. Uses national-level aggregated ACS household data to run a
#   single-population simulation and verify that the final attack rate matches
#   theoretical predictions.
#
# Methodology:
#   For an SIR model, the relationship between R0 and final attack rate (R∞) is:
#     R∞ = 1 - exp(-R0 * R∞)
#
#   Expected final attack rates for common R0 values:
#     R0 = 1.2 → R∞ ≈ 0.31 (31%)
#     R0 = 1.5 → R∞ ≈ 0.58 (58%)
#     R0 = 2.0 → R∞ ≈ 0.80 (80%)
#     R0 = 3.0 → R∞ ≈ 0.94 (94%)
#
#   By running the household-structured model with a candidate beta value and
#   comparing the final attack rate to these theoretical predictions, we can
#   verify that our calibration is correct.
#
# Note:
#   This script runs a SINGLE-POPULATION calibration (agricultural workers
#   disabled) to isolate the community transmission dynamics. The calibrated
#   beta values are then used in the full two-population model.
#
# Outputs:
#   - calibrated_betas_df: Data frame (r0, sar_crowded, fold_diff, gamma, beta)
#     for all one-at-a-time sensitivity combinations (36 rows). Written to
#     output/calibrated_betas.csv.
#   - calibrated_betas: Named vector (R0 → beta) for backward compatibility
#     (baseline SAR/fold only)
#   - fig_calibration_verification: Diagnostic plot of epidemic curves
#
# ==============================================================================

# ==============================================================================
# Dependencies
# ==============================================================================

# Load dependencies (skip if already loaded via run_analysis.R)
if (!exists("paths")) {
  source('code/setup.R')
}

# Import data (skip if already loaded)
if (!exists("naws_data")) {
  source('code/import_naws.R')
}
if (!exists("acs_data")) {
  source('code/import_acs.R')
}

# ==============================================================================
# Aggregate ACS Data to National Level
# ==============================================================================
#
# Aggregates county-level ACS data to create national household size and
# crowding distributions. Uses population-weighted averages.
#
# Input: acs_data (county-level, columns: hhSize, prop, prop_crowded, population)
# Output: nat_data (national-level, same structure)

nat_data <- acs_data %>%
  mutate(
    # Convert proportions to counts (weighted by population)
    prop = prop * population,
    prop_crowded = prop_crowded * population
  ) %>%
  group_by(hhSize) %>%
  summarise(
    prop = sum(prop),
    prop_crowded = sum(prop_crowded),
    population = sum(population),
    .groups = "drop"
  ) %>%
  mutate(
    # Convert back to proportions (national averages)
    prop = prop / population,
    prop_crowded = prop_crowded / population
  )

# ==============================================================================
# Calibration Helper Functions
# ==============================================================================

#' Solve for theoretical final epidemic size from R0
#'
#' Solves the implicit equation R_inf = 1 - exp(-R0 * R_inf) using fixed-point
#' iteration. For R0 <= 1, returns 0 (no epidemic).
#'
#' @param r0 Basic reproduction number
#' @param tol Convergence tolerance (default 1e-10)
#' @param max_iter Maximum iterations (default 1000)
#' @return Final attack rate (proportion infected at end of epidemic)
solve_final_size <- function(r0, tol = 1e-10, max_iter = 1000) {
  # For R0 <= 1, final size is 0 (no epidemic)
  if (r0 <= 1) return(0)

  # Fixed-point iteration: R_inf = 1 - exp(-R0 * R_inf)
  r_inf <- 0.5  # Initial guess
  for (i in 1:max_iter) {
    r_inf_new <- 1 - exp(-r0 * r_inf)
    if (abs(r_inf_new - r_inf) < tol) return(r_inf_new)
    r_inf <- r_inf_new
  }
  warning("solve_final_size did not converge")
  return(r_inf)
}

#' Get final attack rate from a single simulation
#'
#' Runs a calibration simulation with a given beta and returns the
#' final attack rate for the community population.
#'
#' @param beta Between-household transmission rate
#' @param base_pars List of base parameters (without beta set)
#' @param nat_data National-level household data
#' @return Final attack rate (proportion recovered at end of simulation)
get_final_attack_rate <- function(beta, base_pars, nat_data) {
  pars <- base_pars
  pars$beta <- beta

  result <- run_calibration_sim(pars, nat_data)

  # Get final attack rate for community population
  # Use ungroup() to handle any residual grouping from format_output_indiv
  result_C <- result %>%
    ungroup() %>%
    filter(subpop == "C")

  # Get R_indiv at the maximum time point
  final_R <- result_C$R_indiv[which.max(result_C$t)]

  return(final_R)
}

#' Calibrate beta using bisection search
#'
#' Finds the beta value that produces a final attack rate matching
#' the theoretical prediction for a given R0.
#'
#' @param target_r0 Target R0 value to calibrate for
#' @param base_pars List of base parameters (without beta set)
#' @param nat_data National-level household data
#' @param tol Convergence tolerance for final size difference (default 0.0005)
#' @param max_iter Maximum bisection iterations (default 50)
#' @param beta_lower Lower bound for beta search (default 0.01 * gamma)
#' @param beta_upper Upper bound for beta search (default 10.0 * gamma)
#' @return List with r0, beta, target_final_size, simulated_final_size,
#'         iterations, and eval_history (data frame of all evaluated points)
calibrate_beta <- function(target_r0, base_pars, nat_data,
                           tol = 0.0005, max_iter = 50,
                           beta_lower = 0.01 * base_pars$gamma,
                           beta_upper = 10.0 * base_pars$gamma) {

  # Calculate target final size
  target_final_size <- solve_final_size(target_r0)

  cat("Calibrating for R0 =", target_r0, "\n")
  cat("  Target final size:", round(target_final_size, 4), "\n")
  cat("  Initial bounds: [", round(beta_lower, 4), ", ", round(beta_upper, 4), "]\n", sep = "")

  # Track all evaluated (beta, final_size) pairs for warm-starting future calibrations
  eval_history <- data.frame(beta = numeric(), final_size = numeric())

  # Bisection search
  for (iter in 1:max_iter) {
    beta_mid <- (beta_lower + beta_upper) / 2

    sim_final_size <- get_final_attack_rate(beta_mid, base_pars, nat_data)
    error <- sim_final_size - target_final_size

    # Record this evaluation
    eval_history <- rbind(eval_history, data.frame(beta = beta_mid, final_size = sim_final_size))

    cat("  Iter", iter, ": beta =", round(beta_mid, 4),
        ", final_size =", round(sim_final_size, 4),
        ", error =", round(error, 5), "\n")

    # Check convergence
    if (abs(error) < tol) {
      cat("  Converged! beta =", round(beta_mid, 4), "\n\n")
      return(list(
        r0 = target_r0,
        beta = beta_mid,
        target_final_size = target_final_size,
        simulated_final_size = sim_final_size,
        iterations = iter,
        eval_history = eval_history
      ))
    }

    # Update bounds
    if (error > 0) {
      beta_upper <- beta_mid  # Final size too high, reduce beta
    } else {
      beta_lower <- beta_mid  # Final size too low, increase beta
    }
  }

  warning("Calibration did not converge for R0 = ", target_r0)
  return(list(r0 = target_r0, beta = beta_mid,
              target_final_size = target_final_size,
              simulated_final_size = sim_final_size,
              iterations = max_iter,
              eval_history = eval_history))
}

#' Find bounds for a target final size from evaluation history
#'
#' Given a history of (beta, final_size) pairs and a target final size,
#' finds the tightest bounds [beta_lower, beta_upper] that bracket the target.
#'
#' @param eval_history Data frame with beta and final_size columns
#' @param target_final_size Target final size to bracket
#' @param default_lower Default lower bound if no suitable point found
#' @param default_upper Default upper bound if no suitable point found
#' @return Named list with beta_lower and beta_upper
find_bounds_from_history <- function(eval_history, target_final_size,
                                      default_lower = 0.01, default_upper = 10.0) {
  if (nrow(eval_history) == 0) {
    return(list(beta_lower = default_lower, beta_upper = default_upper))
  }

  # Points below target (need higher beta)
  below <- eval_history[eval_history$final_size < target_final_size, ]
  # Points above target (need lower beta)
  above <- eval_history[eval_history$final_size > target_final_size, ]

  # Best lower bound: highest beta that gave final_size below target
  if (nrow(below) > 0) {
    beta_lower <- max(below$beta)
  } else {
    beta_lower <- default_lower
  }

  # Best upper bound: lowest beta that gave final_size above target
  if (nrow(above) > 0) {
    beta_upper <- min(above$beta)
  } else {
    beta_upper <- default_upper
  }

  return(list(beta_lower = beta_lower, beta_upper = beta_upper))
}

# Base parameters for calibration, derived from default_pars in config.R
# Beta will be set during calibration; eps=0 for single-population calibration
# SAR, fold_diff, tau, and tau_boost are overridden per-grid-row below
base_pars <- list(
  gamma = default_pars$gamma,
  sar_uncrowded = default_pars$sar_uncrowded,
  sar_crowded = default_pars$sar_crowded,
  eps = 0,
  max_hh_size = default_pars$max_hh_size,
  crowding_fold_diff = default_pars$crowding_fold_diff,
  adjust_hhvars = default_pars$adjust_hhvars,
  init_prev = default_pars$init_prev
)

# Compute derived tau parameters from SAR values
base_pars$tau <- calculate_tau(base_pars$sar_uncrowded, base_pars$gamma)
base_pars$tau_boost <- calculate_tau_boost(base_pars$sar_crowded, base_pars$gamma, base_pars$tau)

#' Build base_pars variant for a specific (sar_crowded, fold_diff) combination
#'
#' @param sar_crowded SAR for crowded households
#' @param fold_diff Crowding fold difference
#' @return A copy of base_pars with tau, tau_boost, sar_crowded, and
#'         crowding_fold_diff updated
make_calibration_pars <- function(sar_crowded, fold_diff, gamma = base_pars$gamma) {
  pars <- base_pars
  pars$gamma <- gamma
  pars$sar_crowded <- sar_crowded
  pars$crowding_fold_diff <- fold_diff
  pars$tau <- calculate_tau(pars$sar_uncrowded, pars$gamma)
  pars$tau_boost <- calculate_tau_boost(sar_crowded, pars$gamma, pars$tau)
  pars
}

# ==============================================================================
# Calibration Simulation Function
# ==============================================================================
#
#' Run calibration simulation at national level
#'
#' Runs the household-structured epidemic model using national-level household
#' distributions to verify that a given beta value produces the expected
#' final attack rate for the target R0.
#'
#' @param pars List of parameters (see pars_calibrate above)
#' @param nat_data National-level household data (from ACS aggregation)
#' @return Tibble with individual-level epidemic trajectories (t, subpop, S/I/R)
#'
#' @details
#' The simulation uses only the community population (agricultural workers
#' disabled) to isolate the effect of household structure on transmission.
#' Initial conditions seed 1% of 2-person uncrowded households with one
#' infected member.

run_calibration_sim <- function(pars, nat_data) {
  with(as.list(pars), {

    # Generate household state table
    household_states <- generate_household_state_table(
      n_min = 1,
      n_max = max_hh_size,
      crowding = TRUE
    )
    n_states <- nrow(household_states)

    # Create initial condition joiner from national household distribution
    ic_joiner <- make_ic_joiner(nat_data, fold_diff = crowding_fold_diff)

    # --------------------------------------------------------------------------
    # Set up initial conditions for community population
    # --------------------------------------------------------------------------
    # Seed infection using the same approach as simulate.R / simulate_regional.R:
    # For each household size, move init_prev * frac * hh_size fraction from
    # fully susceptible (x=n, y=0, z=0) to one-infected (x=n-1, y=1, z=0).
    # This achieves an individual-level initial prevalence of init_prev.
    #
    # For calibration purposes, the exact seeding doesn't affect the final
    # attack rate, only the timing of the epidemic.

    ic_joiner_inf <- ic_joiner %>%
      mutate(frac = init_prev * frac * hh_size) %>%
      mutate(y = y + 1, x = x - 1)
    ic_joiner$frac <- ic_joiner$frac - ic_joiner_inf$frac
    ic_joiner <- bind_rows(ic_joiner, ic_joiner_inf)

    init_nat_C <- household_states %>%
      left_join(ic_joiner, by = c("x", "y", "z", "hh_size", "crowded")) %>%
      arrange(state_index) %>%
      replace_na(list(frac = 0)) %>%
      pull(frac)

    # --------------------------------------------------------------------------
    # Dummy initial conditions for agricultural workers (disabled)
    # --------------------------------------------------------------------------
    # All in first state (1-person susceptible households) - doesn't matter
    # since pop_A = 0
    init_nat_A <- rep(0, n_states)
    init_nat_A[1] <- 1

    # --------------------------------------------------------------------------
    # Initialize and run model
    # --------------------------------------------------------------------------
    mod_national <- household_model_twopop_crowding$new(
      n_states = n_states,
      x = household_states$x,
      y = household_states$y,
      z = household_states$z,
      hh_size = household_states$hh_size,
      crowded = household_states$crowded,
      rec_index = household_states$rec_index,
      inf_index = household_states$inf_index,
      init_C = init_nat_C,
      init_A = init_nat_A,
      gamma = gamma,
      tau = tau,
      tau_boost = tau_boost,
      beta = beta,
      eps = eps,
      pop_C = 10000,   # Arbitrary (model is normalized)
      pop_A = 0,       # Disabled for single-population calibration
      vax_mult_C = 1,  # No vaccination during calibration
      vax_mult_A = 1
    )

    # Run simulation for 1000 days (ensure epidemic completes)
    times <- seq(0, 1000, by = 1)
    out_national <- as_tibble(data.frame(mod_national$run(times)))

    # Format output
    epidf_hh_national <- format_output_hh(out_national, household_states)
    epidf_indiv_national <- format_output_indiv(out_national, household_states)

    return(epidf_indiv_national)
  })
}

# ==============================================================================
# Run Calibration: Bisection Search Over (R0, SAR, fold_diff) Grid
# ==============================================================================
#
# Calibrates beta values for all one-at-a-time sensitivity combinations of
# R0, SAR in crowded households, and crowding fold difference.
#
# One-at-a-time design: at most one of SAR/fold_diff differs from baseline.
# eps does not affect calibration (eps=0 for single-population model).

r0_targets <- c(1.2, 1.5, 2.0, 3.0)
sar_crowded_values <- c(0.20, 0.30, 0.40, 0.50, 0.60)
fold_diff_values <- c(1, 2, 3)
gamma_values <- c(1/3, 1/5, 1/10)

# Build one-at-a-time calibration grid
calibration_grid <- expand.grid(
  r0 = r0_targets,
  sar_crowded = sar_crowded_values,
  fold_diff = fold_diff_values,
  gamma = gamma_values,
  stringsAsFactors = FALSE
) %>%
  # Keep rows where at most one of SAR/fold/gamma differs from baseline
  filter(
    (sar_crowded != default_pars$sar_crowded) +
    (fold_diff != default_pars$crowding_fold_diff) +
    (abs(gamma - default_pars$gamma) > 1e-10) <= 1
  ) %>%
  arrange(sar_crowded, fold_diff, gamma, r0)

cat("\n========================================\n")
cat("Starting Calibration via Bisection Search\n")
cat("========================================\n")
cat("R0 targets:", paste(r0_targets, collapse = ", "), "\n")
cat("SAR crowded values:", paste(sar_crowded_values, collapse = ", "), "\n")
cat("Fold diff values:", paste(fold_diff_values, collapse = ", "), "\n")
cat("Gamma values:", paste(round(gamma_values, 4), collapse = ", "), "\n")
cat("Total calibrations:", nrow(calibration_grid), "\n")
cat("Convergence tolerance: 0.0005\n")
cat("========================================\n\n")

# Run calibration for each row with cold-start bounds.
# Warm-starting across rows is invalid because changing SAR or fold_diff
# alters the beta-to-final-size mapping, making prior bounds misleading.
calibration_results <- vector("list", nrow(calibration_grid))

for (i in seq_len(nrow(calibration_grid))) {
  row <- calibration_grid[i, ]
  cal_pars <- make_calibration_pars(row$sar_crowded, row$fold_diff, row$gamma)

  cat(sprintf("\n--- Grid row %d/%d: R0=%.1f, SAR_crowded=%.2f, fold_diff=%d, gamma=%.4f ---\n",
              i, nrow(calibration_grid), row$r0, row$sar_crowded, row$fold_diff, row$gamma))

  result <- calibrate_beta(row$r0, cal_pars, nat_data, tol = 0.0005)

  result$sar_crowded <- row$sar_crowded
  result$fold_diff <- row$fold_diff
  result$gamma <- row$gamma
  calibration_results[[i]] <- result
}

# Build calibrated_betas data frame (primary output)
calibrated_betas_df <- tibble(
  r0 = sapply(calibration_results, `[[`, "r0"),
  sar_crowded = sapply(calibration_results, `[[`, "sar_crowded"),
  fold_diff = sapply(calibration_results, `[[`, "fold_diff"),
  gamma = sapply(calibration_results, `[[`, "gamma"),
  beta = sapply(calibration_results, `[[`, "beta"),
  target_final_size = sapply(calibration_results, `[[`, "target_final_size"),
  simulated_final_size = sapply(calibration_results, `[[`, "simulated_final_size"),
  iterations = sapply(calibration_results, `[[`, "iterations")
)

# Write to CSV
write_csv(calibrated_betas_df, file.path(paths$output_dir, "calibrated_betas.csv"))

# Backward-compatible named vector (baseline SAR + fold + gamma only)
baseline_rows <- calibrated_betas_df %>%
  filter(sar_crowded == default_pars$sar_crowded,
         fold_diff == default_pars$crowding_fold_diff,
         abs(gamma - default_pars$gamma) < 1e-10)
calibrated_betas <- setNames(baseline_rows$beta, as.character(baseline_rows$r0))

# ==============================================================================
# Calibration Results Summary
# ==============================================================================

cat("\n========================================\n")
cat("Calibration Results Summary\n")
cat("========================================\n")
cat(sprintf("%-6s %-10s %-6s %-8s %-12s %-12s %-12s %-12s %-6s\n",
            "R0", "SAR_crow", "fold", "gamma", "beta", "beta/gamma", "target", "sim_size", "iters"))
cat(strrep("-", 90), "\n")
for (i in seq_len(nrow(calibrated_betas_df))) {
  row <- calibrated_betas_df[i, ]
  cat(sprintf("%-6.1f %-10.2f %-6d %-8.4f %-12.4f %-12.4f %-12.4f %-12.4f %-6d\n",
              row$r0, row$sar_crowded, row$fold_diff, row$gamma,
              row$beta, row$beta / row$gamma,
              row$target_final_size, row$simulated_final_size, row$iterations))
}
cat("========================================\n")
cat("Total calibrations:", nrow(calibrated_betas_df), "\n")
cat("Saved to:", file.path(paths$output_dir, "calibrated_betas.csv"), "\n")

# ==============================================================================
# Diagnostic Plot: Verify Final Calibration
# ==============================================================================
#
# Run a final simulation with each calibrated beta and plot the results
# to visually verify the calibration.

# Run final verification simulations for all R0 values
verification_results <- lapply(seq_along(r0_targets), function(i) {
  pars <- base_pars
  pars$beta <- calibrated_betas[i]
  result <- run_calibration_sim(pars, nat_data)
  result$r0 <- r0_targets[i]
  result
})

# Combine results
verification_df <- bind_rows(verification_results)

# Create diagnostic plot showing epidemic curves for all R0 values
fig_calibration_verification <- verification_df %>%
  filter(subpop == "C") %>%
  pivot_longer(c("S_indiv", "I_indiv", "R_indiv")) %>%
  mutate(
    name = substr(name, 1, 1),
    r0_label = paste0("R0 = ", r0)
  ) %>%
  ggplot(aes(x = t, y = value, col = name)) +
  geom_line() +
  facet_wrap(~r0_label, nrow = 2) +
  expand_limits(y = 0) +
  labs(
    x = "Time (days)",
    y = "Proportion",
    color = "Compartment",
    title = "Calibration Verification: Epidemic Dynamics by R0"
  ) +
  theme_minimal()

ggsave(file.path(paths$figures_dir, "calibration_verification.pdf"),
       fig_calibration_verification, width = 8, height = 5)

# Key outputs:
# - calibrated_betas_df: Data frame with all (R0, SAR, fold_diff, beta) combos
# - calibrated_betas: Named vector (R0 → beta) for backward compat (baseline SAR/fold only)
