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
#   - epidf_indiv_national: National-level simulation results
#   - fig_indiv_national: Diagnostic plot of epidemic curves
#   - Prints final attack rate for verification
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
# Calibration Parameters
# ==============================================================================
#
# Parameters for calibration run. Key settings:
#   - Single population only (tau_A=0, beta_A=0, pop_A=0, eps=0)
#   - Community transmission uses household-structured model
#   - Beta value set to test a specific R0 (modify beta_C to test different R0s)
#
# Calibrated beta scalars (multiply by gamma to get beta):
#   R0 = 1.2 → beta_scalar = 0.765
#   R0 = 1.5 → beta_scalar = 1.05
#   R0 = 2.0 → beta_scalar = 1.53
#   R0 = 3.0 → beta_scalar = 2.52

# Helper function to calculate tau from SAR (duplicated from parameters.R for standalone use)
calculate_tau_calibrate <- function(sar, gamma) {
  sar * gamma / (1 - sar)
}

pars_calibrate <- list(
  # Disease dynamics
  gamma = 1/5,                              # Recovery rate (5-day infectious period)

  # SAR-based parameters (primary)
  sar_uncrowded = 0.20,                     # Within-HH SAR: 20% baseline
  sar_crowded = 0.40,                       # Crowded HH SAR: 40%

  # Between-household transmission
  # MODIFY THIS VALUE to calibrate different R0 targets:
  beta_C = 2.52 * (1/5),                    # Currently set for R0 = 3.0
  beta_A = 0,                               # Disabled for calibration

  # Population mixing (single population for calibration)
  eps = 0,                                  # No between-group mixing

  # Household structure
  max_hh_size = 7,
  crowding_fold_diff = 2,

  # Other settings
  adjust_hhvars = TRUE,
  init_prev = 0.001
)

# Compute derived tau parameters from SAR values
pars_calibrate$tau <- calculate_tau_calibrate(pars_calibrate$sar_uncrowded, pars_calibrate$gamma)
pars_calibrate$tau_boost <- calculate_tau_calibrate(pars_calibrate$sar_crowded, pars_calibrate$gamma) - pars_calibrate$tau

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
    # Seed infection: Move 1% of 2-person uncrowded households from fully
    # susceptible (x=2, y=0, z=0) to having one infected (x=1, y=1, z=0)
    #
    # Note: This seeds ~1% of 2-person households, not 1% of total population.
    # For calibration purposes, the exact seeding doesn't affect the final
    # attack rate, only the timing of the epidemic.

    init_nat_C <- household_states %>%
      left_join(ic_joiner, by = c("x", "y", "z", "hh_size", "crowded")) %>%
      arrange(state_index) %>%
      replace_na(list(frac = 0)) %>%
      # Remove 1% from fully susceptible 2-person uncrowded households
      mutate(frac = case_when(
        x == 2 & y == 0 & z == 0 & crowded == 0 ~ frac - 0.01,
        TRUE ~ frac
      )) %>%
      # Add 1% to 2-person households with 1 infected
      mutate(frac = case_when(
        x == 1 & y == 1 & z == 0 & crowded == 0 ~ 0.01,
        TRUE ~ frac
      )) %>%
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
      beta_C = beta_C,
      beta_A = beta_A,
      eps = eps,
      pop_C = 10000,   # Arbitrary (model is normalized)
      pop_A = 0        # Disabled for single-population calibration
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
# Run Calibration
# ==============================================================================

epidf_indiv_national <- run_calibration_sim(pars_calibrate, nat_data)

# ==============================================================================
# Diagnostic Plots
# ==============================================================================

# Epidemic curves (S, I, R over time)
fig_indiv_national <- epidf_indiv_national %>%
  pivot_longer(c("S_indiv", "I_indiv", "R_indiv")) %>%
  mutate(name = substr(name, 1, 1)) %>%
  ggplot(aes(x = t, y = value, col = name, lty = subpop)) +
  geom_line() +
  expand_limits(y = 0) +
  labs(
    x = "Time (days)",
    y = "Proportion",
    color = "Compartment",
    linetype = "Population",
    title = "National Calibration: Epidemic Dynamics"
  ) +
  theme_minimal()

# Relative infection rate (A/C) - should be undefined since pop_A = 0
fig_rel_inf_national <- epidf_indiv_national %>%
  select(t, subpop, I_indiv) %>%
  pivot_wider(names_from = "subpop", values_from = "I_indiv") %>%
  mutate(rel_inf = A / C) %>%
  ggplot(aes(x = t, y = rel_inf)) +
  geom_line() +
  labs(
    x = "Time (days)",
    y = "Relative Infection Rate (A/C)",
    title = "Relative Infection Rate"
  ) +
  theme_minimal()

# ==============================================================================
# Verification: Compare Final Attack Rate to Theoretical Prediction
# ==============================================================================
#
# Expected final attack rates (R∞) for different R0 values:
#   R0 = 1.2 → R∞ ≈ 0.31
#   R0 = 1.5 → R∞ ≈ 0.58
#   R0 = 2.0 → R∞ ≈ 0.80
#   R0 = 3.0 → R∞ ≈ 0.94
#
# The current pars_calibrate uses beta for R0 = 3.0, so we expect R∞ ≈ 0.94

cat("\n========================================\n")
cat("Calibration Verification\n")
cat("========================================\n")
cat("Current beta_C:", pars_calibrate$beta_C, "\n")
cat("Expected R0: 3.0 (based on beta_scalar = 2.52)\n")
cat("Expected final attack rate: ~0.94\n")
cat("Observed final attack rate:", round(last(epidf_indiv_national$R_indiv), 4), "\n")
cat("========================================\n")
