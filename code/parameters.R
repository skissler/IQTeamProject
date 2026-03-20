# ==============================================================================
# parameters.R - Sensitivity Analysis Parameter Sets
# ==============================================================================
# Defines all parameter combinations for the sensitivity analyses.
#
# Sensitivity dimensions (one-at-a-time analysis):
#   1. R0 values: 1.2, 1.5 (baseline), 2.0, 3.0
#   2. Assortativity (eta = 1-eps): 0, 1/4, 1/3, 1/2, 2/3 (baseline), 3/4
#   3. SAR in crowded households: 20%, 30%, 40% (baseline), 50%, 60%
#   4. Crowding fold difference: 1, 2 (baseline), 3
#   5. Gamma (recovery rate): 1/3, 1/5 (baseline), 1/10
#   6. Seed target: C only, both (baseline), A only
#
# Baseline parameters are defined in config.R (default_pars)
# ==============================================================================

# ==============================================================================
# Guard: calibrated_betas and calibrated_betas_df must be available
# ==============================================================================

if (!exists("calibrated_betas") || !exists("calibrated_betas_df")) {
  stop("calibrated_betas / calibrated_betas_df not found. Run calibrate_model.R first.")
}

# ==============================================================================
# Helper Functions
# ==============================================================================
# Note: calculate_tau() and calculate_tau_boost() are defined in utils.R

#' Look up calibrated beta from the calibration grid
#'
#' @param df Data frame with columns r0, sar_crowded, fold_diff, beta
#' @param r0 Target R0
#' @param sar_crowded SAR for crowded households
#' @param fold_diff Crowding fold difference
#' @return Calibrated beta value
lookup_beta <- function(df, r0, sar_crowded, fold_diff, gamma = default_pars$gamma) {
  row <- df[df$r0 == r0 & df$sar_crowded == sar_crowded &
            df$fold_diff == fold_diff & abs(df$gamma - gamma) < 1e-10, ]
  if (nrow(row) == 0) {
    stop(sprintf("No calibrated beta found for r0=%.1f, sar_crowded=%.2f, fold_diff=%d, gamma=%.4f",
                 r0, sar_crowded, fold_diff, gamma))
  }
  row$beta[1]
}

#' Create a parameter set with descriptive naming
#' @param sens_type Character. Sensitivity dimension: "r0", "eps", "sar", "fold", "gamma", "seed"
#' @param sens_value Numeric. The value being varied for this sensitivity
#' @param parset Numeric. Unique parameter set ID
#' @param gamma Recovery rate (1/infectious period)
#' @param sar_uncrowded SAR for uncrowded households (used to compute tau)
#' @param sar_crowded Target SAR for crowded households (used to compute tau_boost)
#' @param beta Between-household transmission rate (calibrated by calibrate_model.R)
#' @param eps Assortativity parameter
#' @param crowding_fold_diff Crowding fold difference
#' @param max_hh_size Maximum household size modeled
#' @param adjust_hhvars How to adjust HH vars: "none", "multiplicative", or "additive"
#' @param init_prev Initial prevalence
#' @param seed_target Which subpopulations to seed: "both", "A", or "C"
#' @return Named list with all parameters and metadata
create_parset <- function(sens_type, sens_value, parset,
                          gamma, sar_uncrowded, sar_crowded,
                          beta, eps, crowding_fold_diff,
                          max_hh_size, adjust_hhvars, init_prev,
                          seed_target = "both") {

  # Compute tau and tau_boost from SAR values
  tau <- calculate_tau(sar_uncrowded, gamma)
  tau_boost <- calculate_tau_boost(sar_crowded, gamma, tau)

  # Derive per-subpopulation initial prevalence from seed_target
  init_prev_A <- if (seed_target %in% c("both", "A")) init_prev else 0
  init_prev_C <- if (seed_target %in% c("both", "C")) init_prev else 0

  list(
    # Metadata for tracking
    parset = parset,
    sens_type = sens_type,           # Sensitivity dimension
    sens_value = sens_value,         # Value being varied
    parset_name = paste0(sens_type, "_", round(sens_value, 4)),  # Descriptive name

    # Epidemiological parameters
    gamma = gamma,
    sar_uncrowded = sar_uncrowded,   # Primary parameter
    sar_crowded = sar_crowded,       # Primary parameter
    tau = tau,                       # Derived from sar_uncrowded
    tau_boost = tau_boost,           # Derived from sar_crowded - sar_uncrowded
    beta = beta,
    eps = eps,

    # Household structure
    max_hh_size = max_hh_size,
    crowding_fold_diff = crowding_fold_diff,

    # Simulation settings
    adjust_hhvars = adjust_hhvars,
    init_prev = init_prev,
    seed_target = seed_target,
    init_prev_A = init_prev_A,       # Derived from seed_target
    init_prev_C = init_prev_C        # Derived from seed_target
  )
}

# ==============================================================================
# Sensitivity Parameter Values
# ==============================================================================
# Note: calibrated_betas is produced by calibrate_model.R
# Baseline values are defined in config.R (default_pars)

r0_values <- c(1.2, 1.5, 2.0, 3.0)
eps_values <- c(1/4, 1/3, 1/2, 2/3, 3/4, 1)
sar_crowded_values <- c(0.20, 0.30, 0.40, 0.50, 0.60)
fold_diff_values <- c(1, 2, 3)
gamma_values <- c(1/3, 1/5, 1/10)

# ==============================================================================
# Generate Parameter Sets
# ==============================================================================

pars_list <- list()
parset_counter <- 0

# --- R0 Sensitivity (baseline for other parameters) ---
for (r0 in r0_values) {
  parset_counter <- parset_counter + 1
  pars_list[[parset_counter]] <- create_parset(
    sens_type = "r0",
    sens_value = r0,
    parset = parset_counter,
    gamma = default_pars$gamma,
    sar_uncrowded = default_pars$sar_uncrowded,
    sar_crowded = default_pars$sar_crowded,
    beta = calibrated_betas[as.character(r0)],
    eps = default_pars$eps,
    crowding_fold_diff = default_pars$crowding_fold_diff,
    max_hh_size = default_pars$max_hh_size,
    adjust_hhvars = default_pars$adjust_hhvars,
    init_prev = default_pars$init_prev
  )
}

# --- Assortativity (eps) Sensitivity ---
# Skip eps=0.33 since it's already in R0 sensitivity at baseline R0
for (eps in eps_values[eps_values != default_pars$eps]) {
  parset_counter <- parset_counter + 1
  pars_list[[parset_counter]] <- create_parset(
    sens_type = "eps",
    sens_value = eps,
    parset = parset_counter,
    gamma = default_pars$gamma,
    sar_uncrowded = default_pars$sar_uncrowded,
    sar_crowded = default_pars$sar_crowded,
    beta = calibrated_betas[as.character(default_pars$r0)],
    eps = eps,
    crowding_fold_diff = default_pars$crowding_fold_diff,
    max_hh_size = default_pars$max_hh_size,
    adjust_hhvars = default_pars$adjust_hhvars,
    init_prev = default_pars$init_prev
  )
}

# --- SAR in Crowded Households Sensitivity ---
# Skip 40% since it's already in R0 sensitivity at baseline R0
# Beta is recalibrated for each SAR value to hold R0 constant
for (sar in sar_crowded_values[sar_crowded_values != default_pars$sar_crowded]) {
  parset_counter <- parset_counter + 1
  pars_list[[parset_counter]] <- create_parset(
    sens_type = "sar",
    sens_value = sar,
    parset = parset_counter,
    gamma = default_pars$gamma,
    sar_uncrowded = default_pars$sar_uncrowded,
    sar_crowded = sar,
    beta = lookup_beta(calibrated_betas_df, default_pars$r0, sar, default_pars$crowding_fold_diff),
    eps = default_pars$eps,
    crowding_fold_diff = default_pars$crowding_fold_diff,
    max_hh_size = default_pars$max_hh_size,
    adjust_hhvars = default_pars$adjust_hhvars,
    init_prev = default_pars$init_prev
  )
}

# --- Crowding Fold Difference Sensitivity ---
# Skip fold_diff=2 since it's already in R0 sensitivity at baseline R0
# Beta is recalibrated for each fold_diff value to hold R0 constant
for (fold in fold_diff_values[fold_diff_values != default_pars$crowding_fold_diff]) {
  parset_counter <- parset_counter + 1
  pars_list[[parset_counter]] <- create_parset(
    sens_type = "fold",
    sens_value = fold,
    parset = parset_counter,
    gamma = default_pars$gamma,
    sar_uncrowded = default_pars$sar_uncrowded,
    sar_crowded = default_pars$sar_crowded,
    beta = lookup_beta(calibrated_betas_df, default_pars$r0, default_pars$sar_crowded, fold),
    eps = default_pars$eps,
    crowding_fold_diff = fold,
    max_hh_size = default_pars$max_hh_size,
    adjust_hhvars = default_pars$adjust_hhvars,
    init_prev = default_pars$init_prev
  )
}

# --- Gamma (Recovery Rate / Infectious Period) Sensitivity ---
# Skip baseline gamma (1/5) since it's already in R0 sensitivity at baseline R0
# Beta is recalibrated for each gamma value to hold R0 constant
for (gam in gamma_values[abs(gamma_values - default_pars$gamma) > 1e-10]) {
  parset_counter <- parset_counter + 1
  pars_list[[parset_counter]] <- create_parset(
    sens_type = "gamma",
    sens_value = gam,
    parset = parset_counter,
    gamma = gam,
    sar_uncrowded = default_pars$sar_uncrowded,
    sar_crowded = default_pars$sar_crowded,
    beta = lookup_beta(calibrated_betas_df, default_pars$r0,
                       default_pars$sar_crowded, default_pars$crowding_fold_diff, gam),
    eps = default_pars$eps,
    crowding_fold_diff = default_pars$crowding_fold_diff,
    max_hh_size = default_pars$max_hh_size,
    adjust_hhvars = default_pars$adjust_hhvars,
    init_prev = default_pars$init_prev
  )
}

# --- Seed Target (Initial Infection Source) Sensitivity ---
# Baseline seeds both subpopulations. Compare with seeding only A or only C.
# Numeric encoding: 1 = C only, 2 = Both (baseline), 3 = A only
# No recalibration needed — seeding doesn't affect beta calibration.
seed_targets <- c("C", "A")  # "both" is the baseline, already in R0 sensitivity
seed_codes <- c("C" = 1, "both" = 2, "A" = 3)
for (st in seed_targets) {
  parset_counter <- parset_counter + 1
  pars_list[[parset_counter]] <- create_parset(
    sens_type = "seed",
    sens_value = seed_codes[[st]],
    parset = parset_counter,
    gamma = default_pars$gamma,
    sar_uncrowded = default_pars$sar_uncrowded,
    sar_crowded = default_pars$sar_crowded,
    beta = calibrated_betas[as.character(default_pars$r0)],
    eps = default_pars$eps,
    crowding_fold_diff = default_pars$crowding_fold_diff,
    max_hh_size = default_pars$max_hh_size,
    adjust_hhvars = default_pars$adjust_hhvars,
    init_prev = default_pars$init_prev,
    seed_target = st
  )
}

# ==============================================================================
# Parameter Set Metadata Table
# ==============================================================================
# Create a summary table of all parameter sets for documentation and analysis

pars_metadata <- tibble::tibble(
  parset = sapply(pars_list, `[[`, "parset"),
  sens_type = sapply(pars_list, `[[`, "sens_type"),
  sens_value = sapply(pars_list, `[[`, "sens_value"),
  parset_name = sapply(pars_list, `[[`, "parset_name"),
  gamma = sapply(pars_list, `[[`, "gamma"),
  sar_uncrowded = sapply(pars_list, `[[`, "sar_uncrowded"),
  sar_crowded = sapply(pars_list, `[[`, "sar_crowded"),
  tau = sapply(pars_list, `[[`, "tau"),
  tau_boost = sapply(pars_list, `[[`, "tau_boost"),
  beta = sapply(pars_list, `[[`, "beta"),
  eps = sapply(pars_list, `[[`, "eps"),
  crowding_fold_diff = sapply(pars_list, `[[`, "crowding_fold_diff")
)

# ==============================================================================
# Baseline Parameter Set Helper
# ==============================================================================

#' Get the baseline parameter set (matching default_pars$r0)
#'
#' Returns the parameter set from pars_list that uses the baseline R0 value
#' defined in config.R. This ensures consistent baseline parameters across
#' all scripts (simulate.R, simulate_regional.R, etc.)
#'
#' @return Named list with baseline parameters
get_baseline_pars <- function() {

  baseline_name <- paste0("r0_", default_pars$r0)
  idx <- which(sapply(pars_list, `[[`, "parset_name") == baseline_name)


  if (length(idx) == 0) {
    stop("No parameter set found for baseline R0 = ", default_pars$r0,
         "\nAvailable parset names: ",
         paste(sapply(pars_list, `[[`, "parset_name"), collapse = ", "))
  }

  return(pars_list[[idx]])
}

# Store baseline for convenience
baseline_pars <- get_baseline_pars()

# ==============================================================================
# Print Summary
# ==============================================================================

cat("Generated", length(pars_list), "parameter sets:\n")
cat("  - R0 sensitivity:", sum(pars_metadata$sens_type == "r0"), "sets\n")
cat("  - Assortativity (eps) sensitivity:", sum(pars_metadata$sens_type == "eps"), "sets\n")
cat("  - SAR (crowded) sensitivity:", sum(pars_metadata$sens_type == "sar"), "sets\n")
cat("  - Crowding fold difference sensitivity:", sum(pars_metadata$sens_type == "fold"), "sets\n")
cat("  - Gamma (infectious period) sensitivity:", sum(pars_metadata$sens_type == "gamma"), "sets\n")
cat("  - Seed target sensitivity:", sum(pars_metadata$sens_type == "seed"), "sets\n")
cat("\nBaseline parameters (", baseline_pars$parset_name, "):\n", sep = "")
cat("  R0 = ", default_pars$r0, ", eps = ", default_pars$eps,
    ", SAR_crowded = ", default_pars$sar_crowded * 100, "%",
    ", crowding_fold_diff = ", default_pars$crowding_fold_diff, "\n", sep = "")
