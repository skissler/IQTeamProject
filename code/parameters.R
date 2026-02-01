# ==============================================================================
# parameters.R - Sensitivity Analysis Parameter Sets
# ==============================================================================
# Defines all parameter combinations for the sensitivity analyses.
#
# Sensitivity dimensions (one-at-a-time analysis):
#   1. R0 values: 1.2 (baseline), 1.5, 2.0, 3.0
#   2. Assortativity (eps): 0, 0.33 (baseline), 0.5, 0.7
#   3. SAR in crowded households: 30%, 40% (baseline), 50%, 60%
#   4. Crowding fold difference: 1, 2 (baseline), 3
#
# Baseline parameters: R0=1.2, eps=0.33, SAR_crowded=40%, crowding_fold_diff=2
# ==============================================================================

# ==============================================================================
# Helper Functions
# ==============================================================================

#' Calculate tau from SAR
#'
#' Derives the within-household transmission rate (tau) needed to achieve a
#' target secondary attack rate (SAR).
#'
#' @param sar Target secondary attack rate (proportion, e.g., 0.20)
#' @param gamma Recovery rate
#' @return tau value (within-household transmission rate)
#'
#' @details
#' In the House & Keeling household model with exponentially distributed
#' infectious periods, the SAR for a 2-person household is derived from
#' competing exponentials (infection at rate tau vs recovery at rate gamma):
#'
#'   SAR = tau / (tau + gamma)
#'
#' Solving for tau:
#'   tau = SAR * gamma / (1 - SAR)
calculate_tau <- function(sar, gamma) {
  sar * gamma / (1 - sar)
}

#' Calculate tau_boost for a target SAR in crowded households
#'
#' Derives the additional transmission rate needed to boost from the uncrowded
#' SAR to the crowded SAR.
#'
#' @param sar_crowded Target SAR for crowded households (proportion, e.g., 0.40)
#' @param gamma Recovery rate
#' @param tau Baseline tau for uncrowded households
#' @return tau_boost value to add to tau for crowded households
calculate_tau_boost <- function(sar_crowded, gamma, tau) {
  tau_crowded <- calculate_tau(sar_crowded, gamma)
  tau_boost <- tau_crowded - tau
  return(tau_boost)
}

#' Create a parameter set with descriptive naming
#' @param sens_type Character. Sensitivity dimension: "r0", "eps", "sar", "fold"
#' @param sens_value Numeric. The value being varied for this sensitivity
#' @param parset Numeric. Unique parameter set ID
#' @param gamma Recovery rate (1/infectious period)
#' @param sar_uncrowded SAR for uncrowded households (used to compute tau)
#' @param sar_crowded Target SAR for crowded households (used to compute tau_boost)
#' @param beta_scalar Beta scalar for target R0 (calibrated values below)
#' @param eps Assortativity parameter
#' @param crowding_fold_diff Crowding fold difference
#' @param max_hh_size Maximum household size modeled
#' @param adjust_hhvars How to adjust HH vars: "none", "multiplicative", or "additive"
#' @param init_prev Initial prevalence
#' @return Named list with all parameters and metadata
create_parset <- function(sens_type, sens_value, parset,
                          gamma, sar_uncrowded, sar_crowded,
                          beta_scalar, eps, crowding_fold_diff,
                          max_hh_size, adjust_hhvars, init_prev) {

  # Compute tau and tau_boost from SAR values
  tau <- calculate_tau(sar_uncrowded, gamma)
  tau_boost <- calculate_tau_boost(sar_crowded, gamma, tau)

  list(
    # Metadata for tracking
    parset = parset,
    sens_type = sens_type,           # Sensitivity dimension
    sens_value = sens_value,         # Value being varied
    parset_name = paste0(sens_type, "_", sens_value),  # Descriptive name

    # Epidemiological parameters
    gamma = gamma,
    sar_uncrowded = sar_uncrowded,   # Primary parameter
    sar_crowded = sar_crowded,       # Primary parameter
    tau = tau,                       # Derived from sar_uncrowded
    tau_boost = tau_boost,           # Derived from sar_crowded - sar_uncrowded
    beta = beta_scalar * gamma,
    eps = eps,

    # Household structure
    max_hh_size = max_hh_size,
    crowding_fold_diff = crowding_fold_diff,

    # Simulation settings
    adjust_hhvars = adjust_hhvars,
    init_prev = init_prev
  )
}

# ==============================================================================
# Calibrated Beta Scalars
# ==============================================================================
# These values are calibrated at the national level to achieve target R0 values
# (see calibrate_model.R for methodology)

# Use a named vector with numeric names for reliable lookup
# Note: as.character(2.0) returns "2" not "2.0", so we use a lookup function
# beta_scalars_vec <- c(0.765, 1.05, 1.53, 2.52)
# names(beta_scalars_vec) <- c(1.2, 1.5, 2.0, 3.0)
beta_scalars_vec <- c(
  `1.2` = 0.7649,  # R0 = 1.2
  `1.5` = 1.0490,  # R0 = 1.5
  `2` = 1.5344,  # R0 = 2
  `3` = 2.5270  # R0 = 3
)

#' Get calibrated beta scalar for a target R0
#' @param r0 Target reproduction number
#' @return Beta scalar value
get_beta_scalar <- function(r0) {

  idx <- which(abs(as.numeric(names(beta_scalars_vec)) - r0) < 0.01)
  if (length(idx) == 0) {
    stop("No calibrated beta scalar for R0 = ", r0)
  }
  return(beta_scalars_vec[idx])
}

# ==============================================================================
# Sensitivity Parameter Values
# ==============================================================================
# Baseline values are defined in config.R (default_pars)

r0_values <- c(1.2, 1.5, 2.0, 3.0)
eps_values <- c(0, 0.33, 0.5, 0.7)
sar_crowded_values <- c(0.30, 0.40, 0.50, 0.60)
fold_diff_values <- c(1, 2, 3)

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
    beta_scalar = get_beta_scalar(r0),
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
    beta_scalar = default_pars$beta_scalar,
    eps = eps,
    crowding_fold_diff = default_pars$crowding_fold_diff,
    max_hh_size = default_pars$max_hh_size,
    adjust_hhvars = default_pars$adjust_hhvars,
    init_prev = default_pars$init_prev
  )
}

# --- SAR in Crowded Households Sensitivity ---
# Skip 40% since it's already in R0 sensitivity at baseline R0
for (sar in sar_crowded_values[sar_crowded_values != default_pars$sar_crowded]) {
  parset_counter <- parset_counter + 1
  pars_list[[parset_counter]] <- create_parset(
    sens_type = "sar",
    sens_value = sar,
    parset = parset_counter,
    gamma = default_pars$gamma,
    sar_uncrowded = default_pars$sar_uncrowded,
    sar_crowded = sar,
    beta_scalar = default_pars$beta_scalar,
    eps = default_pars$eps,
    crowding_fold_diff = default_pars$crowding_fold_diff,
    max_hh_size = default_pars$max_hh_size,
    adjust_hhvars = default_pars$adjust_hhvars,
    init_prev = default_pars$init_prev
  )
}

# --- Crowding Fold Difference Sensitivity ---
# Skip fold_diff=2 since it's already in R0 sensitivity at baseline R0
for (fold in fold_diff_values[fold_diff_values != default_pars$crowding_fold_diff]) {
  parset_counter <- parset_counter + 1
  pars_list[[parset_counter]] <- create_parset(
    sens_type = "fold",
    sens_value = fold,
    parset = parset_counter,
    gamma = default_pars$gamma,
    sar_uncrowded = default_pars$sar_uncrowded,
    sar_crowded = default_pars$sar_crowded,
    beta_scalar = default_pars$beta_scalar,
    eps = default_pars$eps,
    crowding_fold_diff = fold,
    max_hh_size = default_pars$max_hh_size,
    adjust_hhvars = default_pars$adjust_hhvars,
    init_prev = default_pars$init_prev
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
# Print Summary
# ==============================================================================

cat("Generated", length(pars_list), "parameter sets:\n")
cat("  - R0 sensitivity:", sum(pars_metadata$sens_type == "r0"), "sets\n")
cat("  - Assortativity (eps) sensitivity:", sum(pars_metadata$sens_type == "eps"), "sets\n")
cat("  - SAR (crowded) sensitivity:", sum(pars_metadata$sens_type == "sar"), "sets\n")
cat("  - Crowding fold difference sensitivity:", sum(pars_metadata$sens_type == "fold"), "sets\n")
cat("\nBaseline parameters (parset 1, r0_1.2):\n")
cat("  R0 = 1.2, eps = 0.33, SAR_crowded = 40%, crowding_fold_diff = 2\n")
