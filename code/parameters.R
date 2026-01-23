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

#' Calculate tau_boost for a target SAR in crowded households
#' @param sar_crowded Target SAR for crowded households (proportion, e.g., 0.40)
#' @param gamma Recovery rate
#' @param tau_base Baseline tau for uncrowded households
#' @return tau_boost value to add to tau_base for crowded households
calculate_tau_boost <- function(sar_crowded, gamma, tau_base) {
  # SAR = 1 - exp(-tau / gamma) => tau = -gamma * log(1 - SAR)
  tau_crowded <- -gamma * log(1 - sar_crowded)
  tau_boost <- tau_crowded - tau_base
  return(tau_boost)
}

#' Create a parameter set with descriptive naming
#' @param sens_type Character. Sensitivity dimension: "r0", "eps", "sar", "fold"
#' @param sens_value Numeric. The value being varied for this sensitivity
#' @param parset Numeric. Unique parameter set ID
#' @param gamma Recovery rate (1/infectious period)
#' @param tau_C Community baseline tau (uncrowded SAR)
#' @param tau_A Agricultural baseline tau (uncrowded SAR)
#' @param sar_crowded Target SAR for crowded households
#' @param beta_scalar Beta scalar for target R0 (calibrated values below)
#' @param eps Assortativity parameter
#' @param crowding_fold_diff Crowding fold difference
#' @return Named list with all parameters and metadata
create_parset <- function(sens_type, sens_value, parset,
                          gamma = 1/5,
                          tau_C = (1/4) * (1/5),  # Baseline: 20% SAR uncrowded
                          tau_A = (1/4) * (1/5),
                          sar_crowded = 0.40,
                          beta_scalar = 0.765,    # Default: R0 = 1.2
                          eps = 0.33,
                          crowding_fold_diff = 2,
                          max_hh_size = 7,
                          adjust_hhvars = TRUE,
                          init_prev = 0.001) {

  tau_boost <- calculate_tau_boost(sar_crowded, gamma, tau_C)

  list(
    # Metadata for tracking
    parset = parset,
    sens_type = sens_type,           # Sensitivity dimension
    sens_value = sens_value,         # Value being varied
    parset_name = paste0(sens_type, "_", sens_value),  # Descriptive name

    # Epidemiological parameters
    gamma = gamma,
    tau_C = tau_C,
    tau_A = tau_A,
    tau_boost = tau_boost,
    beta_C = beta_scalar * gamma,
    beta_A = beta_scalar * gamma,
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
beta_scalars_vec <- c(0.765, 1.05, 1.53, 2.52)
names(beta_scalars_vec) <- c(1.2, 1.5, 2.0, 3.0)

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
# Baseline Parameters
# ==============================================================================

baseline_gamma <- 1/5
baseline_tau_C <- (1/4) * (1/5)  # 20% SAR for uncrowded
baseline_tau_A <- (1/4) * (1/5)
baseline_sar_crowded <- 0.40     # 40% SAR for crowded
baseline_beta_scalar <- 0.765    # R0 = 1.2
baseline_eps <- 0.33
baseline_fold_diff <- 2

# ==============================================================================
# Sensitivity Parameter Values
# ==============================================================================

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
    beta_scalar = get_beta_scalar(r0),
    eps = baseline_eps,
    sar_crowded = baseline_sar_crowded,
    crowding_fold_diff = baseline_fold_diff
  )
}

# --- Assortativity (eps) Sensitivity ---
# Skip eps=0.33 since it's already in R0 sensitivity at baseline R0
for (eps in eps_values[eps_values != baseline_eps]) {
  parset_counter <- parset_counter + 1
  pars_list[[parset_counter]] <- create_parset(
    sens_type = "eps",
    sens_value = eps,
    parset = parset_counter,
    beta_scalar = baseline_beta_scalar,  # Baseline R0 = 1.2
    eps = eps,
    sar_crowded = baseline_sar_crowded,
    crowding_fold_diff = baseline_fold_diff
  )
}

# --- SAR in Crowded Households Sensitivity ---
# Skip 40% since it's already in R0 sensitivity at baseline R0
for (sar in sar_crowded_values[sar_crowded_values != baseline_sar_crowded]) {
  parset_counter <- parset_counter + 1
  pars_list[[parset_counter]] <- create_parset(
    sens_type = "sar",
    sens_value = sar,
    parset = parset_counter,
    beta_scalar = baseline_beta_scalar,
    eps = baseline_eps,
    sar_crowded = sar,
    crowding_fold_diff = baseline_fold_diff
  )
}

# --- Crowding Fold Difference Sensitivity ---
# Skip fold_diff=2 since it's already in R0 sensitivity at baseline R0
for (fold in fold_diff_values[fold_diff_values != baseline_fold_diff]) {
  parset_counter <- parset_counter + 1
  pars_list[[parset_counter]] <- create_parset(
    sens_type = "fold",
    sens_value = fold,
    parset = parset_counter,
    beta_scalar = baseline_beta_scalar,
    eps = baseline_eps,
    sar_crowded = baseline_sar_crowded,
    crowding_fold_diff = fold
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
  tau_C = sapply(pars_list, `[[`, "tau_C"),
  tau_boost = sapply(pars_list, `[[`, "tau_boost"),
  beta_C = sapply(pars_list, `[[`, "beta_C"),
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
