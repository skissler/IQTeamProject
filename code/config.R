# code/config.R
# Centralized configuration for the influenza-agriculture impact analysis
# All configurable paths, parameters, and settings in one place

# ==============================================================================
# File Paths
# ==============================================================================

paths <- list(
  # Input data
  naws_data = "data/naws_all.sas7bdat",
  state_region = "data/stateregion.csv",
  state_abbrev = "data/stateabbrev.csv",
  movements_data = "data/movements.csv",

  # Output directories
  output_dir = "output",
  figures_dir = "figures",

  # Primary output files
  county_output = "output/epidf_indiv_full.csv",
  regional_output_prefix = "output/epidf_indiv_full_regional_",

  # Sensitivity analysis outputs
  sensitivity_summary = "output/sensitivity_summary.csv",
  sensitivity_differential = "output/sensitivity_differential.csv"
)

# ==============================================================================
# Calibrated R0 to Beta Scalar Mapping
# ==============================================================================
# These values are calibrated at the national level to achieve target R0 values
# (see calibrate_model.R for methodology)

beta_scalars_vec <- c(
  `1.2` = 0.7649,
  `1.5` = 1.0490,
  `2` = 1.5344,
  `3` = 2.5270
)

#' Get calibrated beta scalar for a target R0
#' @param r0 Target reproduction number
#' @return Beta scalar value
get_beta_scalar <- function(r0) {
  idx <- which(abs(as.numeric(names(beta_scalars_vec)) - r0) < 0.01)
  if (length(idx) == 0) {
    stop("No calibrated beta scalar for R0 = ", r0,
         "\nAvailable R0 values: ", paste(names(beta_scalars_vec), collapse = ", "))
  }
  return(beta_scalars_vec[idx])
}

# ==============================================================================
# Data Settings
# ==============================================================================

data_settings <- list(
  # American Community Survey settings
  acs_year = 2022,              # ACS data year (uses 5-year estimates ending this year)
  decennial_year = 2020,        # Decennial census year for urban/rural data

  # NAWS filtering
  naws_start_year = 2018,       # Start year for NAWS data filtering
  naws_end_year = 2022          # End year for NAWS data filtering
)

# ==============================================================================
# Model Parameters (Defaults)
# ==============================================================================
# These are the baseline epidemiological parameters. For sensitivity analysis,
# see parameters.R which defines multiple parameter sets with varying R0 values.

default_pars <- list(
  # Disease dynamics
  gamma = 1/5,                  # Recovery rate: 1/5 = 5-day infectious period

  # Basic reproduction number (R0)
  # beta_scalar is derived from r0 using get_beta_scalar()
  r0 = 1.5,                     # Baseline R0 value

  # Household secondary attack rates (SAR)
  # Primary parameters: SAR values for uncrowded and crowded households
  # Derived parameters (tau, tau_boost) computed in parameters.R using:
  #   tau = SAR * gamma / (1 - SAR)
  sar_uncrowded = 0.20,         # Baseline SAR for uncrowded households: 20%
  sar_crowded = 0.40,           # SAR for crowded households: 40%

  # Population mixing
  eps = 0.33,                   # Assortativity: probability of within-group contact
                                # 0 = random mixing, 1 = complete assortment

  # Household structure
  max_hh_size = 7,              # Maximum household size modeled (7+ grouped together)
  crowding_fold_diff = 2,       # Crowding transmission multiplier for large households

  # Simulation settings
  # adjust_hhvars controls how county-level ag worker HH distributions are derived:
  #   "none"           - Use regional NAWS data directly (no county variation)
  #   "multiplicative" - Multiply NAWS by (county_ACS / regional_ACS_mean)
  #   "additive"       - Add (county_ACS - regional_ACS_mean) to NAWS
  adjust_hhvars = "multiplicative",
  init_prev = 0.001             # Initial prevalence (0.1% of population infected)
)

# ==============================================================================
# Simulation Settings
# ==============================================================================

sim_settings <- list(
  # Time parameters (days)
  t_max = 365,                  # Maximum simulation time (full year)
  t_max_calibration = 1000,     # Maximum time for calibration runs
  t_step = 1,                   # Time step size

  # Parallelization
  use_parallel = TRUE,          # Use parallel processing for county simulations
  progress_interval = 20        # Print progress every N counties
)

# ==============================================================================
# Geographic Settings
# ==============================================================================

# States excluded from analysis (non-contiguous US)
excluded_states <- c(
  "02",  # Alaska
  "15",  # Hawaii
  "72",  # Puerto Rico
  "60",  # American Samoa
  "66",  # Guam
  "69",  # Northern Mariana Islands
  "78"   # US Virgin Islands
)

# NAWS region mapping
region_map <- tibble::tibble(
  REGION6 = 1:6,
  REGION_NAME = c("East", "Southeast", "Midwest", "Southwest", "Northwest", "California"),
  REGION_ABBREV = c("EA", "SE", "MW", "SW", "NW", "CA")
)
