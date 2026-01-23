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

  # Household secondary attack rates (SAR)
  # tau represents within-household transmission rate
  tau_C = (1/4) * (1/5),        # Community baseline SAR: 20%
  tau_A = (1/4) * (1/5),        # Agricultural worker baseline SAR: 20%
  tau_boost = (2/3) * (1/5) - (1/4) * (1/5),  # SAR boost for crowded households: to 40%

  # Community transmission (beta determines R0)
  # beta_C = 0.765 * gamma gives R0 ~ 1.2 (baseline)
  # beta_C = 1.05 * gamma gives R0 ~ 1.5
  # beta_C = 1.53 * gamma gives R0 ~ 2.0
  # beta_C = 2.52 * gamma gives R0 ~ 3.0
  beta_C = 0.765 * (1/5),       # Community transmission rate (R0 ~ 1.2)
  beta_A = 0.765 * (1/5),       # Agricultural worker transmission rate

  # Population mixing
  eps = 0.33,                   # Assortativity: probability of within-group contact
                                # 0 = random mixing, 1 = complete assortment

  # Household structure
  max_hh_size = 7,              # Maximum household size modeled (7+ grouped together)
  crowding_fold_diff = 2,       # Crowding transmission multiplier for large households

  # Simulation settings
  adjust_hhvars = TRUE,         # Adjust county household vars using regional NAWS data
  init_prev = 0.001             # Initial prevalence (0.1% of population infected)
)

# ==============================================================================
# Simulation Settings
# ==============================================================================

sim_settings <- list(
  # Time parameters (days)
  t_max = 100,                  # Maximum simulation time for county runs
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
