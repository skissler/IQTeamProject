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
  movements_lettuce = "data/movements_lettuce.csv",
  movements_strawberries = "data/movements_strawberries.csv",
  movements_oranges = "data/movements_oranges.csv",

  # Output directories
  output_dir = "output",
  figures_dir = "figures",

  # Primary output files
  county_output = "output/county_sim.csv",
  regional_output_prefix = "output/regional_sim_",

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

  # Basic reproduction number (R0)
  # beta is calibrated directly by calibrate_model.R for each target R0
  r0 = 1.5,                     # Baseline R0 value

  # Household secondary attack rates (SAR)
  # Primary parameters: SAR values for uncrowded and crowded households
  # Derived parameters (tau, tau_boost) computed in parameters.R using:
  #   tau = SAR * gamma / (1 - SAR)
  sar_uncrowded = 0.20,         # Baseline SAR for uncrowded households: 20%
  sar_crowded = 0.40,           # SAR for crowded households: 40%

  # Population mixing
  eps = 1/3,                    # Mixing parameter (eta = 1 - eps for assortativity)
                                # eps = 0: complete assortment, eps = 1: proportional mixing

  # Household structure
  max_hh_size = 7,              # Maximum household size modeled (7+ grouped together)
  crowding_fold_diff = 2,       # Crowding transmission multiplier for large households

  # Simulation settings
  # adjust_hhvars controls how county-level ag worker HH distributions are derived:
  #   "none"           - Use regional NAWS data directly (no county variation)
  #   "multiplicative" - Multiply NAWS by (county_ACS / regional_ACS_mean)
  #   "additive"       - Add (county_ACS - regional_ACS_mean) to NAWS
  adjust_hhvars = "additive",
  init_prev = 0.001,            # Initial prevalence (0.1% of population infected)
  seed_target = "both",         # Which subpop to seed: "both", "A", or "C"

  # Vaccination parameters
  # vax_mult = 1 - vax_eff * vax_cov is passed to the odin model.
  # Baseline values reflect observed influenza vaccination rates and typical effectiveness.
  vax_eff   = 0.60,             # Vaccine efficacy against infection (~60% in a good flu year)
  vax_cov_C = 0.50,             # Community vaccination coverage (~50% US influenza uptake)
  vax_cov_A = 0.40              # Agricultural worker vaccination coverage (~40% observed)
)

# ==============================================================================
# Comorbidity Parameters
# ==============================================================================
# Parameterise the effect of obesity on the symptomatic fraction for each
# subpopulation. p_symp is derived separately for C and A by back-solving for
# the non-obese baseline probability p0 that anchors the community at p_symp = 0.5.

comorbidity_pars <- list(
  or_symp_obesity = 1.5,  # OR of obesity -> symptomatic disease (conditional on infection)
  obs_C = 0.40,           # Community obesity prevalence (fixed; no sensitivity on this)
  obs_A = 0.55            # Agricultural worker obesity prevalence (baseline)
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
  progress_interval = 20,       # Print progress every N counties

  # Epidemic establishment threshold (prevalence fraction)
  establishment_threshold = 0.001  # 0.1% = 1 per 1,000; used for time_to_1pct and epidemic_duration
)

# ==============================================================================
# Sensitivity Analysis Values
# ==============================================================================
# One-at-a-time sensitivity ranges for each parameter dimension.
# This is the single source of truth — parameters.R reads from here.

sensitivity_values <- list(
  r0      = c(1.2, 1.5, 2.0, 3.0),
  eps     = c(1/4, 1/3, 1/2, 2/3, 3/4, 1),
  sar     = c(0.20, 0.30, 0.40, 0.50, 0.60),
  fold    = c(1, 2, 3),
  gamma   = c(1/3, 1/5, 1/10),
  vax_A   = c(0.2, 0.4, 0.6, 0.8),   # Ag worker coverage;  baseline = 0.4
  vax_C   = c(0.3, 0.4, 0.5, 0.6),   # Community coverage;  baseline = 0.5
  vax_eff = c(0.2, 0.4, 0.6, 0.8)    # Vaccine efficacy;    baseline = 0.6
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
n_regions <- nrow(region_map)
