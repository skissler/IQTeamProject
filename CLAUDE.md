# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is an epidemiological modeling project in R that assesses the impact of an influenza pandemic on agricultural workers and food production in the United States. It combines household-structured compartmental disease models with spatial and demographic data to simulate disease spread and agricultural impact.

## Running the Analysis

**Prerequisites:**
- R 4.5.0 with renv for dependency management
- Census API key set in `~/.Renviron` as `CENSUS_API_KEY`

**First-time setup:**
```r
renv::restore()                  # Install dependencies
source('code/setup_check.R')     # Validate environment
check_setup()                    # Will report any missing components
```

**Run full analysis from R (working directory must be project root):**
```r
source('code/run_analysis.R')
```

This orchestrates: `setup.R` → `summarystats.R` → `calibrate_model.R` → `parameters.R` → `simulate_regional.R` (×12 parameter sets) → `sensitivity_analysis.R` → `crop_calendars.R`

**Run individual components:**
```r
source('code/setup.R')           # Load all dependencies and config (do this first)
source('code/calibrate_model.R') # National-level calibration only
source('code/simulate.R')        # County-level simulation (default params)
source('code/simulate_regional.R') # Regional simulation (requires pars object)
source('code/sensitivity_analysis.R') # Compare results across sensitivity dimensions
```

## Architecture

### Configuration (`code/config.R`)
Central configuration file containing all paths, parameters, and settings:
- `paths` - File paths for input data and output directories
- `data_settings` - ACS/Census year settings, NAWS date range
- `default_pars` - Baseline epidemiological parameters (gamma, sar_uncrowded, sar_crowded, r0, eps)
- `sim_settings` - Simulation options (time steps, parallelization)
- `region_map` - NAWS region name/abbreviation mapping

### Setup Files
- **`setup.R`** - Consolidated dependency loader; source this once at session start
- **`setup_check.R`** - Environment validation; run `check_setup()` to verify prerequisites
- **`utils.R`** - Helper functions with roxygen documentation

### Disease Models (`code/epimodels.R`)
Uses the `odin` package to define four progressively complex compartmental models:
- `basic_model` - Simple SIR
- `household_model` - Household-stratified SIR
- `household_model_twopop` - Two populations (community + agricultural workers)
- `household_model_twopop_crowding` - Adds household crowding effects (primary model used)

The models implement House & Keeling (2009) household-structured transmission with states compartmentalized by (susceptible, infected, recovered, household size, crowding status).

### Data Pipeline
- **`import_acs.R`** - Downloads county-level ACS data via tidycensus API (household size, crowding)
- **`import_naws.R`** - Processes National Agricultural Workers Survey (SAS file at `data/naws_all.sas7bdat`)
- **`calibrate_model.R`** - Calibrates beta directly for each target R0; outputs `calibrated_betas` named vector
- **`simulate.R`** / **`simulate_regional.R`** - Runs epidemic simulations across counties/regions

### Parameter Sets (`code/parameters.R`)
Defines 14 parameter configurations for one-at-a-time sensitivity analysis across four dimensions:

**Sensitivity Dimensions (baseline values in bold):**
1. **R0 values:** 1.2, 1.5 **(baseline)**, 2.0, 3.0
2. **Assortativity (η):** 0, 0.25, 0.33, 0.5, 0.67 **(baseline)**, 0.75, 1
3. **SAR in crowded households:** 20%, 30%, 40% **(baseline)**, 50%, 60%
4. **Crowding fold difference:** 1, 2 **(baseline)**, 3

**Key Parameters:**

*Primary SAR parameters (defined in config.R):*
- `sar_uncrowded` - Baseline secondary attack rate for uncrowded households (20%)
- `sar_crowded` - Secondary attack rate for crowded households (40%)

*Derived transmission parameters (computed in parameters.R using helpers from utils.R):*
- `tau` - Within-household transmission rate, derived from `sar_uncrowded` using: `tau = SAR * gamma / (1 - SAR)`
- `tau_boost` - Additional transmission rate for crowded households, computed as the difference between tau_crowded and tau
- `beta` - Between-household transmission rate, calibrated directly by `calibrate_model.R` and passed to `parameters.R` via `calibrated_betas` (a named vector mapping R0 → beta)

*Other epidemiological parameters:*
- `gamma` - Recovery rate (1/5 = 5-day infectious period)
- `eps` - Assortativity between populations
- `crowding_fold_diff` - How much more likely large households are to be crowded

**Parameter Set Naming Convention:**
- `r0_1.2`, `r0_1.5`, etc. - R0 sensitivity runs
- `eps_0`, `eps_0.5`, etc. - Assortativity sensitivity runs
- `sar_0.3`, `sar_0.5`, etc. - Crowded household SAR sensitivity runs
- `fold_1`, `fold_3` - Crowding fold difference sensitivity runs

### Sensitivity Analysis (`code/sensitivity_analysis.R`)
Loads all regional simulation outputs and generates comparative summaries:
- Calculates summary statistics (peak prevalence, attack rates, timing)
- Computes differential metrics (agricultural workers vs community)
- Creates comparison figures across sensitivity dimensions

### Output Files
- `output/county_sim.csv` - County-level daily trajectories (~46 MB)
- `output/regional_sim_{parset_name}.csv` - Regional sensitivity runs (e.g., `regional_sim_r0_1.2.csv`)
- `output/sensitivity_summary.csv` - Summary statistics for all parameter sets
- `output/sensitivity_differential.csv` - Differential metrics (A vs C) for all parameter sets
- `figures/sensitivity_*.pdf` - Comparison visualizations

## Key Conventions

- All scripts assume working directory is project root
- Source `setup.R` at session start to load all dependencies and configuration
- All configurable values (paths, parameters, years) are in `config.R`
- Simulations use `future.apply::future_lapply()` for parallelization
- Regional mapping defined in `config.R` (`region_map` tibble)
- Maximum household size capped at 7; baseline crowding fold-difference = 2
- Output files include parameter metadata (`parset`, `parset_name`, `sens_type`, `sens_value`)
- Archived/deprecated code is in `code/_archived/` (not used in current analysis)
