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

This orchestrates: `setup.R` → `summarystats.R` → `calibrate_model.R` → `parameters.R` → `simulate_regional.R` (×4 parameter sets)

**Run individual components:**
```r
source('code/setup.R')           # Load all dependencies and config (do this first)
source('code/calibrate_model.R') # National-level calibration only
source('code/simulate.R')        # County-level simulation (default params)
source('code/simulate_regional.R') # Regional simulation (requires pars object)
```

## Architecture

### Configuration (`code/config.R`)
Central configuration file containing all paths, parameters, and settings:
- `paths` - File paths for input data and output directories
- `data_settings` - ACS/Census year settings, NAWS date range
- `default_pars` - Baseline epidemiological parameters (gamma, tau, beta, eps)
- `sim_settings` - Simulation options (time steps, parallelization)
- `region_map` - NAWS region name/abbreviation mapping

### Setup Files
- **`setup.R`** - Consolidated dependency loader; source this once at session start
- **`setup_check.R`** - Environment validation; run `check_setup()` to verify prerequisites
- **`utils_documented.R`** - Helper functions with roxygen documentation

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
- **`calibrate_model.R`** - Tunes beta parameters at national level to achieve target R0 values
- **`simulate.R`** / **`simulate_regional.R`** - Runs epidemic simulations across counties/regions

### Parameter Sets (`code/parameters.R`)
Four parameter configurations for sensitivity analysis varying R0: 1.2 (default), 1.5, 2.0, 3.0. Key parameters:
- `gamma` - Recovery rate (1/5 = 5-day infectious period)
- `tau_C`, `tau_A` - Household secondary attack rates (community/agricultural)
- `beta_C`, `beta_A` - Community transmission rates
- `eps` - Assortativity between populations (0.33)

### Output Files
- `output/epidf_indiv_full.csv` - County-level daily trajectories (~46 MB)
- `output/epidf_indiv_full_regional_*.csv` - Regional sensitivity runs
- `figures/` - Generated visualizations (PDF/PNG)

## Key Conventions

- All scripts assume working directory is project root
- Source `setup.R` at session start to load all dependencies and configuration
- All configurable values (paths, parameters, years) are in `config.R`
- Simulations use `future.apply::future_lapply()` for parallelization
- Regional mapping defined in `config.R` (`region_map` tibble)
- Maximum household size capped at 7; crowding fold-difference = 2
- Archived/deprecated code is in `code/_archived/` (not used in current analysis)
