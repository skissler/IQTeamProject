# Modeling the impact of respiratory disease outbreaks on the United States agricultural workforce

Katherine Bardsley, Luis X. de Pablo, Emma Keppler Canada, Naia Ormaza Zulueta, Zia Mehrabi, Stephen M. Kissler

## Overview

Agricultural workers live in larger, more crowded households than the general U.S. population, amplifying their potential exposure to respiratory pathogens. This repository contains code and data for a household-structured susceptible-infectious-recovered (SIR) transmission model that compares disease dynamics between agricultural workers and the general population across six U.S. regions, and assesses downstream productivity losses for labor-intensive crops.

Key findings:
- Peak disease prevalence among agricultural workers is 23-45% higher than in the general population, with outbreaks peaking 5-12 days earlier.
- At the point of maximum divergence, prevalence among agricultural workers is 74-178% higher than in the general community.
- For three labor-intensive California crops (strawberries, iceberg lettuce, oranges), worst-case productivity losses range from 0.50-0.62%, translating to $4-22 million in lost revenue per crop under baseline assumptions.

An interactive scenario modeling tool is available at: [https://kisslerlab.shinyapps.io/ag-epi-model/](https://kisslerlab.shinyapps.io/ag-epi-model/)

## Prerequisites

- **R 4.5.0** with [renv](https://rstudio.github.io/renv/) for dependency management
- **Census API key** set in `~/.Renviron` as `CENSUS_API_KEY` (obtain one at https://api.census.gov/data/key_signup.html)

## Setup

```r
# Install dependencies
renv::restore()

# Validate environment
source('code/setup_check.R')
check_setup()
```

## Running the analysis

The full analysis pipeline is orchestrated by `code/run_analysis.R`:

```r
source('code/run_analysis.R')
```

This runs the following steps in order:

| Step | Script | Description |
|------|--------|-------------|
| 1 | `setup.R` | Load packages, configuration, helper functions, and model definitions |
| 2 | `summarystats.R` | Generate descriptive statistics and household characteristic figures |
| 3 | `calibrate_model.R` | Calibrate between-household transmission rate (beta) for target R0 values |
| 4 | `parameters.R` | Define parameter sets for one-at-a-time sensitivity analysis |
| 5 | `run_regional_batch.R` | Run regional simulations for all parameter sets |
| 6 | `sensitivity_analysis.R` | Compute summary statistics and generate comparison figures |
| 7 | `simulate.R` | Run county-level simulations under three imputation methods |
| 8 | `crop_calendars.R` | Assess crop-specific productivity losses in California |
| 9 | `plot_main_figures.R` | Generate main publication figures |

All scripts assume the working directory is the project root. Source `code/setup.R` first if running individual scripts interactively.

## Repository structure

```
.
├── code/
│   ├── run_analysis.R           # Main entry point
│   ├── config.R                 # Central configuration (paths, parameters, settings)
│   ├── setup.R                  # Dependency loader (source this first)
│   ├── setup_check.R            # Environment validation
│   ├── utils.R                  # Helper functions
│   ├── epimodels.R              # Household-structured SIR model (odin)
│   ├── import_acs.R             # Download/process ACS county-level data
│   ├── import_naws.R            # Process NAWS agricultural worker data
│   ├── summarystats.R           # Descriptive statistics and figures
│   ├── calibrate_model.R        # Beta calibration via bisection search
│   ├── parameters.R             # Sensitivity analysis parameter sets
│   ├── run_regional_batch.R     # Regional simulations (all parameter sets)
│   ├── simulate.R               # County-level simulations
│   ├── sensitivity_analysis.R   # Sensitivity analysis and comparison figures
│   ├── crop_calendars.R         # Crop productivity impact analysis
│   └── plot_main_figures.R      # Publication figures
├── data/
│   ├── stateregion.csv          # State-to-NAWS-region mapping
│   ├── stateabbrev.csv          # State abbreviations
│   ├── movements_lettuce.csv    # USDA crop shipment data (iceberg lettuce)
│   ├── movements_strawberries.csv
│   └── movements_oranges.csv
├── app/                         # Interactive Shiny scenario modeling tool
├── output/                      # Generated simulation results and summary tables
├── figures/                     # Generated figures
└── renv/                        # R dependency management
```

Note: `data/naws_all.sas7bdat` (National Agricultural Workers Survey microdata) is required but excluded from the repository due to the file size. They can be obtained from https://www.dol.gov/agencies/eta/national-agricultural-workers-survey/data/files-sas.

## Data sources

- **American Community Survey (ACS)**: County-level household size, crowding, and agricultural employment data (2018-2022 5-year estimates). Downloaded via the Census API by `import_acs.R`.
- **National Agricultural Workers Survey (NAWS)**: Regional household size and crowding distributions for agricultural workers (2018-2022). Processed by `import_naws.R`.
- **USDA Agricultural Marketing Service**: Weekly point-to-point crop shipment volumes for California (2018-2024). Used to approximate seasonal harvest patterns.

## Model

The analysis uses a deterministic household-structured SIR model ([House & Keeling, 2009](https://doi.org/10.1017/S0950268808001416)) extended to two populations (agricultural workers and general community) with assortative mixing and household crowding effects. The model tracks within- and between-household transmission, where within-household transmission rates are elevated in crowded households. Transmission parameters are calibrated so that the simulated final epidemic size matches theoretical predictions for a given basic reproduction number (R0).

Sensitivity analyses vary five epidemiological parameters one at a time: R0 (1.2-3.0), assortativity (0-0.75), secondary attack rate in crowded households (20-60%), crowding fold difference (1-3), and infectious period (3-10 days).

## Citation

Bardsley K, de Pablo LX, Keppler Canada E, Ormaza Zulueta N, Mehrabi Z, Kissler SM. Modeling the impact of respiratory disease outbreaks on the United States agricultural workforce. *Submitted*.
