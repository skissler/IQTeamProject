# County-Level Imputation of Agricultural Worker Household Characteristics

This project estimates county-level proportions of:
- **Crowded households**
- **Households with children under 18**

for the **agricultural workforce**, using:
- County-level ACS estimates for the **non-agricultural workforce**
- Regional NAWS estimates for the agricultural workforce
- Bayesian partial pooling with uncertainty propagation

## Structure

- `R/` contains modular scripts for pulling data, preparing inputs, fitting models, and extracting results
- `stan/` holds the Stan models (univariate and bivariate)
- `data/` contains raw and processed data
- `results/` stores posterior summaries and plots

## Requirements

- `rstan`, `tidycensus`, `dplyr`, `tidyr`, `readr`, `stringr`
- A Census API key (see `01_pull_acs_data.R`)

## Usage

Run scripts in order:

```r
source("R/01_pull_acs_data.R")
source("R/02_prep_stan_inputs.R")
source("R/03_run_model_univariate.R")   # or "04_run_model_bivariate.R"
source("R/05_extract_results.R")
```
