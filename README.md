# Assessing the impact of an influenza pandemic on agricultural workers and food production in the United States

*Katie Bardsley, Luis X. de Pablo, Emma Keppler, Naia Ormaza Zulueta, Zia Mehrabi, Stephen Kissler*


## Files

- `run_analysis.R`: This is the main entry point for the analysis. It sources the other key scripts in a specific order to execute the full workflow: first `calibrate_model.R`, then `simulate.R`, and finally `plot_model_output.R`.
- `epimodels.R`: This script defines the mathematical framework for the epidemiological models. It uses the odin package to create four models: a `basic_model` (a simple SIR model), a `household_model`, and more complex versions, `household_model_twopop` and `household_model_twopop_crowding`, which account for different populations and household crowding.
- `import_acs.R`: This script connects to the U.S. Census Bureau's American Community Survey (ACS) API to download and process county-level data, including household size and crowding information. It requires a Census API key to run.
- `import_naws.R`: This script reads data from a SAS file (`naws_all.sas7bdat`), which contains information from the National Agricultural Workers Survey (NAWS). It processes this data to derive regional-level summaries on household sizes and crowding for agricultural workers.
- `calibrate_model.R`: This script uses the data from `import_acs.R` and `import_naws.R` to calibrate the `household_model_twopop_crowding` model at a national level before running the county-level simulations.
- `simulate.R`: This is the primary simulation script. It iterates through each county in the U.S., fetches county-specific data, and runs the `household_model_twopop_crowding` model to simulate disease spread for two distinct populations (community/non-agricultural workers and agricultural workers). The results are saved to a CSV file named `epidf_indiv_full.csv`.
- `simulate_regional.R`: This script serves as an alternative to `simulate.R`, running the simulation at a regional level instead of the county level.
- `plot_model_output.R`: This script reads the simulation output from the `epidf_indiv_full.csv` file. It then generates various plots and maps to visualize the simulation results, such as infection curves over time and geographic maps of infection differences.
- `utils.R`: This file contains a collection of helper functions used by the other scripts, including functions for processing spatial data and formatting model output.
- `household_model.R`: This is a separate test or example script for the models defined in `epimodels.R` and is not part of the main run_analysis.R workflow.