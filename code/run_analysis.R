# ==============================================================================
# run_analysis.R - Main entry point for full analysis
# ==============================================================================
# Usage: source('code/run_analysis.R')
#
# This script runs the complete analysis pipeline for the influenza-agriculture
# impact paper. It orchestrates all steps from data import through simulation.
# ==============================================================================

# Load all dependencies and validate environment
source('code/setup.R')

# Generate summary statistics and associated figures:
source('code/summarystats.R')

# Run the model calibration code:
source('code/calibrate_model.R')

# Define the default and sensitivity parameter values:
source('code/parameters.R')

# Run the county-level simulation only for the default parameters:
# pars <- pars_list[[1]]
# source('code/simulate.R')
# source('code/plot_model_output.R')

# Run the sensitivity analysis for all parameter sets:
for (pars in pars_list) {
	source('code/simulate_regional.R')
}

