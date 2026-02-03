# ==============================================================================
# run_analysis.R - Main entry point for full analysis
# ==============================================================================
# Usage: source('code/run_analysis.R')
#
# This script runs the complete analysis pipeline for the influenza-agriculture
# impact paper. It orchestrates all steps from data import through simulation.
#
# Pipeline:
#   1. Setup and data import
#   2. Summary statistics
#   3. Model calibration
#   4. Define sensitivity parameter sets
#   5. Run regional simulations for all parameter sets
#   6. Sensitivity analysis and comparison figures
#   7. County-level simulations (with and without HH adjustment)
#   8. Crop calendar productivity analysis
# ==============================================================================

cat("\n", rep("=", 60), "\n", sep = "")
cat("Influenza-Agriculture Impact Analysis Pipeline\n")
cat(rep("=", 60), "\n\n", sep = "")

# ==============================================================================
# 1. Setup
# ==============================================================================

cat("Step 1: Loading dependencies and configuration...\n")
source('code/setup.R')

# ==============================================================================
# 2. Summary Statistics
# ==============================================================================

cat("\nStep 2: Generating summary statistics...\n")
source('code/summarystats.R')

# ==============================================================================
# 3. Model Calibration
# ==============================================================================

cat("\nStep 3: Running model calibration...\n")
source('code/calibrate_model.R')

# ==============================================================================
# 4. Define Parameter Sets
# ==============================================================================

cat("\nStep 4: Defining sensitivity parameter sets...\n")
source('code/parameters.R')

# ==============================================================================
# 5. Run Regional Simulations
# ==============================================================================

cat("\nStep 5: Running regional simulations for all parameter sets...\n")
source('code/run_regional_batch.R')

# ==============================================================================
# 6. Sensitivity Analysis
# ==============================================================================

cat("\nStep 6: Running sensitivity analysis...\n")
source('code/sensitivity_analysis.R')

# ==============================================================================
# 7. County-Level Simulations
# ==============================================================================
# Runs county-level simulations with baseline parameters, using three different
# approaches to derive county-level agricultural worker household distributions:
#
# - "none":           Use regional NAWS data directly (no county variation)
# - "multiplicative": Multiply NAWS by (county_ACS / regional_ACS_mean)
# - "additive":       Add (county_ACS - regional_ACS_mean) to NAWS

cat("\nStep 7: Running county-level simulations...\n")

# Use baseline parameters (from get_baseline_pars() in parameters.R)
pars <- baseline_pars

# Run with no adjustment (use regional NAWS directly)
cat("  [1/3] Running with adjust_hhvars = 'none'...\n")
pars$adjust_hhvars <- "none"
paths$county_output <- "output/epidf_indiv_county_none.csv"
source('code/simulate.R')

# Run with multiplicative adjustment (baseline behavior)
cat("  [2/3] Running with adjust_hhvars = 'multiplicative'...\n")
pars$adjust_hhvars <- "multiplicative"
paths$county_output <- "output/epidf_indiv_county_multiplicative.csv"
source('code/simulate.R')

# Run with additive adjustment
cat("  [3/3] Running with adjust_hhvars = 'additive'...\n")
pars$adjust_hhvars <- "additive"
paths$county_output <- "output/epidf_indiv_county_additive.csv"
source('code/simulate.R')

cat("  County-level simulations complete.\n")

# ==============================================================================
# 8. Crop Calendar Analysis
# ==============================================================================

cat("\nStep 8: Running crop calendar productivity analysis...\n")
source('code/crop_calendars.R')

# ==============================================================================
# 9. Main Publication Figures
# ==============================================================================

cat("\nStep 9: Generating main publication figures...\n")
source('code/plot_main_figures.R')

# ==============================================================================
# Summary
# ==============================================================================

cat("\n", rep("=", 60), "\n", sep = "")
cat("Analysis Pipeline Complete\n")
cat(rep("=", 60), "\n\n", sep = "")

cat("Output files saved to:\n")
cat("  - Regional simulations:", paths$output_dir, "\n")
cat("  - Figures:", paths$figures_dir, "\n")
cat("\nSensitivity summary files:\n")
cat("  - output/sensitivity_summary.csv\n")
cat("  - output/sensitivity_differential.csv\n")
cat("\nCounty-level simulation files:\n")
cat("  - output/epidf_indiv_county_none.csv (no adjustment)\n")
cat("  - output/epidf_indiv_county_multiplicative.csv (multiplicative adjustment)\n")
cat("  - output/epidf_indiv_county_additive.csv (additive adjustment)\n")
