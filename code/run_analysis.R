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
#   7. Crop calendar productivity analysis
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
cat("  Total parameter sets:", length(pars_list), "\n\n")

# Track timing
start_time <- Sys.time()

for (i in seq_along(pars_list)) {
  pars <- pars_list[[i]]
  cat("  [", i, "/", length(pars_list), "] Running: ", pars$parset_name, "\n", sep = "")
  source('code/simulate_regional.R')
}

elapsed <- difftime(Sys.time(), start_time, units = "mins")
cat("\n  Completed all simulations in", round(elapsed, 1), "minutes\n")

# ==============================================================================
# 6. Sensitivity Analysis
# ==============================================================================

cat("\nStep 6: Running sensitivity analysis...\n")
source('code/sensitivity_analysis.R')

# ==============================================================================
# 7. Crop Calendar Analysis
# ==============================================================================

cat("\nStep 7: Running crop calendar productivity analysis...\n")
source('code/crop_calendars.R')

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
