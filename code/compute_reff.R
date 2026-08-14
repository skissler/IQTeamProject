# ==============================================================================
# compute_reff.R - Effective reproduction number under baseline vaccination
# ==============================================================================
# Purpose:
#   Reports the effective reproduction number (R_eff) implied by each R0
#   scenario once baseline vaccination is applied. R_eff is recovered from the
#   simulated community INFECTION final size via the SIR final-size relation
#   R_eff = -ln(1 - R_inf) / R_inf (see compute_reff() in utils.R) -- the same
#   operational definition used to calibrate R0 in calibrate_model.R.
#
#   The general community (~98% of the population, mixing largely within itself)
#   is the natural analogue of a "general population under community-level
#   vaccination", so its R_eff is the value comparable to published pandemic /
#   seasonal influenza reproduction-number estimates.
#
# Inputs:  output/regional_sim_r0_*.csv (raw infection trajectories)
# Outputs: output/reff_table.csv (r0, subpop, region, final_size, reff)
#          printed R0 -> R_eff summary
#
# Usage:   Rscript code/compute_reff.R   or   source('code/compute_reff.R')
# ==============================================================================

if (!exists("paths")) source('code/setup.R')

out_dir <- paths$output_dir

# --- Locate the R0-sensitivity regional simulations --------------------------
r0_files <- list.files(out_dir, pattern = "^regional_sim_r0_[0-9.]+\\.csv$",
                       full.names = TRUE)
if (length(r0_files) == 0) {
  stop("No regional_sim_r0_*.csv files found in ", out_dir,
       ". Run the regional simulations first.")
}

# --- Extract infection final size and R_eff per (R0, region, subpop) ---------
# Uses the RAW infection final size (last R_indiv), not the symptomatic-case
# rate, because R_eff describes transmission rather than symptom development.
reff_long <- purrr::map_dfr(r0_files, function(f) {
  read_csv(f, show_col_types = FALSE) %>%
    group_by(parset_name, sens_value, REGION6, subpop) %>%
    summarise(final_size = R_indiv[which.max(t)], .groups = "drop") %>%
    rename(r0 = sens_value)
}) %>%
  left_join(region_map, by = "REGION6") %>%
  mutate(reff = compute_reff(final_size)) %>%
  arrange(r0, subpop, REGION6)

write_csv(
  reff_long %>% select(r0, subpop, REGION6, region = REGION_NAME, final_size, reff),
  file.path(out_dir, "reff_table.csv")
)

# --- Summarise across regions -------------------------------------------------
reff_summary <- reff_long %>%
  group_by(r0, subpop) %>%
  summarise(reff_mean = mean(reff),
            reff_min  = min(reff),
            reff_max  = max(reff),
            .groups = "drop") %>%
  arrange(subpop, r0)

pop_label <- c(C = "General community", A = "Agricultural workers")

cat("\n========================================================\n")
cat("Effective reproduction number under baseline vaccination\n")
cat("(R_eff from community/ag infection final size; VE = ",
    default_pars$vax_eff, ", VC_C = ", default_pars$vax_cov_C,
    ", VC_A = ", default_pars$vax_cov_A, ")\n", sep = "")
cat("========================================================\n")
cat(sprintf("%-22s %-6s %-14s %s\n", "Population", "R0", "R_eff (mean)", "R_eff (range across regions)"))
cat(strrep("-", 70), "\n")
for (i in seq_len(nrow(reff_summary))) {
  row <- reff_summary[i, ]
  cat(sprintf("%-22s %-6.1f %-14.2f %.2f-%.2f\n",
              pop_label[row$subpop], row$r0, row$reff_mean, row$reff_min, row$reff_max))
}

# --- Baseline headline (community R_eff at the baseline R0) -------------------
baseline_reff <- reff_summary %>%
  filter(subpop == "C", abs(r0 - default_pars$r0) < 1e-6)
if (nrow(baseline_reff) == 1) {
  cat(sprintf("\nBaseline (R0 = %.1f): community R_eff = %.2f (%.2f-%.2f across regions).\n",
              default_pars$r0, baseline_reff$reff_mean,
              baseline_reff$reff_min, baseline_reff$reff_max))
  cat(sprintf("Simple approximation R0*(1 - VC_C*VE) = %.2f.\n",
              default_pars$r0 * (1 - default_pars$vax_cov_C * default_pars$vax_eff)))
}
cat("Saved:", file.path(out_dir, "reff_table.csv"), "\n")
