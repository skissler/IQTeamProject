# ==============================================================================
# decomposition_analysis.R - Factor decomposition of the A-C epidemic disparity
# ==============================================================================
# Decomposes the observed difference in epidemic outcomes between agricultural
# workers (A) and the general community (C) into contributions from:
#
#   1. Household structure  (size distribution and crowding, from NAWS vs ACS)
#   2. Vaccination coverage (vax_cov_A = 0.40 vs vax_cov_C = 0.50)
#   3. Comorbidities        (symptomatic fraction only; post-hoc, not in epidemic model)
#
# Four epidemic scenarios are run (one-at-a-time design):
#   "null"     - Null model: all factors equal (community values for both A and C)
#   "hh_only"  - Household structure differs; vaccination equal (community rate for both)
#   "vax_only" - Vaccination differs; household structure equal (community dist. for both)
#   "full"     - All factors differ (replicates the baseline r0_1.5 run)
#
# Comorbidities are not in the epidemic model (they shift p_symp post-hoc only),
# so their contribution to CROP LOSSES is reported analytically via p_symp_A.
# Their contribution to EPIDEMIC DISPARITY is zero by construction.
#
# Prerequisites: setup.R sourced; calibrated_betas available; NAWS and ACS data loaded
# Output:
#   output/decomposition_results.csv
#   figures/decomposition_attack_rate_diff.pdf/.png
#   figures/decomposition_attack_rate_ratio.pdf/.png
# ==============================================================================

if (!exists("paths"))            source("code/setup.R")
if (!exists("naws_data"))        source("code/import_naws.R")
if (!exists("acs_data_regional")) source("code/import_acs.R")
if (!exists("calibrated_betas")) source("code/calibrate_model.R")
if (!exists("pars_list"))        source("code/parameters.R")

library(odin)

cat("\n", rep("=", 60), "\n", sep = "")
cat("Decomposition Analysis\n")
cat(rep("=", 60), "\n\n", sep = "")

# ==============================================================================
# Baseline parameter set (r0 = 1.5, all other defaults)
# ==============================================================================
# Pull the r0_1.5 parset from pars_list to guarantee consistency with main runs

baseline_pars <- pars_list[[which(sapply(pars_list, function(p) p$parset_name) == "r0_1.5")]]
if (is.null(baseline_pars)) stop("r0_1.5 parset not found in pars_list. Run parameters.R first.")

# Derived vaccination multipliers
vax_mult_C   <- baseline_pars$vax_mult_C    # community: 1 - vax_eff * vax_cov_C
vax_mult_A   <- baseline_pars$vax_mult_A    # ag workers: 1 - vax_eff * vax_cov_A
vax_mult_equal <- vax_mult_C                # use community rate when equalizing

cat("Baseline parameters:\n")
cat("  R0 =", default_pars$r0, "| gamma =", baseline_pars$gamma,
    "| tau =", round(baseline_pars$tau, 4),
    "| tau_boost =", round(baseline_pars$tau_boost, 4), "\n")
cat("  vax_mult_C =", round(vax_mult_C, 3),
    "| vax_mult_A =", round(vax_mult_A, 3), "\n\n")

# ==============================================================================
# Scenario definitions
# ==============================================================================
# equalize_hh:  if TRUE, agricultural workers get the community (ACS) HH distribution
# equalize_vax: if TRUE, agricultural workers get the community vaccination multiplier

scenarios <- list(
  null     = list(label = "Null\n(all equal)",            equalize_hh = TRUE,  equalize_vax = TRUE),
  hh_only  = list(label = "Household\nstructure only",    equalize_hh = FALSE, equalize_vax = TRUE),
  vax_only = list(label = "Vaccination\ndisparity only",  equalize_hh = TRUE,  equalize_vax = FALSE),
  full     = list(label = "Full model\n(all factors)",    equalize_hh = FALSE, equalize_vax = FALSE)
)

# ==============================================================================
# Simulation function: run one scenario for one region
# ==============================================================================

run_decomp_region <- function(region, pars, equalize_hh, equalize_vax,
                               household_states, acs_data_regional, naws_data) {

  n_states <- nrow(household_states)

  # Community household distribution (always ACS)
  ic_joiner_C <- acs_data_regional %>%
    filter(REGION6 == region) %>%
    make_ic_joiner(fold_diff = pars$crowding_fold_diff)

  # Agricultural worker household distribution
  ic_joiner_A_base <- if (equalize_hh) {
    ic_joiner_C   # community distribution for both populations
  } else {
    naws_data %>%
      filter(REGION6 == region) %>%
      make_ic_joiner(fold_diff = pars$crowding_fold_diff)
  }

  # Vaccination multiplier for ag workers
  vax_mult_A_scen <- if (equalize_vax) vax_mult_C else vax_mult_A

  # Seed initial infected into each household distribution
  seed_ic <- function(ic_joiner, init_prev) {
    ic_inf <- ic_joiner %>%
      mutate(frac = init_prev * frac * hh_size, y = y + 1, x = x - 1)
    bind_rows(
      mutate(ic_joiner, frac = frac - ic_inf$frac),
      ic_inf
    )
  }

  ic_joiner_C_seeded <- seed_ic(ic_joiner_C,      pars$init_prev)
  ic_joiner_A_seeded <- seed_ic(ic_joiner_A_base, pars$init_prev)

  # Map to odin initial condition vectors
  init_C <- household_states %>%
    left_join(ic_joiner_C_seeded, by = c("x", "y", "z", "hh_size", "crowded")) %>%
    arrange(state_index) %>% replace_na(list(frac = 0)) %>% pull(frac)

  init_A <- household_states %>%
    left_join(ic_joiner_A_seeded, by = c("x", "y", "z", "hh_size", "crowded")) %>%
    arrange(state_index) %>% replace_na(list(frac = 0)) %>% pull(frac)

  # Population sizes
  pop_cty <- acs_data_regional %>% filter(REGION6 == region) %>% pull(population)  %>% first()
  prop_ag <- acs_data_regional %>% filter(REGION6 == region) %>% pull(prop_ag_workers) %>% first()
  pop_C   <- pop_cty * (1 - prop_ag)
  pop_A   <- pop_cty * prop_ag

  # Run model
  mod <- household_model_twopop_crowding$new(
    n_states   = n_states,
    x          = household_states$x,
    y          = household_states$y,
    z          = household_states$z,
    hh_size    = household_states$hh_size,
    crowded    = household_states$crowded,
    rec_index  = household_states$rec_index,
    inf_index  = household_states$inf_index,
    init_C     = init_C,
    init_A     = init_A,
    gamma      = pars$gamma,
    tau        = pars$tau,
    tau_boost  = pars$tau_boost,
    beta       = pars$beta,
    eps        = pars$eps,
    pop_C      = pop_C,
    pop_A      = pop_A,
    vax_mult_C = vax_mult_C,
    vax_mult_A = vax_mult_A_scen
  )

  times <- seq(0, sim_settings$t_max, by = sim_settings$t_step)
  out   <- as_tibble(data.frame(mod$run(times)))
  format_output_indiv(out, household_states) %>% mutate(REGION6 = region)
}

# ==============================================================================
# Precompute household state table (shared across all runs)
# ==============================================================================

household_states <- generate_household_state_table(
  n_min = 1, n_max = default_pars$max_hh_size, crowding = TRUE
)
n_states <- nrow(household_states)

# ==============================================================================
# Run all scenarios
# ==============================================================================

all_results <- list()

for (scenario_name in names(scenarios)) {
  scen <- scenarios[[scenario_name]]
  cat("Running scenario:", scenario_name,
      "(equalize_hh =", scen$equalize_hh,
      ", equalize_vax =", scen$equalize_vax, ")\n")

  scen_data <- bind_rows(lapply(seq_len(n_regions), function(region) {
    run_decomp_region(
      region            = region,
      pars              = baseline_pars,
      equalize_hh       = scen$equalize_hh,
      equalize_vax      = scen$equalize_vax,
      household_states  = household_states,
      acs_data_regional = acs_data_regional,
      naws_data         = naws_data
    )
  })) %>%
    mutate(scenario = scenario_name)

  all_results[[scenario_name]] <- scen_data
  cat("  Done.\n")
}

decomp_data <- bind_rows(all_results)

# ==============================================================================
# Summary statistics and A/C differentials
# ==============================================================================

scenario_levels <- names(scenarios)
scenario_labels <- sapply(scenarios, `[[`, "label")

decomp_summary <- decomp_data %>%
  group_by(scenario, REGION6, subpop) %>%
  summarise(
    peak_prevalence   = max(I_indiv, na.rm = TRUE),
    time_to_peak      = t[which.max(I_indiv)],
    final_attack_rate = last(R_indiv),
    .groups = "drop"
  )

decomp_diff <- decomp_summary %>%
  select(scenario, REGION6, subpop, peak_prevalence, final_attack_rate, time_to_peak) %>%
  pivot_wider(
    names_from  = subpop,
    values_from = c(peak_prevalence, final_attack_rate, time_to_peak)
  ) %>%
  mutate(
    attack_rate_diff      = final_attack_rate_A - final_attack_rate_C,
    attack_rate_ratio     = final_attack_rate_A / final_attack_rate_C,
    peak_prevalence_ratio = peak_prevalence_A   / peak_prevalence_C,
    time_to_peak_diff     = time_to_peak_A      - time_to_peak_C,
    scenario = factor(scenario, levels = scenario_levels, labels = scenario_labels)
  )

# Obesity contribution (post-hoc, crop losses only):
# The difference in p_symp_A between "full" (0.515) and "null" (0.500) represents
# the obesity factor's contribution; since crop losses scale linearly with p_symp_A,
# the obesity contribution scales crop losses by p_symp_A_full / p_symp_A_equal.
p_symp_A_full  <- compute_p_symp(comorbidity_pars$obs_A, comorbidity_pars$or_symp_obesity)
p_symp_C_equal <- 0.50
obesity_crop_multiplier <- p_symp_A_full / p_symp_C_equal
cat("\nObesity contribution to crop losses (p_symp_A multiplier):\n")
cat("  p_symp_A (full)  =", round(p_symp_A_full, 4), "\n")
cat("  p_symp_A (equal) =", round(p_symp_C_equal, 4), "\n")
cat("  Crop loss ratio (full vs. equal) =", round(obesity_crop_multiplier, 4), "\n\n")

# Save results
write_csv(decomp_diff, file.path(paths$output_dir, "decomposition_results.csv"))
cat("Saved: output/decomposition_results.csv\n")

# ==============================================================================
# Print summary table
# ==============================================================================

cat("\nMean A/C attack rate ratio by scenario (across regions):\n")
decomp_diff %>%
  group_by(scenario) %>%
  summarise(
    mean_ar_ratio  = mean(attack_rate_ratio, na.rm = TRUE),
    mean_ar_diff   = mean(attack_rate_diff,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print()

# ==============================================================================
# Figures
# ==============================================================================

cb_palette    <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")
region_labels <- setNames(region_map$REGION_NAME, region_map$REGION6)

# Attack rate difference (A - C)
fig_diff <- decomp_diff %>%
  ggplot(aes(x = scenario, y = attack_rate_diff,
             color = factor(REGION6), group = REGION6)) +
  geom_line(alpha = 0.5, linewidth = 1) +
  geom_point(size = 2.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.6) +
  scale_color_manual(values = cb_palette, labels = region_labels) +
  labs(
    x     = NULL,
    y     = "Difference in Final Attack Rate\n(Agricultural − Community)",
    color = "Region"
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "right", axis.text.x = element_text(size = 11))

# Attack rate ratio (A / C)
fig_ratio <- decomp_diff %>%
  ggplot(aes(x = scenario, y = attack_rate_ratio,
             color = factor(REGION6), group = REGION6)) +
  geom_line(alpha = 0.5, linewidth = 1) +
  geom_point(size = 2.5) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50", alpha = 0.6) +
  scale_color_manual(values = cb_palette, labels = region_labels) +
  labs(
    x     = NULL,
    y     = "Final Attack Rate Ratio\n(Agricultural / Community)",
    color = "Region"
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "right", axis.text.x = element_text(size = 11))

ggsave(file.path(paths$figures_dir, "decomposition_attack_rate_diff.pdf"),
       fig_diff,  width = 8, height = 5)
ggsave(file.path(paths$figures_dir, "decomposition_attack_rate_diff.png"),
       fig_diff,  width = 8, height = 5, dpi = 300)
ggsave(file.path(paths$figures_dir, "decomposition_attack_rate_ratio.pdf"),
       fig_ratio, width = 8, height = 5)
ggsave(file.path(paths$figures_dir, "decomposition_attack_rate_ratio.png"),
       fig_ratio, width = 8, height = 5, dpi = 300)
cat("Saved: figures/decomposition_*.pdf/.png\n")
