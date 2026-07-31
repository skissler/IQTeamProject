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

baseline_parset_name <- paste0("r0_", default_pars$r0)
baseline_pars <- pars_list[[which(sapply(pars_list, function(p) p$parset_name) == baseline_parset_name)]]
if (is.null(baseline_pars)) stop(baseline_parset_name, " parset not found in pars_list. Run parameters.R first.")

# Derived vaccination multipliers
vax_mult_C   <- baseline_pars$vax_mult_C    # community: 1 - vax_eff * vax_cov_C
vax_mult_A   <- baseline_pars$vax_mult_A    # ag workers: 1 - vax_eff * vax_cov_A


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

# Save infection-based results (ground truth from epidemic model)
write_csv(decomp_diff, file.path(paths$output_dir, "decomposition_results_infections.csv"))
cat("Saved: output/decomposition_results_infections.csv\n")

# ==============================================================================
# 8-Scenario Decomposition: Add Obesity as Third Factor
# ==============================================================================
# Obesity (p_symp) is post-hoc: it shifts symptomatic case fractions without
# affecting transmission. To add it as a decomposition element, we cross the
# 4 epidemic scenarios with 2 p_symp conditions:
#
#   equalize_symp = TRUE:  both populations use p_symp_C (no obesity disparity)
#   equalize_symp = FALSE: A uses p_symp_A, C uses p_symp_C (observed disparity)
#
# This yields 8 "cases" scenarios from 4 epidemic runs (no new simulations needed).

cat("\nObesity (p_symp) contribution:\n")
cat("  p_symp_A =", round(p_symp_A, 4), "\n")
cat("  p_symp_C =", round(p_symp_C, 4), "\n")
cat("  Ratio p_symp_A / p_symp_C =", round(p_symp_A / p_symp_C, 4), "\n\n")

# 8-scenario ordering: null first, then single-factor, then two-factor, then full
scenario_8_levels <- c(
  "Null\n(all equal)",
  "Obesity\ndisparity only",
  "Vaccination\ndisparity only",
  "Household\nstructure only",
  "Vaccination\n+ Obesity",
  "HH structure\n+ Obesity",
  "HH + Vaccination\n(no obesity)",
  "Full model\n(all factors)"
)

decomp_cases <- bind_rows(
  mutate(decomp_diff, equalize_symp = TRUE),
  mutate(decomp_diff, equalize_symp = FALSE)
) %>%
  mutate(
    case_rate_A = case_when(
      equalize_symp ~ final_attack_rate_A * p_symp_C,
      TRUE          ~ final_attack_rate_A * p_symp_A
    ),
    case_rate_C    = final_attack_rate_C * p_symp_C,
    case_rate_diff  = case_rate_A - case_rate_C,
    case_rate_ratio = case_rate_A / case_rate_C,
    epidemic_scen   = as.character(scenario),
    scenario_8 = case_when(
      epidemic_scen == scenario_labels["null"]     & equalize_symp  ~ "Null\n(all equal)",
      epidemic_scen == scenario_labels["null"]     & !equalize_symp ~ "Obesity\ndisparity only",
      epidemic_scen == scenario_labels["hh_only"]  & equalize_symp  ~ "Household\nstructure only",
      epidemic_scen == scenario_labels["hh_only"]  & !equalize_symp ~ "HH structure\n+ Obesity",
      epidemic_scen == scenario_labels["vax_only"] & equalize_symp  ~ "Vaccination\ndisparity only",
      epidemic_scen == scenario_labels["vax_only"] & !equalize_symp ~ "Vaccination\n+ Obesity",
      epidemic_scen == scenario_labels["full"]     & equalize_symp  ~ "HH + Vaccination\n(no obesity)",
      epidemic_scen == scenario_labels["full"]     & !equalize_symp ~ "Full model\n(all factors)"
    ),
    scenario_8 = factor(scenario_8, levels = scenario_8_levels)
  )

write_csv(decomp_cases, file.path(paths$output_dir, "decomposition_results_cases.csv"))
cat("Saved: output/decomposition_results_cases.csv\n")

# ==============================================================================
# Print summary tables
# ==============================================================================

cat("\nMean A/C infection attack rate ratio by epidemic scenario (across regions):\n")
decomp_diff %>%
  group_by(scenario) %>%
  summarise(
    mean_ar_ratio = mean(attack_rate_ratio, na.rm = TRUE),
    mean_ar_diff  = mean(attack_rate_diff,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print()

cat("\nMean A/C symptomatic case rate ratio by 8-factor scenario (across regions):\n")
decomp_cases %>%
  group_by(scenario_8) %>%
  summarise(
    mean_case_ratio = mean(case_rate_ratio, na.rm = TRUE),
    mean_case_diff  = mean(case_rate_diff,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print()

# ==============================================================================
# Figures
# ==============================================================================

cb_palette    <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")
region_labels <- setNames(region_map$REGION_NAME, region_map$REGION6)

# --- 4-scenario infection figures (epidemic model output) ---

fig_diff <- decomp_diff %>%
  ggplot(aes(x = scenario, y = attack_rate_diff,
             color = factor(REGION6), group = REGION6)) +
  geom_line(alpha = 0.5, linewidth = 1) +
  geom_point(size = 2.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.6) +
  scale_color_manual(values = cb_palette, labels = region_labels) +
  labs(
    x     = NULL,
    y     = "Difference in Infection Attack Rate\n(Agricultural − Community)",
    color = "Region"
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "right", axis.text.x = element_text(size = 11))

fig_ratio <- decomp_diff %>%
  ggplot(aes(x = scenario, y = attack_rate_ratio,
             color = factor(REGION6), group = REGION6)) +
  geom_line(alpha = 0.5, linewidth = 1) +
  geom_point(size = 2.5) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50", alpha = 0.6) +
  scale_color_manual(values = cb_palette, labels = region_labels) +
  labs(
    x     = NULL,
    y     = "Infection Attack Rate Ratio\n(Agricultural / Community)",
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
cat("Saved: figures/decomposition_attack_rate_*.pdf/.png\n")

# --- 8-scenario symptomatic case figures ---

fig_cases_diff <- decomp_cases %>%
  ggplot(aes(x = scenario_8, y = case_rate_diff,
             color = factor(REGION6), group = REGION6)) +
  geom_line(alpha = 0.5, linewidth = 1) +
  geom_point(size = 2.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.6) +
  scale_color_manual(values = cb_palette, labels = region_labels) +
  labs(
    x     = NULL,
    y     = "Difference in Symptomatic Case Fraction\n(Agricultural − Community)",
    color = "Region"
  ) +
  theme_classic(base_size = 13) +
  theme(legend.position = "right",
        axis.text.x = element_text(size = 9, angle = 15, hjust = 1))

fig_cases_ratio <- decomp_cases %>%
  ggplot(aes(x = scenario_8, y = case_rate_ratio,
             color = factor(REGION6), group = REGION6)) +
  geom_line(alpha = 0.5, linewidth = 1) +
  geom_point(size = 2.5) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50", alpha = 0.6) +
  scale_color_manual(values = cb_palette, labels = region_labels) +
  labs(
    x     = NULL,
    y     = "Symptomatic Case Fraction Ratio\n(Agricultural / Community)",
    color = "Region"
  ) +
  theme_classic(base_size = 13) +
  theme(legend.position = "right",
        axis.text.x = element_text(size = 9, angle = 15, hjust = 1))

ggsave(file.path(paths$figures_dir, "decomposition_cases_diff.pdf"),
       fig_cases_diff,  width = 10, height = 5)
ggsave(file.path(paths$figures_dir, "decomposition_cases_diff.png"),
       fig_cases_diff,  width = 10, height = 5, dpi = 300)
ggsave(file.path(paths$figures_dir, "decomposition_cases_ratio.pdf"),
       fig_cases_ratio, width = 10, height = 5)
ggsave(file.path(paths$figures_dir, "decomposition_cases_ratio.png"),
       fig_cases_ratio, width = 10, height = 5, dpi = 300)
cat("Saved: figures/decomposition_cases_*.pdf/.png\n")

# ==============================================================================
# Word-ready HTML table: case rates by scenario and region
# ==============================================================================

region_name_map <- setNames(region_map$REGION_NAME, as.character(region_map$REGION6))

scenario_order_table <- c(
  "Null\n(all equal)",
  "Household\nstructure only",
  "Vaccination\ndisparity only",
  "Obesity\ndisparity only",
  "HH structure\n+ Obesity",
  "HH + Vaccination\n(no obesity)",
  "Vaccination\n+ Obesity",
  "Full model\n(all factors)"
)

clean_scen <- function(x) gsub("\n", " ", x)
THRESH <- 1e-6

full_ref_tbl <- decomp_cases %>%
  filter(grepl("Full model", scenario_8)) %>%
  select(REGION6, full_case_diff = case_rate_diff)

tbl <- decomp_cases %>%
  left_join(full_ref_tbl, by = "REGION6") %>%
  mutate(
    Region      = region_name_map[as.character(REGION6)],
    Scenario    = factor(clean_scen(as.character(scenario_8)),
                         levels = clean_scen(scenario_order_table)),
    diff_clean  = ifelse(abs(case_rate_diff) < THRESH, 0, case_rate_diff),
    pct_clean   = ifelse(abs(case_rate_diff) < THRESH, 0,
                         case_rate_diff / full_case_diff * 100),
    AR_A        = sprintf("%.1f%%", case_rate_A  * 100),
    AR_C        = sprintf("%.1f%%", case_rate_C  * 100),
    AR_diff     = sprintf("%.1f%%", diff_clean   * 100),
    Pct_full    = sprintf("%.0f%%", pct_clean)
  ) %>%
  arrange(Scenario, REGION6) %>%
  select(Scenario, Region, AR_A, AR_C, AR_diff, Pct_full)

hs  <- "background-color:#2C4770;color:white;font-weight:bold;padding:6px 10px;text-align:center;border:1px solid #888;"
cs  <- "padding:5px 10px;border:1px solid #ccc;text-align:center;"
fc  <- "padding:5px 10px;border:1px solid #ccc;text-align:left;"
sc  <- "padding:6px 10px;border:1px solid #ccc;text-align:left;vertical-align:middle;font-weight:bold;"

rows_html <- ""
row_alt   <- 0L
for (scen in levels(tbl$Scenario)) {
  scen_rows <- tbl[tbl$Scenario == scen, ]
  n         <- nrow(scen_rows)
  row_alt   <- row_alt + 1L
  bg        <- if (row_alt %% 2L == 0L) "#EFF2F9" else "#FFFFFF"
  for (i in seq_len(n)) {
    row      <- scen_rows[i, ]
    row_html <- "<tr>\n"
    if (i == 1L)
      row_html <- paste0(row_html, sprintf(
        '  <td rowspan="%d" style="%sbackground-color:%s;width:185px;">%s</td>\n',
        n, sc, bg, scen))
    row_html <- paste0(row_html,
      sprintf('  <td style="%sbackground-color:%s;">%s</td>\n', fc, bg, row$Region),
      sprintf('  <td style="%sbackground-color:%s;">%s</td>\n', cs, bg, row$AR_A),
      sprintf('  <td style="%sbackground-color:%s;">%s</td>\n', cs, bg, row$AR_C),
      sprintf('  <td style="%sbackground-color:%s;">%s</td>\n', cs, bg, row$AR_diff),
      sprintf('  <td style="%sbackground-color:%s;">%s</td>\n', cs, bg, row$Pct_full),
      "</tr>\n")
    rows_html <- paste0(rows_html, row_html)
  }
}

html_table <- paste0(
'<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<style>
  body  { font-family: Calibri, Arial, sans-serif; font-size: 11pt; margin: 30px; }
  table { border-collapse: collapse; width: 760px; }
  caption { font-weight: bold; font-size: 12pt; text-align: left; margin-bottom: 8px; }
  p.footnote { font-size: 9pt; color: #555; margin-top: 8px; width: 760px; }
</style>
</head>
<body>
<table>
<caption>Symptomatic case rates by decomposition scenario and NAWS region</caption>
<thead>
<tr>
  <th style="', hs, 'width:185px;">Scenario</th>
  <th style="', hs, 'width:110px;">Region</th>
  <th style="', hs, 'width:105px;">Symptomatic<br>Case Rate,<br>Ag Workers</th>
  <th style="', hs, 'width:105px;">Symptomatic<br>Case Rate,<br>Community</th>
  <th style="', hs, 'width:90px;">Difference</th>
  <th style="', hs, 'width:115px;">% of Full-Model<br>Disparity</th>
</tr>
</thead>
<tbody>
', rows_html, '
</tbody>
</table>
<p class="footnote">
Case rates are symptomatic fractions (infections &times; p<sub>symp</sub>) of the regional population.
&ldquo;% of Full-Model Disparity&rdquo; is the ag worker &minus; community difference
as a percentage of the Full Model (all factors) difference within each region.
Scenarios with &ldquo;no obesity&rdquo; apply a common p<sub>symp</sub> to both subpopulations;
all others apply subpopulation-specific p<sub>symp</sub>.
</p>
</body>
</html>')

out_path <- file.path(paths$output_dir, "decomposition_table.html")
writeLines(html_table, out_path)
cat("Saved: output/decomposition_table.html\n")
