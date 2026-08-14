# ==============================================================================
# generate_si_tables.R - Regenerate Supplementary Tables S3, S4, S6 as HTML
# ==============================================================================
# Purpose:
#   Rebuilds the formatted SI tables from the pipeline's CSV outputs so they
#   stay in sync with the analysis. Previously these tables (S3, S4, and the
#   S6 crop grid) had no generator in the repo and were maintained by hand.
#
#   - Table S3: per-region peak prevalence, time to peak, and final size for
#     agricultural workers (A) and the general community (C), by parameter set.
#     Source: output/sensitivity_summary.csv
#   - Table S4: per-region A-vs-C differential metrics (peak prevalence ratio,
#     final size ratio, peak timing difference, maximum relative prevalence).
#     Source: output/sensitivity_differential.csv
#   - Table S6: crop production losses across R0 and symptomatic-fraction
#     scenarios. Source: output/crop_impact_r0_comparison.csv
#
# Usage:
#   Rscript code/generate_si_tables.R      # standalone (reads/writes output/)
#   source('code/generate_si_tables.R')    # or after run_analysis.R
#
# Outputs: output/table_s3.html, output/table_s4.html, output/table_s6.html
#
# Notes:
#   - Rows are keyed by (sens_type, sens_value), NOT parset_name, because the
#     obesity dimensions reuse the baseline transmission parset (r0_2).
#   - Values are rendered directly from the CSV columns (infection-based peak
#     prevalence and final size), matching the submission-2 presentation.
# ==============================================================================

suppressMessages(library(tidyverse))

out_dir <- if (exists("paths")) paths$output_dir else "output"

# Baseline dimension values (used to identify and label the baseline row and to
# drop each dimension's baseline value, which is already shown as "Baseline").
BASE <- list(r0 = 2.0, eps = 2/3, sar = 0.4, fold = 2, gamma = 1/5,
             vax_A = 0.4, vax_C = 0.5, vax_eff = 0.6, obesity_or = 1.5, obesity_obs_A = 0.55)

region_names <- c("East", "Southeast", "Midwest", "Southwest", "Northwest", "California")

# ------------------------------------------------------------------------------
# Parameter-set block ordering and labels
# ------------------------------------------------------------------------------
# Each block is one (sens_type, sens_value) combination rendered as a rowspan
# group of six regions. Order mirrors the manuscript. The obesity dimensions
# intentionally retain their baseline value (they reuse the baseline epidemic
# and differ only in post-hoc symptomatic scaling).

pct <- function(v) paste0(formatC(v * 100, format = "f", digits = 0), "%")

# Assortativity is reported in the manuscript as eta = 1 - eps, where the
# stored sens_value is eps. All other dimensions are labeled by sens_value.
block_label <- function(sens_type, sens_value) {
  switch(sens_type,
    r0      = paste0("R₀ = ", formatC(sens_value, format = "fg")),
    eps     = paste0("η = ", formatC(round(1 - sens_value, 2), format = "fg")),
    sar     = paste0("Crowded SAR = ", pct(sens_value)),
    fold    = paste0("d = ", sens_value),
    gamma   = paste0("γ = 1/", round(1 / sens_value)),
    vax_A   = paste0("Ag. vax. coverage = ", pct(sens_value)),
    vax_C   = paste0("Comm. vax. coverage = ", pct(sens_value)),
    vax_eff = paste0("Vaccine efficacy = ", pct(sens_value)),
    obesity_or    = paste0("Obesity OR = ", formatC(sens_value, format = "fg")),
    obesity_obs_A = paste0("Ag. obesity prev. = ", pct(sens_value)),
    seed    = paste0("Seed target = ", sens_value),
    paste0(sens_type, " = ", sens_value)
  )
}

# Dimension display order. Blocks are discovered from the data (not hardcoded),
# so the table always reflects the parameter sets actually present.
DIM_ORDER   <- c("r0", "eps", "sar", "fold", "gamma",
                 "vax_A", "vax_C", "vax_eff", "obesity_or", "obesity_obs_A")
# Ascending sens_value within every dimension. For assortativity this yields
# descending eta (eta = 1 - eps), matching the manuscript's high-eta-first order.
# gamma is sorted descending so the infectious period 1/gamma reads short-to-long
# (1/3 before 1/10), matching the manuscript.
DIM_DESC    <- c("gamma")
# Seed is a categorical dimension and is not tabulated in S3/S4 (matches the
# manuscript); add "seed" here to include it.
DIM_EXCLUDE <- c("seed")

# Build the ordered block spec by reading the distinct (sens_type, sens_value)
# combinations from a data frame. The baseline (sens_type "r0", baseline R0) is
# emitted first and its value is dropped from the R0 block.
build_block_spec <- function(df) {
  combos <- df %>%
    distinct(sens_type, sens_value) %>%
    filter(!sens_type %in% DIM_EXCLUDE)

  rows <- list(list(sens_type = "r0", sens_value = BASE$r0, label = "Baseline"))
  for (dim in DIM_ORDER) {
    vals <- combos %>% filter(sens_type == dim) %>% pull(sens_value)
    if (dim == "r0") vals <- vals[abs(vals - BASE$r0) > 1e-6]   # baseline shown separately
    vals <- sort(unique(vals), decreasing = dim %in% DIM_DESC)
    for (v in vals) {
      rows[[length(rows) + 1]] <- list(sens_type = dim, sens_value = v,
                                       label = block_label(dim, v))
    }
  }
  bind_rows(lapply(rows, as_tibble))
}

# Match a block's rows in a data frame, tolerating floating-point sens_value.
match_block <- function(df, sens_type, sens_value) {
  df %>% filter(.data$sens_type == !!sens_type,
                abs(.data$sens_value - !!sens_value) < 1e-6)
}

# ------------------------------------------------------------------------------
# HTML helpers
# ------------------------------------------------------------------------------
TH  <- 'padding:5px 8px;border-bottom:2px solid #333;border-top:2px solid #333;white-space:nowrap;'
TD  <- 'padding:4px 8px;border-bottom:1px solid #ccc;white-space:nowrap;'
TDL <- paste0(TD, 'text-align:left;')
TDR <- paste0(TD, 'text-align:right;font-variant-numeric:tabular-nums;')
TDG <- paste0('padding:4px 8px;border-bottom:1px solid #ccc;border-top:1px solid #999;',
              'text-align:left;vertical-align:top;font-weight:bold;white-space:nowrap;')

html_head <- function(caption) {
  paste0(
    '<!DOCTYPE html><html><head><meta charset="UTF-8">\n<style>\n',
    '  body { font-family: Arial, sans-serif; font-size: 10pt; margin:20px; color:#111; }\n',
    '  table { border-collapse:collapse; width:auto; }\n',
    '  caption { font-weight:bold; font-size:11pt; text-align:left; margin-bottom:8px; max-width:900px; }\n',
    '</style></head><body>\n<table>\n<caption>', caption, '</caption>\n')
}
html_foot <- '</table>\n</body></html>\n'

# Render a full table given the block spec, a per-block data getter, and a
# function that emits the <td> cells (after the region cell) for one region row.
render_table <- function(spec, header_html, get_block, cell_fn, caption, path) {
  body <- ""
  for (i in seq_len(nrow(spec))) {
    blk <- get_block(spec$sens_type[i], spec$sens_value[i])
    if (nrow(blk) == 0) next
    blk <- blk %>% arrange(REGION6)
    for (r in seq_len(nrow(blk))) {
      row <- blk[r, ]
      region_cell <- sprintf('<td style="%s">%s</td>', TDR %>% sub("text-align:right", "text-align:left", .),
                             region_names[row$REGION6])
      if (r == 1) {
        grp <- sprintf('<td rowspan="%d" style="%s">%s</td>', nrow(blk), TDG, spec$label[i])
        body <- paste0(body, "<tr>", grp, region_cell, cell_fn(row), "</tr>\n")
      } else {
        body <- paste0(body, "<tr>", region_cell, cell_fn(row), "</tr>\n")
      }
    }
  }
  writeLines(paste0(html_head(caption), header_html, "<tbody>\n", body, "</tbody>\n", html_foot), path)
  cat("Saved:", path, "\n")
}

fmt_pct1 <- function(x) if (!is.finite(x)) "&ndash;" else paste0(formatC(x * 100, format = "f", digits = 1), "%")
fmt_num  <- function(x, d = 2) if (!is.finite(x)) "&ndash;" else formatC(x, format = "f", digits = d)
fmt_int  <- function(x) if (!is.finite(x)) "&ndash;" else formatC(as.integer(round(x)), format = "d")

# ------------------------------------------------------------------------------
# Table S3 - Summary statistics
# ------------------------------------------------------------------------------
build_s3 <- function() {
  s <- read_csv(file.path(out_dir, "sensitivity_summary.csv"), show_col_types = FALSE)
  wide <- s %>%
    select(sens_type, sens_value, REGION6, subpop, peak_prevalence, time_to_peak, final_attack_rate) %>%
    pivot_wider(names_from = subpop,
                values_from = c(peak_prevalence, time_to_peak, final_attack_rate))

  header <- paste0(
    '<thead>\n<tr>',
    sprintf('<th rowspan="2" style="%stext-align:left;">Parameter set</th>', TH),
    sprintf('<th rowspan="2" style="%stext-align:left;">Region</th>', TH),
    sprintf('<th colspan="2" style="%stext-align:center;">Peak prevalence</th>', TH),
    sprintf('<th colspan="2" style="%stext-align:center;">Time to peak (days)</th>', TH),
    sprintf('<th colspan="2" style="%stext-align:center;">Final size</th>', TH),
    '</tr>\n<tr>',
    paste0(sprintf('<th style="%stext-align:right;">%s</th>', TH, c("Ag. workers","Community")), collapse = ""),
    paste0(sprintf('<th style="%stext-align:right;">%s</th>', TH, c("Ag. workers","Community")), collapse = ""),
    paste0(sprintf('<th style="%stext-align:right;">%s</th>', TH, c("Ag. workers","Community")), collapse = ""),
    '</tr>\n</thead>\n')

  cell_fn <- function(row) {
    paste0(
      sprintf('<td style="%s">%s</td>', TDR, fmt_pct1(row$peak_prevalence_A)),
      sprintf('<td style="%s">%s</td>', TDR, fmt_pct1(row$peak_prevalence_C)),
      sprintf('<td style="%s">%s</td>', TDR, fmt_int(row$time_to_peak_A)),
      sprintf('<td style="%s">%s</td>', TDR, fmt_int(row$time_to_peak_C)),
      sprintf('<td style="%s">%s</td>', TDR, fmt_pct1(row$final_attack_rate_A)),
      sprintf('<td style="%s">%s</td>', TDR, fmt_pct1(row$final_attack_rate_C)))
  }

  caption <- paste0("Table S3. Summary statistics for simulated epidemics across regions and ",
    "parameter sets. Simulated peak prevalence, time to epidemic peak, and final size for ",
    "agricultural workers (Ag. workers) and the general community (Community). The baseline uses ",
    "R&#8320; = 2.0; each other row varies a single parameter (Supplementary Table S2).")

  render_table(build_block_spec(wide), header,
               function(st, sv) match_block(wide, st, sv), cell_fn, caption,
               file.path(out_dir, "table_s3.html"))
}

# ------------------------------------------------------------------------------
# Table S4 - Differential metrics
# ------------------------------------------------------------------------------
build_s4 <- function() {
  d <- read_csv(file.path(out_dir, "sensitivity_differential.csv"), show_col_types = FALSE)

  header <- paste0(
    '<thead>\n<tr>',
    sprintf('<th style="%stext-align:left;">Parameter set</th>', TH),
    sprintf('<th style="%stext-align:left;">Region</th>', TH),
    sprintf('<th style="%stext-align:right;">Peak prevalence ratio</th>', TH),
    sprintf('<th style="%stext-align:right;">Final size ratio</th>', TH),
    sprintf('<th style="%stext-align:right;">Peak timing difference (days)</th>', TH),
    sprintf('<th style="%stext-align:right;">Max. relative prevalence</th>', TH),
    '</tr>\n</thead>\n')

  cell_fn <- function(row) {
    paste0(
      sprintf('<td style="%s">%s</td>', TDR, fmt_num(row$peak_prevalence_ratio)),
      sprintf('<td style="%s">%s</td>', TDR, fmt_num(row$attack_rate_ratio)),
      sprintf('<td style="%s">%s</td>', TDR, fmt_int(row$time_to_peak_diff)),
      sprintf('<td style="%s">%s</td>', TDR, fmt_num(row$max_relative_infection)))
  }

  caption <- paste0("Table S4. Differential metrics between agricultural workers and the general ",
    "community across regions and parameter sets. Peak prevalence ratio, final size ratio, peak ",
    "timing difference, and maximum relative prevalence between agricultural workers and the ",
    "general community. The baseline uses R&#8320; = 2.0.")

  render_table(build_block_spec(d), header,
               function(st, sv) match_block(d, st, sv), cell_fn, caption,
               file.path(out_dir, "table_s4.html"))
}

# ------------------------------------------------------------------------------
# Table S6 - Crop production losses across R0 and symptomatic fraction
# ------------------------------------------------------------------------------
build_s6 <- function() {
  cr <- read_csv(file.path(out_dir, "crop_impact_r0_comparison.csv"), show_col_types = FALSE) %>%
    mutate(psymp_disp = ifelse(p_symp_scenario == "all_symptomatic", 1, round(p_symp_A, 2))) %>%
    arrange(psymp_disp, r0, commodity)

  header <- paste0(
    '<thead>\n<tr>',
    paste0(sprintf('<th style="%stext-align:%s;">%s</th>', TH,
                   c("center","center","left","right","right","left","right","right"),
                   c("R&#8320;", "p<sub>symp,A</sub>", "Crop", "2024 value (USD)",
                     "Worst peak day", "Month", "Max loss (%)", "Max loss (USD)")),
           collapse = ""),
    '</tr>\n</thead>\n')

  # format="f" (not "d") avoids 32-bit integer overflow on billion-dollar values
  dollar <- function(x) paste0("$", formatC(round(x), format = "f", digits = 0, big.mark = ","))
  body <- ""
  for (i in seq_len(nrow(cr))) {
    row <- cr[i, ]
    body <- paste0(body, "<tr>",
      sprintf('<td style="%stext-align:center;">%s</td>', TD, formatC(row$r0, format = "fg")),
      sprintf('<td style="%stext-align:center;">%s</td>', TD, formatC(row$psymp_disp, format = "fg")),
      sprintf('<td style="%s">%s</td>', TDL, row$commodity),
      sprintf('<td style="%s">%s</td>', TDR, dollar(row$annual_value_usd)),
      sprintf('<td style="%s">%d</td>', TDR, as.integer(row$worst_peakday)),
      sprintf('<td style="%s">%s</td>', TDL, row$worst_peakday_month),
      sprintf('<td style="%s">%s</td>', TDR, paste0(formatC(row$pct_loss, format = "f", digits = 2), "%")),
      sprintf('<td style="%s">%s</td>', TDR, dollar(row$dollar_loss_usd)),
      "</tr>\n")
  }

  caption <- paste0("Table S6. Estimated harvest-related crop production losses due to ",
    "epidemic-induced workforce illness across R&#8320; values and symptomatic proportions. ",
    "For each crop we report the worst-case epidemic peak timing, the corresponding maximum ",
    "production loss as a percentage of total annual production, and the estimated dollar value ",
    "of that loss based on 2024 California crop values, at the baseline symptomatic probability ",
    "(p<sub>symp,A</sub> adjusting for obesity) and assuming all infections are symptomatic ",
    "(p<sub>symp,A</sub> = 1).")

  path <- file.path(out_dir, "table_s6.html")
  writeLines(paste0(html_head(caption), header, "<tbody>\n", body, "</tbody>\n", html_foot), path)
  cat("Saved:", path, "\n")
}

# ------------------------------------------------------------------------------
build_s3()
build_s4()
build_s6()
cat("SI tables regenerated.\n")
