# ==============================================================================
# sensitivity_analysis.R - Compare Results Across Sensitivity Analyses
# ==============================================================================
# This script loads all regional simulation outputs and creates comparative
# summaries and visualizations across the sensitivity dimensions:
#   1. R0 values: 1.2, 1.5 (baseline), 2.0, 3.0
#   2. Assortativity (eta = 1-eps): 0, 1/4, 1/3, 1/2, 2/3 (baseline), 3/4
#   3. SAR in crowded households: 30%, 40% (baseline), 50%, 60%
#   4. Crowding fold difference: 1, 2 (baseline), 3
#   5. Gamma (recovery rate): 1/3, 1/5 (baseline), 1/10
#   6. Seed target: C only, both (baseline), A only
#
# Baseline values are defined in config.R (default_pars)
#
# Outputs:
#   - output/sensitivity_summary.csv - Summary statistics for all parameter sets
#   - figures/sensitivity_*.pdf - Comparison figures
# ==============================================================================

# Load dependencies (skip if already loaded)
if (!exists("paths")) {
  source('code/setup.R')
}

# Load parameter metadata
if (!exists("pars_metadata")) {
  source('code/parameters.R')
}

# ==============================================================================
# Load All Simulation Results
# ==============================================================================

#' Load all regional simulation output files
#' @param output_dir Directory containing output files
#' @param prefix File prefix (default: "regional_sim_")
#' @return Combined tibble with all results
load_all_regional_outputs <- function(output_dir = paths$output_dir,
                                       prefix = "regional_sim_") {

  # Find all matching files
  pattern <- paste0("^", prefix, ".*\\.csv$")
  files <- list.files(output_dir, pattern = pattern, full.names = TRUE)

  if (length(files) == 0) {
    stop("No regional output files found in ", output_dir)
  }

  cat("Found", length(files), "regional output files\n")

  # Load and combine files, skipping any that lack metadata columns
  required_cols <- c("parset", "parset_name", "sens_type", "sens_value")
  all_data <- lapply(files, function(f) {
    df <- read_csv(f, show_col_types = FALSE)
    if (!all(required_cols %in% names(df))) {
      cat("  Skipping (no metadata):", basename(f), "\n")
      return(NULL)
    }
    cat("  Loading:", basename(f), "\n")
    df
  }) %>%
    bind_rows()

  return(all_data)
}

# ==============================================================================
# Summary Statistics Functions
# ==============================================================================

#' Calculate epidemic summary statistics
#' @param df Data frame with columns: t, subpop, S_indiv, I_indiv, R_indiv, REGION6, parset_name
#' @return Tibble with summary statistics per (parset_name, REGION6, subpop)
calculate_summary_stats <- function(df) {

  summary_df <- df %>%
    group_by(parset, parset_name, sens_type, sens_value, REGION6, subpop) %>%
    summarise(
      # Peak prevalence
      peak_prevalence = max(I_indiv, na.rm = TRUE),
      time_to_peak = t[which.max(I_indiv)],

      # Final attack rate (proportion ever infected = final R)
      final_attack_rate = last(R_indiv),

      # Time to 1% prevalence (early detection threshold)
      time_to_1pct = {
        idx <- which(I_indiv >= 0.01)[1]
        if (is.na(idx)) NA_real_ else t[idx]
      },

      # Epidemic duration (time from 1% to below 1% prevalence)
      epidemic_duration = {
        above_1pct <- I_indiv >= 0.01
        if (sum(above_1pct) == 0) NA_real_ else {
          first_above <- which(above_1pct)[1]
          last_above <- tail(which(above_1pct), 1)
          t[last_above] - t[first_above]
        }
      },

      .groups = "drop"
    )

  return(summary_df)
}

#' Calculate differential statistics (ag workers vs community)
#' @param summary_df Output from calculate_summary_stats
#' @return Tibble with differential metrics
calculate_differential_stats <- function(summary_df) {

  diff_df <- summary_df %>%
    select(parset, parset_name, sens_type, sens_value, REGION6, subpop,
           peak_prevalence, final_attack_rate, time_to_peak) %>%
    pivot_wider(
      names_from = subpop,
      values_from = c(peak_prevalence, final_attack_rate, time_to_peak)
    ) %>%
    mutate(
      # Absolute differences (A - C)
      peak_prevalence_diff = peak_prevalence_A - peak_prevalence_C,
      attack_rate_diff = final_attack_rate_A - final_attack_rate_C,
      time_to_peak_diff = time_to_peak_A - time_to_peak_C,

      # Relative differences (A / C)
      peak_prevalence_ratio = peak_prevalence_A / peak_prevalence_C,
      attack_rate_ratio = final_attack_rate_A / final_attack_rate_C
    )

  return(diff_df)
}

#' Calculate peak relative infection rate (max A/C ratio over time) from time series
#' @param all_data Combined simulation output with t, subpop, I_indiv, REGION6, parset_name, etc.
#' @return Tibble with max_relative_infection per (parset_name, REGION6)
calculate_max_relative_infection <- function(all_data) {
  # Pivot A and C into separate columns; use t, REGION6, parset_name as unique ID
  wide <- all_data %>%
    select(t, subpop, I_indiv, REGION6, parset, parset_name, sens_type, sens_value) %>%
    pivot_wider(
      id_cols = c(t, REGION6, parset, parset_name, sens_type, sens_value),
      names_from = subpop,
      values_from = I_indiv
    ) %>%
    filter(C > 0) %>%
    mutate(rel_inf = A / C)

  wide %>%
    group_by(parset, parset_name, sens_type, sens_value, REGION6) %>%
    summarise(max_relative_infection = max(rel_inf, na.rm = TRUE), .groups = "drop")
}

# ==============================================================================
# Visualization Functions
# ==============================================================================

# Baseline parameter values from config.R
# Used to map the baseline parset to correct values for each dimension
baseline_values <- list(
  r0 = default_pars$r0,
  eps = default_pars$eps,
  sar = default_pars$sar_crowded,
  fold = default_pars$crowding_fold_diff,
  gamma = default_pars$gamma,
  seed = 2                        # Numeric code: 1=C only, 2=Both (baseline), 3=A only
)

#' Prepare data for sensitivity plotting
#'
#' For each sensitivity dimension, includes both the dedicated sensitivity runs
#' AND the baseline R0 parset, with the baseline's sens_value correctly mapped
#' to that dimension's baseline parameter value.
#'
#' @param df Data frame with sens_type, sens_value, parset_name columns
#' @param sens_dimension Which dimension to prepare ("r0", "eps", "sar", "fold", "gamma")
#' @return Data frame ready for plotting with correct sens_value for x-axis
prepare_sensitivity_data <- function(df, sens_dimension) {
  baseline_parset <- paste0("r0_", default_pars$r0)

  # Get rows for this sensitivity dimension
  sens_rows <- df %>%
    filter(!is.na(sens_type) & sens_type == sens_dimension)

  # Get baseline row and update sens_value to the correct baseline for this dimension
  baseline_rows <- df %>%
    filter(parset_name == baseline_parset) %>%
    mutate(
      sens_type = sens_dimension,
      sens_value = baseline_values[[sens_dimension]]
    )

  # Combine: if this is the r0 dimension, baseline is already included
  # Otherwise, we add the remapped baseline
  if (sens_dimension == "r0") {
    result <- sens_rows
  } else {
    # For non-r0 dimensions, check if baseline value already exists
    baseline_val <- baseline_values[[sens_dimension]]
    existing_vals <- unique(sens_rows$sens_value)

    if (baseline_val %in% existing_vals) {
      # Baseline already represented (shouldn't happen with current parameters.R logic)
      result <- sens_rows
    } else {
      # Add the remapped baseline
      result <- bind_rows(sens_rows, baseline_rows)
    }
  }

  # Transform eps → eta (eta = 1 - eps) for display
  if (sens_dimension == "eps") {
    result <- result %>%
      mutate(sens_value = round(1 - sens_value, 2))
  }

  # Transform gamma → infectious period in days (1/gamma) for display
  if (sens_dimension == "gamma") {
    result <- result %>%
      mutate(sens_value = round(1 / sens_value))
  }

  # Transform seed numeric codes → labels for display
  if (sens_dimension == "seed") {
    seed_labels <- c("1" = "C only", "2" = "Both", "3" = "A only")
    result <- result %>%
      mutate(sens_value = factor(seed_labels[as.character(sens_value)],
                                 levels = c("C only", "Both", "A only")))
  }

  return(result)
}

#' Create multi-panel sensitivity overview figure
#' @param diff_df Differential statistics data frame
#' @param metric Which metric to plot
#' @return ggplot object (faceted)
plot_sensitivity_overview <- function(diff_df, metric = "attack_rate_diff") {

  # Combine data from all numeric sensitivity dimensions with correct baseline mappings
  # (seed is excluded: categorical dimension, plotted separately via individual plots)
  sens_dimensions <- c("r0", "eps", "sar", "fold", "gamma")
  plot_data <- bind_rows(
    lapply(sens_dimensions, function(dim) prepare_sensitivity_data(diff_df, dim))
  ) %>%
    mutate(
      sens_type_label = case_when(
        sens_type == "r0" ~ "R0",
        sens_type == "eps" ~ "Assortativity (eta)",
        sens_type == "sar" ~ "SAR (Crowded)",
        sens_type == "fold" ~ "Crowding Fold",
        sens_type == "gamma" ~ "Infectious Period",
        sens_type == "seed" ~ "Seed Target",
        TRUE ~ sens_type
      ),
      sens_type_label = factor(sens_type_label,
                               levels = c("R0", "Assortativity (eta)",
                                          "SAR (Crowded)", "Crowding Fold",
                                          "Infectious Period",
                                          "Seed Target"))
    ) %>%
    # Create numeric x for proper line connections within each facet
    group_by(sens_type) %>%
    mutate(x_numeric = as.numeric(factor(sens_value))) %>%
    ungroup()

  metric_labels <- c(
    "attack_rate_diff" = "Difference in Final Size\n(Agricultural - Community)",
    "peak_prevalence_diff" = "Peak Prevalence Difference\n(Agricultural - Community)",
    "time_to_peak_diff" = "Peak Timing Difference in Days\n(Agricultural - Community)",
    "max_relative_infection" = "Max Relative Prevalence\n(Agricultural / Community)",
    "peak_prevalence_ratio" = "Peak Prevalence Ratio\n(Agricultural / Community)",
    "attack_rate_ratio" = "Final Size Ratio\n(Agricultural / Community)",
    "final_attack_rate_A" = "Final Size\n(Agricultural Workers)"
  )

  # Colorblind-friendly palette (Okabe-Ito) and region labels
  cb_palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")
  region_labels <- setNames(region_map$REGION_NAME, region_map$REGION6)

  p <- plot_data %>%
    ggplot(aes(x = factor(sens_value), y = .data[[metric]], color = factor(REGION6))) +
    geom_line(aes(group = REGION6), alpha = 0.4, linewidth = 1) +
    geom_point(size = 1.5, alpha = 0.8) +
    facet_wrap(~sens_type_label, scales = "free_x", nrow = 1) +
    scale_color_manual(values = cb_palette, labels = region_labels) +
    labs(
      x = "Parameter Value",
      y = metric_labels[metric],
      color = "Region"
    ) +
    theme_classic(base_size = 17) +
    theme(
      legend.position = "bottom",
      strip.text = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )

  if (grepl("diff", metric)) {
    p <- p + geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.5)
  } else if (grepl("relative|ratio", metric)) {
    p <- p + geom_hline(yintercept = 1, linetype = "dashed", color = "gray50", alpha = 0.5)
  }

  return(p)
}

#' Plot epidemic curves across sensitivity values
#' @param all_data Combined simulation output
#' @param sens_dimension Sensitivity dimension to compare
#' @param region Which region to plot (1-6)
#' @return ggplot object
plot_epidemic_curves <- function(all_data, sens_dimension, region = 1) {

  # Use helper to get data with correctly mapped baseline values
  plot_data <- prepare_sensitivity_data(all_data, sens_dimension) %>%
    filter(REGION6 == region) %>%
    mutate(
      sens_label = paste0(sens_dimension, " = ", sens_value)
    )

  p <- plot_data %>%
    ggplot(aes(x = t, y = I_indiv, color = factor(sens_value), linetype = subpop)) +
    geom_line(linewidth = 0.8, alpha = 0.8) +
    labs(
      x = "Time (days)",
      y = "Proportion Infected",
      color = paste0(c("r0" = "R0", "eps" = "eta", "sar" = "SAR", "fold" = "FOLD", "gamma" = "Infectious Period", "seed" = "Seed Target")[sens_dimension], " Value"),
      linetype = "Population",
      title = paste("Epidemic Curves - Region", region)
    ) +
    scale_linetype_manual(
      values = c("A" = "solid", "C" = "dashed"),
      labels = c("A" = "Agricultural", "C" = "Community")
    ) +
    theme_classic(base_size = 11) +
    theme(legend.position = "bottom")

  return(p)
}

#' Plot epidemic curves across sensitivity values for all regions (faceted)
#' @param all_data Combined simulation output
#' @param sens_dimension Sensitivity dimension to compare
#' @param metric Which metric to plot: "I_indiv" (current infections) or "R_indiv" (cumulative)
#' @return ggplot object with facets for each region
plot_epidemic_curves_all_regions <- function(all_data, sens_dimension, metric = "I_indiv") {

  sens_labels <- c(
    "r0" = "R0",
    "eps" = "Assortativity (eta)",
    "sar" = "SAR (Crowded)",
    "fold" = "Crowding Fold Diff.",
    "gamma" = "Infectious Period",
    "seed" = "Seed Target"
  )

  metric_labels <- c(
    "I_indiv" = "Proportion Currently Infected",
    "R_indiv" = "Cumulative Proportion Infected"
  )

  title_type <- c(
    "I_indiv" = "Epidemic Curves",
    "R_indiv" = "Cumulative Infections"
  )

  # Use helper to get data with correctly mapped baseline values
  plot_data <- prepare_sensitivity_data(all_data, sens_dimension) %>%
    mutate(
      region_label = paste("Region", REGION6)
    )

  # Okabe-Ito colorblind-friendly palette
  cb_palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")
  n_levels <- length(unique(plot_data$sens_value))

  p <- plot_data %>%
    ggplot(aes(x = t, y = .data[[metric]], color = factor(sens_value), linetype = subpop)) +
    geom_line(linewidth = 0.9, alpha = 0.8) +
    facet_wrap(~region_label, ncol = 3) +
    {if (metric == "R_indiv") scale_y_continuous(limits = c(0, 1))} +
    scale_color_manual(values = cb_palette[1:n_levels]) +
    labs(
      x = "Time (days)",
      y = metric_labels[metric],
      color = sens_labels[sens_dimension],
      linetype = "Population",
      title = paste(title_type[metric], "by Region - Sensitivity to", sens_labels[sens_dimension])
    ) +
    scale_linetype_manual(
      values = c("A" = "solid", "C" = "dashed"),
      labels = c("A" = "Agricultural", "C" = "Community")
    ) +
    theme_classic(base_size = 17) +
    theme(
      legend.position = "bottom",
      strip.text = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    ) +
    guides(color = guide_legend(nrow = 1), linetype = guide_legend(nrow = 1))

  return(p)
}

#' Plot relative infection rate (A/C) across sensitivity values for all regions (faceted)
#' @param all_data Combined simulation output
#' @param sens_dimension Sensitivity dimension to compare
#' @return ggplot object with facets for each region
plot_relative_infection_all_regions <- function(all_data, sens_dimension) {

  sens_labels <- c(
    "r0" = "R0",
    "eps" = "Assortativity (eta)",
    "sar" = "SAR (Crowded)",
    "fold" = "Crowding Fold Diff.",
    "gamma" = "Infectious Period",
    "seed" = "Seed Target"
  )

  # Use helper to get data with correctly mapped baseline values
  plot_data <- prepare_sensitivity_data(all_data, sens_dimension) %>%
    select(t, subpop, I_indiv, REGION6, sens_value) %>%
    pivot_wider(names_from = "subpop", values_from = "I_indiv") %>%
    mutate(
      rel_inf = A / C,
      region_label = paste("Region", REGION6)
    )

  # Okabe-Ito colorblind-friendly palette
  cb_palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")
  n_levels <- length(unique(plot_data$sens_value))

  p <- plot_data %>%
    ggplot(aes(x = t, y = rel_inf, color = factor(sens_value))) +
    geom_line(linewidth = 0.9, alpha = 0.8) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "grey50", alpha = 0.5) +
    facet_wrap(~region_label, ncol = 3) +
    expand_limits(y = 0.5) +
    scale_color_manual(values = cb_palette[1:n_levels]) +
    labs(
      x = "Time (days)",
      y = "Relative Prevalence (Agricultural / Community)",
      color = sens_labels[sens_dimension],
      title = paste("Relative Prevalence by Region - Sensitivity to", sens_labels[sens_dimension])
    ) +
    theme_classic(base_size = 17) +
    theme(
      legend.position = "bottom",
      strip.text = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    ) +
    guides(color = guide_legend(nrow = 1))

  return(p)
}

# ==============================================================================
# Main Analysis
# ==============================================================================

run_sensitivity_analysis <- function() {

  cat("\n", rep("=", 60), "\n", sep = "")
  cat("Running Sensitivity Analysis\n")
  cat(rep("=", 60), "\n\n", sep = "")

  # Load all results
  cat("Loading simulation results...\n")
  all_data <- load_all_regional_outputs()

  cat("\nData summary:\n")
  cat("  Total rows:", nrow(all_data), "\n")
  cat("  Parameter sets:", length(unique(all_data$parset_name)), "\n")
  cat("  Regions:", length(unique(all_data$REGION6)), "\n")

  # Calculate summary statistics
  cat("\nCalculating summary statistics...\n")
  summary_stats <- calculate_summary_stats(all_data)
  diff_stats <- calculate_differential_stats(summary_stats)

  # Calculate max relative infection (peak A/C ratio) from time series
  max_rel_inf <- calculate_max_relative_infection(all_data)
  diff_stats <- diff_stats %>%
    left_join(max_rel_inf, by = c("parset", "parset_name", "sens_type", "sens_value", "REGION6"))

  # Save summary tables
  write_csv(summary_stats, file.path(paths$output_dir, "sensitivity_summary.csv"))
  write_csv(diff_stats, file.path(paths$output_dir, "sensitivity_differential.csv"))
  cat("  Saved: sensitivity_summary.csv, sensitivity_differential.csv\n")

  # Create and save figures
  cat("\nGenerating figures...\n")

  # Overview figure - attack rate differential across all dimensions
  fig_overview <- plot_sensitivity_overview(diff_stats, "attack_rate_diff")
  ggsave(file.path(paths$figures_dir, "sensitivity_overview_attackrate.pdf"),
         fig_overview, width = 12, height = 5)
  ggsave(file.path(paths$figures_dir, "sensitivity_overview_attackrate.png"),
         fig_overview, width = 12, height = 5, dpi = 300)
  cat("  Saved: sensitivity_overview_attackrate.pdf/.png\n")

  # Overview figure - peak prevalence differential across all dimensions
  fig_overview_peak <- plot_sensitivity_overview(diff_stats, "peak_prevalence_diff")
  ggsave(file.path(paths$figures_dir, "sensitivity_overview_peaksize.pdf"),
         fig_overview_peak, width = 12, height = 5)
  ggsave(file.path(paths$figures_dir, "sensitivity_overview_peaksize.png"),
         fig_overview_peak, width = 12, height = 5, dpi = 300)
  cat("  Saved: sensitivity_overview_peaksize.pdf/.png\n")

  # Overview figure - peak timing differential across all dimensions
  fig_overview_timing <- plot_sensitivity_overview(diff_stats, "time_to_peak_diff")
  ggsave(file.path(paths$figures_dir, "sensitivity_overview_peaktiming.pdf"),
         fig_overview_timing, width = 12, height = 5)
  ggsave(file.path(paths$figures_dir, "sensitivity_overview_peaktiming.png"),
         fig_overview_timing, width = 12, height = 5, dpi = 300)
  cat("  Saved: sensitivity_overview_peaktiming.pdf/.png\n")

  # Overview figure - max relative infection rate across all dimensions
  fig_overview_relinf <- plot_sensitivity_overview(diff_stats, "max_relative_infection")
  ggsave(file.path(paths$figures_dir, "sensitivity_overview_max_relative_infection.pdf"),
         fig_overview_relinf, width = 12, height = 5)
  ggsave(file.path(paths$figures_dir, "sensitivity_overview_max_relative_infection.png"),
         fig_overview_relinf, width = 12, height = 5, dpi = 300)
  cat("  Saved: sensitivity_overview_max_relative_infection.pdf/.png\n")

  # Overview figure - peak prevalence ratio across all dimensions
  fig_overview_peakratio <- plot_sensitivity_overview(diff_stats, "peak_prevalence_ratio")
  ggsave(file.path(paths$figures_dir, "sensitivity_overview_peak_prevalence_ratio.pdf"),
         fig_overview_peakratio, width = 12, height = 5)
  ggsave(file.path(paths$figures_dir, "sensitivity_overview_peak_prevalence_ratio.png"),
         fig_overview_peakratio, width = 12, height = 5, dpi = 300)
  cat("  Saved: sensitivity_overview_peak_prevalence_ratio.pdf/.png\n")

  # Overview figure - attack rate ratio across all dimensions
  fig_overview_arratio <- plot_sensitivity_overview(diff_stats, "attack_rate_ratio")
  ggsave(file.path(paths$figures_dir, "sensitivity_overview_attack_rate_ratio.pdf"),
         fig_overview_arratio, width = 12, height = 5)
  ggsave(file.path(paths$figures_dir, "sensitivity_overview_attack_rate_ratio.png"),
         fig_overview_arratio, width = 12, height = 5, dpi = 300)
  cat("  Saved: sensitivity_overview_attack_rate_ratio.pdf/.png\n")

  # Individual sensitivity dimension plots
  sens_dimensions <- c("r0", "eps", "sar", "fold", "gamma", "seed")

  for (sens_dim in sens_dimensions) {
    # Check if data exists for this dimension (handle NA values)
    n_rows <- sum(diff_stats$sens_type == sens_dim, na.rm = TRUE)

    if (n_rows > 0) {
      cat("  Processing:", sens_dim, "(", n_rows, "rows)\n")

      # Epidemic curves for region 6 (California)
      fig_curves <- plot_epidemic_curves(all_data, sens_dim, region = 6)
      filename <- paste0("sensitivity_", sens_dim, "_curves_region6")
      ggsave(file.path(paths$figures_dir, paste0(filename, ".pdf")), fig_curves, width = 8, height = 5)
      cat("    Saved:", paste0(filename, ".pdf (California)\n"))

      # Epidemic curves for all regions (faceted) - current infections
      fig_curves_all <- plot_epidemic_curves_all_regions(all_data, sens_dim, metric = "I_indiv")
      filename_all <- paste0("sensitivity_", sens_dim, "_curves_all_regions")
      ggsave(file.path(paths$figures_dir, paste0(filename_all, ".pdf")), fig_curves_all, width = 12, height = 8)
      ggsave(file.path(paths$figures_dir, paste0(filename_all, ".png")), fig_curves_all, width = 12, height = 8, dpi = 300)
      cat("    Saved:", paste0(filename_all, ".pdf/.png\n"))

      # Cumulative infections for all regions (faceted)
      fig_cumul_all <- plot_epidemic_curves_all_regions(all_data, sens_dim, metric = "R_indiv")
      filename_cumul <- paste0("sensitivity_", sens_dim, "_cumulative_all_regions")
      ggsave(file.path(paths$figures_dir, paste0(filename_cumul, ".pdf")), fig_cumul_all, width = 12, height = 8)
      ggsave(file.path(paths$figures_dir, paste0(filename_cumul, ".png")), fig_cumul_all, width = 12, height = 8, dpi = 300)
      cat("    Saved:", paste0(filename_cumul, ".pdf/.png\n"))

      # Relative infection rate (A/C) for all regions (faceted)
      fig_rel_inf_all <- plot_relative_infection_all_regions(all_data, sens_dim)
      filename_rel <- paste0("sensitivity_", sens_dim, "_relative_infection_all_regions")
      ggsave(file.path(paths$figures_dir, paste0(filename_rel, ".pdf")), fig_rel_inf_all, width = 12, height = 8)
      ggsave(file.path(paths$figures_dir, paste0(filename_rel, ".png")), fig_rel_inf_all, width = 12, height = 8, dpi = 300)
      cat("    Saved:", paste0(filename_rel, ".pdf/.png\n"))
    } else {
      cat("  Skipping:", sens_dim, "(no data)\n")
    }
  }

  # Print summary table
  cat("\n", rep("=", 60), "\n", sep = "")
  cat("Summary Statistics by Sensitivity Dimension\n")
  cat(rep("=", 60), "\n\n", sep = "")

  # Mean differential statistics across regions
  # Filter out baseline row (sens_type = NA) since it's already included in r0 dimension
  summary_table <- diff_stats %>%
    filter(!is.na(sens_type)) %>%
    group_by(sens_type, sens_value) %>%
    summarise(
      mean_attack_rate_diff = mean(attack_rate_diff, na.rm = TRUE),
      sd_attack_rate_diff = sd(attack_rate_diff, na.rm = TRUE),
      mean_peak_diff = mean(peak_prevalence_diff, na.rm = TRUE),
      mean_attack_rate_A = mean(final_attack_rate_A, na.rm = TRUE),
      mean_attack_rate_C = mean(final_attack_rate_C, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(sens_type, sens_value)

  print(summary_table, n = 20)

  cat("\nSensitivity analysis complete.\n")

  # Return objects for further analysis
  return(list(
    all_data = all_data,
    summary_stats = summary_stats,
    diff_stats = diff_stats,
    summary_table = summary_table
  ))
}

# ==============================================================================
# County-Level Summary Statistics
# ==============================================================================

#' Compute county-level epidemic summary statistics and A/C ratios
#' @param county_file Path to county simulation CSV (default: additive method)
#' @return List with county_diff (per-county stats) and county_quantiles (by region)
calculate_county_summary <- function(county_file = file.path(paths$output_dir, "county_sim_additive.csv")) {
  if (!file.exists(county_file)) {
    cat("County simulation file not found:", county_file, "\n")
    return(NULL)
  }

  cat("Loading county simulation data...\n")
  county_data <- read_csv(county_file, show_col_types = FALSE)

  # Per-county summary stats (same metrics as regional)
  county_stats <- county_data %>%
    group_by(GEOID, REGION6, subpop) %>%
    summarise(
      peak_prevalence = max(I_indiv, na.rm = TRUE),
      time_to_peak = t[which.max(I_indiv)],
      final_attack_rate = last(R_indiv),
      .groups = "drop"
    )

  # Compute A/C ratios per county
  county_diff <- county_stats %>%
    select(GEOID, REGION6, subpop, peak_prevalence, final_attack_rate, time_to_peak) %>%
    pivot_wider(
      names_from = subpop,
      values_from = c(peak_prevalence, final_attack_rate, time_to_peak)
    ) %>%
    mutate(
      peak_prevalence_ratio = peak_prevalence_A / peak_prevalence_C,
      attack_rate_ratio = final_attack_rate_A / final_attack_rate_C,
      time_to_peak_diff = time_to_peak_A - time_to_peak_C
    )

  # Summarize by region: median and 20th/80th percentiles
  county_quantiles <- county_diff %>%
    group_by(REGION6) %>%
    summarise(
      n_counties = n(),
      peak_prev_ratio_median = median(peak_prevalence_ratio, na.rm = TRUE),
      peak_prev_ratio_q20 = quantile(peak_prevalence_ratio, 0.20, na.rm = TRUE),
      peak_prev_ratio_q80 = quantile(peak_prevalence_ratio, 0.80, na.rm = TRUE),
      attack_rate_ratio_median = median(attack_rate_ratio, na.rm = TRUE),
      attack_rate_ratio_q20 = quantile(attack_rate_ratio, 0.20, na.rm = TRUE),
      attack_rate_ratio_q80 = quantile(attack_rate_ratio, 0.80, na.rm = TRUE),
      .groups = "drop"
    )

  write_csv(county_diff, file.path(paths$output_dir, "county_differential.csv"))
  write_csv(county_quantiles, file.path(paths$output_dir, "county_quantiles.csv"))
  cat("  Saved: county_differential.csv, county_quantiles.csv\n")

  return(list(county_diff = county_diff, county_quantiles = county_quantiles))
}

# ==============================================================================
# Run Analysis
# ==============================================================================
# Always run when sourced (the function handles its own error checking)

results <- run_sensitivity_analysis()

# Run county-level summary if county data exists
county_results <- calculate_county_summary()
