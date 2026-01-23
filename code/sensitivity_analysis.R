# ==============================================================================
# sensitivity_analysis.R - Compare Results Across Sensitivity Analyses
# ==============================================================================
# This script loads all regional simulation outputs and creates comparative
# summaries and visualizations across the sensitivity dimensions:
#   1. R0 values: 1.2 (baseline), 1.5, 2.0, 3.0
#   2. Assortativity (eps): 0, 0.33 (baseline), 0.5, 0.7
#   3. SAR in crowded households: 30%, 40% (baseline), 50%, 60%
#   4. Crowding fold difference: 1, 2 (baseline), 3
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
#' @param prefix File prefix (default: "epidf_indiv_full_regional_")
#' @return Combined tibble with all results
load_all_regional_outputs <- function(output_dir = paths$output_dir,
                                       prefix = "epidf_indiv_full_regional_") {

  # Find all matching files
  pattern <- paste0("^", prefix, ".*\\.csv$")
  files <- list.files(output_dir, pattern = pattern, full.names = TRUE)

  if (length(files) == 0) {
    stop("No regional output files found in ", output_dir)
  }

  cat("Found", length(files), "regional output files\n")

  # Load and combine all files
  all_data <- lapply(files, function(f) {
    cat("  Loading:", basename(f), "\n")
    read_csv(f, show_col_types = FALSE)
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

#' Calculate differential statistics (Ag workers vs Community)
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

# ==============================================================================
# Visualization Functions
# ==============================================================================

#' Create sensitivity comparison plot for a given dimension
#' @param diff_df Differential statistics data frame
#' @param sens_dimension Sensitivity dimension to plot ("r0", "eps", "sar", "fold")
#' @param metric Which metric to plot ("attack_rate_diff", "peak_prevalence_diff", etc.)
#' @param add_baseline Whether to add baseline reference line
#' @return ggplot object
plot_sensitivity <- function(diff_df, sens_dimension, metric = "attack_rate_diff",
                             add_baseline = TRUE) {

  # Filter to relevant sensitivity dimension (include baseline r0_1.2 for reference)
  baseline_parset <- "r0_1.2"

  plot_data <- diff_df %>%
    filter(sens_type == sens_dimension | parset_name == baseline_parset)

  # Get nice labels
  metric_labels <- c(
    "attack_rate_diff" = "Attack Rate Difference (A - C)",
    "peak_prevalence_diff" = "Peak Prevalence Difference (A - C)",
    "attack_rate_ratio" = "Attack Rate Ratio (A / C)",
    "peak_prevalence_ratio" = "Peak Prevalence Ratio (A / C)"
  )

  sens_labels <- c(
    "r0" = "Basic Reproduction Number (R0)",
    "eps" = "Assortativity (\u03B5)",
    "sar" = "SAR in Crowded Households",
    "fold" = "Crowding Fold Difference"
  )

  p <- plot_data %>%
    ggplot(aes(x = factor(sens_value), y = .data[[metric]], fill = factor(REGION6))) +
    geom_boxplot(alpha = 0.7, position = position_dodge(0.8)) +
    labs(
      x = sens_labels[sens_dimension],
      y = metric_labels[metric],
      fill = "Region",
      title = paste("Sensitivity to", sens_labels[sens_dimension])
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5)
    )

  if (add_baseline && grepl("diff", metric)) {
    p <- p + geom_hline(yintercept = 0, linetype = "dashed", color = "gray50")
  }
  if (add_baseline && grepl("ratio", metric)) {
    p <- p + geom_hline(yintercept = 1, linetype = "dashed", color = "gray50")
  }

  return(p)
}

#' Create multi-panel sensitivity overview figure
#' @param diff_df Differential statistics data frame
#' @param metric Which metric to plot
#' @return ggplot object (faceted)
plot_sensitivity_overview <- function(diff_df, metric = "attack_rate_diff") {

  # Prepare data with nice factor labels
  plot_data <- diff_df %>%
    mutate(
      sens_type_label = case_when(
        sens_type == "r0" ~ "R0",
        sens_type == "eps" ~ "Assortativity (\u03B5)",
        sens_type == "sar" ~ "SAR (Crowded)",
        sens_type == "fold" ~ "Crowding Fold"
      ),
      sens_type_label = factor(sens_type_label,
                               levels = c("R0", "Assortativity (\u03B5)",
                                          "SAR (Crowded)", "Crowding Fold"))
    )

  metric_labels <- c(
    "attack_rate_diff" = "Attack Rate Difference\n(Agricultural - Community)",
    "peak_prevalence_diff" = "Peak Prevalence Difference\n(Agricultural - Community)",
    "final_attack_rate_A" = "Final Attack Rate\n(Agricultural Workers)"
  )

  p <- plot_data %>%
    ggplot(aes(x = factor(sens_value), y = .data[[metric]], color = factor(REGION6))) +
    geom_point(size = 2, alpha = 0.8) +
    geom_line(aes(group = REGION6), alpha = 0.5) +
    facet_wrap(~sens_type_label, scales = "free_x", nrow = 1) +
    labs(
      x = "Parameter Value",
      y = metric_labels[metric],
      color = "Region"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      strip.text = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )

  if (grepl("diff", metric)) {
    p <- p + geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.5)
  }

  return(p)
}

#' Plot epidemic curves across sensitivity values
#' @param all_data Combined simulation output
#' @param sens_dimension Sensitivity dimension to compare
#' @param region Which region to plot (1-6)
#' @return ggplot object
plot_epidemic_curves <- function(all_data, sens_dimension, region = 1) {

  baseline_parset <- "r0_1.2"

  plot_data <- all_data %>%
    filter((sens_type == sens_dimension | parset_name == baseline_parset) &
           REGION6 == region) %>%
    mutate(
      sens_label = paste0(sens_type, " = ", sens_value)
    )

  p <- plot_data %>%
    ggplot(aes(x = t, y = I_indiv, color = factor(sens_value), linetype = subpop)) +
    geom_line(linewidth = 0.8, alpha = 0.8) +
    labs(
      x = "Time (days)",
      y = "Proportion Infected",
      color = paste0(toupper(sens_dimension), " Value"),
      linetype = "Population",
      title = paste("Epidemic Curves - Region", region)
    ) +
    scale_linetype_manual(
      values = c("A" = "solid", "C" = "dashed"),
      labels = c("A" = "Agricultural", "C" = "Community")
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

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

  # Individual sensitivity dimension plots
  sens_dimensions <- c("r0", "eps", "sar", "fold")

  for (sens_dim in sens_dimensions) {
    # Check if data exists for this dimension
    if (sum(diff_stats$sens_type == sens_dim) > 0) {

      # Attack rate difference plot
      fig_sens <- plot_sensitivity(diff_stats, sens_dim, "attack_rate_diff")
      filename <- paste0("sensitivity_", sens_dim, "_attackrate")
      ggsave(file.path(paths$figures_dir, paste0(filename, ".pdf")), fig_sens, width = 8, height = 5)
      cat("  Saved:", paste0(filename, ".pdf\n"))

      # Epidemic curves for region 1
      fig_curves <- plot_epidemic_curves(all_data, sens_dim, region = 1)
      filename <- paste0("sensitivity_", sens_dim, "_curves_region1")
      ggsave(file.path(paths$figures_dir, paste0(filename, ".pdf")), fig_curves, width = 8, height = 5)
      cat("  Saved:", paste0(filename, ".pdf\n"))
    }
  }

  # Print summary table
  cat("\n", rep("=", 60), "\n", sep = "")
  cat("Summary Statistics by Sensitivity Dimension\n")
  cat(rep("=", 60), "\n\n", sep = "")

  # Mean differential statistics across regions
  summary_table <- diff_stats %>%
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

# Run if called directly
if (sys.nframe() == 0 || !interactive()) {
  results <- run_sensitivity_analysis()
}
