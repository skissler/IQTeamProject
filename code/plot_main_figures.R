# ==============================================================================
# plot_main_figures.R - Main Publication Figures
# ==============================================================================
# Creates overlay plots comparing regional and county-level epidemic simulations.
# Regional curves shown as thick lines in front; county curves as thin,
# semi-transparent lines in the background.
#
# Generates figures for all three county adjustment methods:
#   - none: Use regional NAWS data directly
#   - multiplicative: Multiply NAWS by (county_ACS / regional_ACS_mean)
#   - additive: Add (county_ACS - regional_ACS_mean) to NAWS
#
# Outputs (for each adjustment method):
#   - figures/main_overlay_{method}.pdf/.png - Current infections
#   - figures/main_cumulative_{method}.pdf/.png - Cumulative infections
# ==============================================================================

# Load dependencies
if (!exists("paths")) {
  source('code/setup.R')
}

cat("\n", rep("=", 60), "\n", sep = "")
cat("Generating Main Publication Figures\n")
cat(rep("=", 60), "\n\n", sep = "")

# ==============================================================================
# 1. Load Regional Data
# ==============================================================================

cat("Loading regional simulation data...\n")

# Load baseline regional simulation
regional_file <- file.path(paths$output_dir,
                           paste0("epidf_indiv_full_regional_r0_", default_pars$r0, ".csv"))
if (!file.exists(regional_file)) {
  stop("Regional simulation file not found: ", regional_file)
}
regional_data <- read_csv(regional_file, show_col_types = FALSE)
cat("  Regional data:", nrow(regional_data), "rows\n")

# ==============================================================================
# 2. Prepare Regional Data for Plotting
# ==============================================================================

# Add region labels (ordered by REGION6, not alphabetically)
region_order <- region_map %>%
  mutate(region_label = paste0(REGION_NAME, " (", REGION_ABBREV, ")")) %>%
  pull(region_label)

regional_plot <- regional_data %>%
  left_join(region_map, by = "REGION6") %>%
  mutate(region_label = factor(paste0(REGION_NAME, " (", REGION_ABBREV, ")"),
                               levels = region_order))

# ==============================================================================
# 3. Define Plotting Functions
# ==============================================================================

# Population labels and colors
pop_labels <- c("A" = "Agricultural Workers", "C" = "Community")
pop_colors <- c("A" = "#377EB8", "C" = "#E41A1C")  # A = blue, C = red

# Adjustment method labels for titles
adjust_labels <- c(
  "none" = "No County Adjustment",
  "multiplicative" = "Multiplicative Adjustment",
  "additive" = "Additive Adjustment"
)

#' Create overlay plot for current infections (I_indiv)
#' @param county_plot County data prepared for plotting
#' @param regional_plot Regional data prepared for plotting
#' @param adjust_method Adjustment method name for subtitle
#' @return ggplot object
create_overlay_plot <- function(county_plot, regional_plot, adjust_method) {
  ggplot() +
    # County curves: thin, semi-transparent, colored, in back
    geom_line(
      data = county_plot,
      aes(x = t, y = I_indiv, group = interaction(GEOID, subpop), color = subpop),
      linewidth = 0.15,
      alpha = 0.3
    ) +
    # Regional curves: white outline for visibility
    geom_line(
      data = regional_plot,
      aes(x = t, y = I_indiv, group = subpop),
      color = "white",
      linewidth = 2.5,
      alpha = 1
    ) +
    # Regional curves: thick, opaque, in front
    geom_line(
      data = regional_plot,
      aes(x = t, y = I_indiv, color = subpop),
      linewidth = 1.2,
      alpha = 1
    ) +
    facet_wrap(~region_label, ncol = 3) +
    scale_color_manual(
      values = pop_colors,
      labels = pop_labels,
      name = "Population"
    ) +
    labs(
      x = "Time (days)",
      y = "Proportion Infected",
      title = paste0("Epidemic Dynamics: ", adjust_labels[adjust_method]),
      subtitle = paste0("Thick lines: regional average. Thin lines: individual counties. ",
                        "R0 = ", default_pars$r0)
    ) +
    theme_classic() +
    theme(
      legend.position = "bottom",
      strip.text = element_text(face = "bold", size = 10),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 9, color = "grey40")
    ) +
    guides(color = guide_legend(override.aes = list(linewidth = 1.5, alpha = 1)))
}

#' Create overlay plot for cumulative infections (R_indiv)
#' @param county_plot County data prepared for plotting
#' @param regional_plot Regional data prepared for plotting
#' @param adjust_method Adjustment method name for subtitle
#' @return ggplot object
create_cumulative_plot <- function(county_plot, regional_plot, adjust_method) {
  ggplot() +
    # County curves: thin, semi-transparent, colored, in back
    geom_line(
      data = county_plot,
      aes(x = t, y = R_indiv, group = interaction(GEOID, subpop), color = subpop),
      linewidth = 0.15,
      alpha = 0.3
    ) +
    # Regional curves: white outline for visibility
    geom_line(
      data = regional_plot,
      aes(x = t, y = R_indiv, group = subpop),
      color = "white",
      linewidth = 2.5,
      alpha = 1
    ) +
    # Regional curves: thick, opaque, in front
    geom_line(
      data = regional_plot,
      aes(x = t, y = R_indiv, color = subpop),
      linewidth = 1.2,
      alpha = 1
    ) +
    facet_wrap(~region_label, ncol = 3) +
    scale_color_manual(
      values = pop_colors,
      labels = pop_labels,
      name = "Population"
    ) +
    labs(
      x = "Time (days)",
      y = "Cumulative Proportion Infected",
      title = paste0("Cumulative Infections: ", adjust_labels[adjust_method]),
      subtitle = paste0("Thick lines: regional average. Thin lines: individual counties. ",
                        "R0 = ", default_pars$r0)
    ) +
    theme_classic() +
    theme(
      legend.position = "bottom",
      strip.text = element_text(face = "bold", size = 10),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 9, color = "grey40")
    ) +
    guides(color = guide_legend(override.aes = list(linewidth = 1.5, alpha = 1)))
}

# ==============================================================================
# 4. Generate Figures for Each Adjustment Method
# ==============================================================================

adjust_methods <- c("none", "multiplicative", "additive")

for (method in adjust_methods) {
  cat("\nProcessing adjustment method:", method, "\n")

  # Load county data for this method

  county_file <- file.path(paths$output_dir, paste0("epidf_indiv_county_", method, ".csv"))
  if (!file.exists(county_file)) {
    cat("  WARNING: County file not found, skipping:", county_file, "\n")
    next
  }
  county_data <- read_csv(county_file, show_col_types = FALSE)
  cat("  Loaded county data:", nrow(county_data), "rows\n")

  # Prepare county data for plotting
  county_plot <- county_data %>%
    left_join(region_map, by = "REGION6") %>%
    mutate(region_label = factor(paste0(REGION_NAME, " (", REGION_ABBREV, ")"),
                                 levels = region_order))

  # Create and save overlay plot (current infections)
  fig_overlay <- create_overlay_plot(county_plot, regional_plot, method)
  ggsave(file.path(paths$figures_dir, paste0("main_overlay_", method, ".pdf")),
         fig_overlay, width = 12, height = 8)
  ggsave(file.path(paths$figures_dir, paste0("main_overlay_", method, ".png")),
         fig_overlay, width = 12, height = 8, dpi = 300)
  cat("  Saved: main_overlay_", method, ".pdf/.png\n", sep = "")

  # Create and save cumulative plot
  fig_cumulative <- create_cumulative_plot(county_plot, regional_plot, method)
  ggsave(file.path(paths$figures_dir, paste0("main_cumulative_", method, ".pdf")),
         fig_cumulative, width = 12, height = 8)
  ggsave(file.path(paths$figures_dir, paste0("main_cumulative_", method, ".png")),
         fig_cumulative, width = 12, height = 8, dpi = 300)
  cat("  Saved: main_cumulative_", method, ".pdf/.png\n", sep = "")
}

cat("\nMain figures complete.\n")
