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
#   - figures/main_relative_infection_{method}.pdf/.png - Relative infection rate (A/C)
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
                           paste0("regional_sim_r0_", default_pars$r0, ".csv"))
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
    # Regional curves: black outline for visibility
    geom_line(
      data = regional_plot,
      aes(x = t, y = I_indiv, group = subpop),
      color = "black",
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
    scale_x_continuous(limits = c(0, 150), breaks = seq(0, 150, 30)) +
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
    theme_classic(base_size = 17) +
    theme(
      legend.position = "bottom",
      strip.text = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(color = "grey40")
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
    # Regional curves: black outline for visibility
    geom_line(
      data = regional_plot,
      aes(x = t, y = R_indiv, group = subpop),
      color = "black",
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
    scale_x_continuous(limits = c(0, 150), breaks = seq(0, 150, 30)) +
    scale_y_continuous(limits = c(0, 1)) +
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
    theme_classic(base_size = 17) +
    theme(
      legend.position = "bottom",
      strip.text = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(color = "grey40")
    ) +
    guides(color = guide_legend(override.aes = list(linewidth = 1.5, alpha = 1)))
}

#' Create overlay plot for relative infection rate (A/C)
#' @param county_plot County data prepared for plotting
#' @param regional_plot Regional data prepared for plotting
#' @param adjust_method Adjustment method name for subtitle
#' @return ggplot object
create_relative_infection_plot <- function(county_plot, regional_plot, adjust_method) {

  # Compute A/C ratio for county data
  county_rel <- county_plot %>%
    select(t, GEOID, subpop, I_indiv, region_label) %>%
    pivot_wider(names_from = subpop, values_from = I_indiv) %>%
    filter(C > 0) %>%
    mutate(rel_inf = A / C)

  # Compute A/C ratio for regional data
  regional_rel <- regional_plot %>%
    select(t, subpop, I_indiv, region_label) %>%
    pivot_wider(names_from = subpop, values_from = I_indiv) %>%
    filter(C > 0) %>%
    mutate(rel_inf = A / C)

  ggplot() +
    # County curves: thin, semi-transparent, in back
    geom_line(
      data = county_rel,
      aes(x = t, y = rel_inf, group = GEOID),
      color = "grey50",
      linewidth = 0.15,
      alpha = 0.3
    ) +
    # Regional curves: black outline for visibility
    geom_line(
      data = regional_rel,
      aes(x = t, y = rel_inf),
      color = "black",
      linewidth = 2.5,
      alpha = 1
    ) +
    # Regional curves: thick, opaque, in front
    geom_line(
      data = regional_rel,
      aes(x = t, y = rel_inf),
      color = "#7B287D",
      linewidth = 1.2,
      alpha = 1
    ) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "grey50", alpha = 0.5) +
    facet_wrap(~region_label, ncol = 3) +
    scale_x_continuous(limits = c(0, 150), breaks = seq(0, 150, 30)) +
    expand_limits(y = 0.5) +
    labs(
      x = "Time (days)",
      y = "Relative Infection Rate (Agricultural / Community)",
      title = paste0("Relative Infection Rate: ", adjust_labels[adjust_method]),
      subtitle = paste0("Thick lines: regional average. Thin lines: individual counties. ",
                        "R0 = ", default_pars$r0)
    ) +
    theme_classic(base_size = 17) +
    theme(
      legend.position = "bottom",
      strip.text = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(color = "grey40")
    )
}

# ==============================================================================
# 4. Generate Figures for Each Adjustment Method
# ==============================================================================

adjust_methods <- c("none", "multiplicative", "additive")

for (method in adjust_methods) {
  cat("\nProcessing adjustment method:", method, "\n")

  # Load county data for this method

  county_file <- file.path(paths$output_dir, paste0("county_sim_", method, ".csv"))
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

  # Create and save relative infection plot (A/C ratio)
  fig_relative <- create_relative_infection_plot(county_plot, regional_plot, method)
  ggsave(file.path(paths$figures_dir, paste0("main_relative_infection_", method, ".pdf")),
         fig_relative, width = 12, height = 8)
  ggsave(file.path(paths$figures_dir, paste0("main_relative_infection_", method, ".png")),
         fig_relative, width = 12, height = 8, dpi = 300)
  cat("  Saved: main_relative_infection_", method, ".pdf/.png\n", sep = "")
}

cat("\nMain overlay/cumulative figures complete.\n")

# ==============================================================================
# 5. County-Level Household Characteristic Histograms
# ==============================================================================

cat("\n", rep("=", 60), "\n", sep = "")
cat("Generating County-Level Household Characteristic Histograms\n")
cat(rep("=", 60), "\n\n", sep = "")

# --- Check that acs_data and naws_data are loaded ---
if (!exists("acs_data") || !exists("naws_data")) {
  cat("  Loading ACS and NAWS data...\n")
  source('code/import_acs.R')
  source('code/import_naws.R')
}

# --- Community stats: summarize acs_data per county ---
community_stats <- acs_data %>%
  group_by(GEOID, REGION6) %>%
  summarise(
    mean_hhsize = sum(hhSize * prop),
    prop_hhsize4plus = sum(prop[hhSize >= 4]),
    prop_crowded = first(prop_crowded),
    .groups = "drop"
  ) %>%
  mutate(subpop = "C")

# --- Pre-split NAWS data by region ---
naws_by_region <- lapply(1:n_regions, function(r) {
  naws_data %>% filter(REGION6 == r)
})

# --- Function to compute imputed ag worker stats for one county ---
compute_ag_stats <- function(geoid, region, hhSize_factor, hhSize_diff,
                             crowded_factor, crowded_diff, method) {
  naws_regional <- naws_by_region[[region]]

  if (method == "multiplicative") {
    processed <- naws_regional %>%
      mutate(prop = prop * hhSize_factor,
             prop = prop / sum(prop),
             prop_crowded = prop_crowded * crowded_factor,
             prop_crowded = case_when(prop_crowded > 1 ~ 1,
                                      prop_crowded < 0 ~ 0,
                                      TRUE ~ prop_crowded))
  } else if (method == "additive") {
    processed <- naws_regional %>%
      mutate(prop = prop + hhSize_diff,
             prop = case_when(prop < 0 ~ 0, TRUE ~ prop),
             prop = prop / sum(prop),
             prop_crowded = prop_crowded + crowded_diff,
             prop_crowded = case_when(prop_crowded > 1 ~ 1,
                                      prop_crowded < 0 ~ 0,
                                      TRUE ~ prop_crowded))
  } else {
    processed <- naws_regional
  }

  tibble(
    GEOID = geoid,
    REGION6 = region,
    mean_hhsize = sum(processed$hhSize * processed$prop),
    prop_hhsize4plus = sum(processed$prop[processed$hhSize >= 4]),
    prop_crowded = first(processed$prop_crowded)
  )
}

# --- Pre-extract county-level adjustment factors ---
county_factors <- acs_data %>%
  arrange(GEOID, hhSize) %>%
  group_by(GEOID, REGION6) %>%
  summarise(
    hhSize_factor = list(hhSize_factor),
    hhSize_diff = list(hhSize_diff),
    crowded_factor = first(crowded_factor),
    crowded_diff = first(crowded_diff),
    .groups = "drop"
  )

# --- Compute ag worker stats for all counties × all methods ---
all_hist_data <- list()

for (method in adjust_methods) {
  cat("  Computing ag worker household stats for method:", method, "\n")

  ag_stats <- purrr::pmap_dfr(
    list(
      geoid = county_factors$GEOID,
      region = county_factors$REGION6,
      hhSize_factor = county_factors$hhSize_factor,
      hhSize_diff = county_factors$hhSize_diff,
      crowded_factor = county_factors$crowded_factor,
      crowded_diff = county_factors$crowded_diff
    ),
    function(geoid, region, hhSize_factor, hhSize_diff,
             crowded_factor, crowded_diff) {
      compute_ag_stats(geoid, region, hhSize_factor, hhSize_diff,
                       crowded_factor, crowded_diff, method)
    }
  ) %>%
    mutate(subpop = "A")

  method_data <- bind_rows(
    community_stats %>% mutate(method = method),
    ag_stats %>% mutate(method = method)
  )
  all_hist_data[[method]] <- method_data
}

hist_data <- bind_rows(all_hist_data) %>%
  left_join(region_map, by = "REGION6") %>%
  mutate(region_label = factor(paste0(REGION_NAME, " (", REGION_ABBREV, ")"),
                               levels = region_order))

# --- Compute NAWS regional reference values for vlines ---
naws_reference <- naws_data %>%
  group_by(REGION6) %>%
  summarise(
    naws_mean_hhsize = sum(hhSize * prop),
    naws_prop_hhsize4plus = sum(prop[hhSize >= 4]),
    naws_prop_crowded = first(prop_crowded),
    .groups = "drop"
  ) %>%
  left_join(region_map, by = "REGION6") %>%
  mutate(region_label = factor(paste0(REGION_NAME, " (", REGION_ABBREV, ")"),
                               levels = region_order))

# --- Compute ACS regional reference values for vlines ---
acs_reference <- community_stats %>%
  group_by(REGION6) %>%
  summarise(
    acs_mean_hhsize = mean(mean_hhsize),
    acs_prop_hhsize4plus = mean(prop_hhsize4plus),
    acs_prop_crowded = mean(prop_crowded),
    .groups = "drop"
  ) %>%
  left_join(region_map, by = "REGION6") %>%
  mutate(region_label = factor(paste0(REGION_NAME, " (", REGION_ABBREV, ")"),
                               levels = region_order))

# --- Plotting functions ---

#' Create household size distribution histograms
#' @param data Combined data frame with method column
#' @param method Adjustment method to filter on
#' @return ggplot object
plot_hhsize_histograms <- function(data, method) {
  df <- data %>% filter(.data$method == .env$method)

  p1 <- ggplot(df, aes(x = mean_hhsize, fill = subpop)) +
    geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
    geom_vline(data = naws_reference, aes(xintercept = naws_mean_hhsize),
               linetype = "dashed", color = "black", linewidth = 0.6) +
    facet_wrap(~region_label, ncol = 3, scales = "free_y") +
    scale_fill_manual(values = pop_colors, labels = pop_labels, name = "Population") +
    labs(x = "Mean Household Size", y = "Number of Counties") +
    theme_classic(base_size = 17) +
    theme(legend.position = "none",
          strip.text = element_text(face = "bold"))

  p2 <- ggplot(df, aes(x = prop_hhsize4plus, fill = subpop)) +
    geom_histogram(position = "identity", alpha = 0.5, binwidth = 0.01) +
    geom_vline(data = naws_reference, aes(xintercept = naws_prop_hhsize4plus),
               linetype = "dashed", color = "black", linewidth = 0.6) +
    facet_wrap(~region_label, ncol = 3, scales = "free_y") +
    scale_x_continuous(limits = c(0, 1)) +
    scale_fill_manual(values = pop_colors, labels = pop_labels, name = "Population") +
    labs(x = "Proportion of Households Size 4+", y = "Number of Counties") +
    theme_classic(base_size = 17) +
    theme(legend.position = "bottom",
          strip.text = element_text(face = "bold"))

  patchwork::wrap_plots(p1, p2, ncol = 1) +
    patchwork::plot_annotation(
      title = paste0("Household Size Distributions: ", adjust_labels[method]),
      theme = theme(plot.title = element_text(face = "bold"))
    )
}

#' Create crowding distribution histograms
#' @param data Combined data frame with method column
#' @param method Adjustment method to filter on
#' @return ggplot object
plot_crowding_histograms <- function(data, method) {
  df <- data %>% filter(.data$method == .env$method)

  ggplot(df, aes(x = prop_crowded, fill = subpop)) +
    geom_histogram(position = "identity", alpha = 0.5, binwidth = 0.01) +
    geom_vline(data = acs_reference, aes(xintercept = acs_prop_crowded),
               linetype = "dashed", color = "#E41A1C", linewidth = 0.6) +
    geom_vline(data = naws_reference, aes(xintercept = naws_prop_crowded),
               linetype = "dashed", color = "#377EB8", linewidth = 0.6) +
    facet_wrap(~region_label, ncol = 3, scales = "free_y") +
    scale_x_continuous(limits = c(0, 1)) +
    scale_fill_manual(values = pop_colors, labels = pop_labels, name = "Population") +
    labs(
      x = "Proportion of Households Crowded",
      y = "Number of Counties",
      title = paste0("Crowding Distributions: ", adjust_labels[method])
    ) +
    theme_classic(base_size = 17) +
    theme(
      legend.position = "bottom",
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold")
    )
}

# --- Generate and save figures ---
for (method in adjust_methods) {
  cat("  Saving histogram figures for method:", method, "\n")

  fig_hh <- plot_hhsize_histograms(hist_data, method)
  ggsave(file.path(paths$figures_dir, paste0("hhsize_distribution_", method, ".pdf")),
         fig_hh, width = 12, height = 10)
  ggsave(file.path(paths$figures_dir, paste0("hhsize_distribution_", method, ".png")),
         fig_hh, width = 12, height = 10, dpi = 300)

  fig_crowd <- plot_crowding_histograms(hist_data, method)
  ggsave(file.path(paths$figures_dir, paste0("crowding_distribution_", method, ".pdf")),
         fig_crowd, width = 12, height = 6)
  ggsave(file.path(paths$figures_dir, paste0("crowding_distribution_", method, ".png")),
         fig_crowd, width = 12, height = 6, dpi = 300)
}

cat("\nAll figures complete.\n")
