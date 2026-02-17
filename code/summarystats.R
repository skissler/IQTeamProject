# code/summarystats.R
# Summary statistics and descriptive figures for household size and crowding data
#
# Compares ACS (county-level) and NAWS (regional) household size distributions
# and crowding rates. Produces two figures:
#   - figures/fig_hhsize_pyramid.pdf: Back-to-back histogram of household sizes
#   - figures/fig_crowding_hists.pdf: County crowding distributions with NAWS reference lines
#
# Prerequisites: setup.R, import_naws.R, import_acs.R

# ==============================================================================
# Setup
# ==============================================================================

if (!exists("paths")) source("code/setup.R")
if (!exists("naws_data")) source("code/import_naws.R")
if (!exists("acs_data_regional")) source("code/import_acs.R")

# ==============================================================================
# Combine regional data
# ==============================================================================

# Create ordered factor for region labels (preserves numeric order, shows names)
region_labels <- setNames(region_map$REGION_NAME, region_map$REGION6)

data_regional <- bind_rows(
  mutate(select(naws_data, REGION6, hhSize, prop), SOURCE = "NAWS"),
  mutate(select(acs_data_regional, REGION6, hhSize, prop), SOURCE = "ACS")
) %>%
  mutate(region_label = factor(region_labels[as.character(REGION6)],
                               levels = region_map$REGION_NAME))

# ==============================================================================
# Figure 1: Household size pyramid (ACS vs NAWS by region)
# ==============================================================================

# Mirror ACS proportions to create back-to-back histogram
data_modified <- data_regional %>%
  mutate(
    plot_prop = ifelse(SOURCE == "ACS", -prop, prop),
    hhSize = as.factor(hhSize)
  )

# Mean household size per region and source (for reference lines)
data_means <- data_modified %>%
  mutate(temp = as.numeric(hhSize) * prop) %>%
  group_by(region_label, SOURCE) %>%
  summarise(mean_hhSize = sum(temp), .groups = "drop") %>%
  mutate(lineend = case_when(SOURCE == "NAWS" ~ Inf, TRUE ~ -Inf))

source_colors <- c("ACS" = "#E41A1C", "NAWS" = "#377EB8")  # ACS = community (red), NAWS = ag workers (blue)

fig_hhsize_pyramid <- ggplot(data_modified, aes(x = hhSize, y = plot_prop, fill = SOURCE)) +
  geom_col(alpha = 0.8) +
  coord_flip() +
  geom_segment(data = data_means, aes(x = mean_hhSize, xend = mean_hhSize, y = 0, yend = lineend, col = SOURCE),
               lty = "dashed") +
  facet_wrap(~region_label) +
  scale_y_continuous(
    labels = function(x) paste0(abs(x) * 100, "%"),
    breaks = scales::pretty_breaks(n = 5)
  ) +
  scale_fill_manual(values = source_colors) +
  scale_color_manual(values = source_colors) +
  labs(
    x = "Household Size",
    y = "Proportion",
    fill = "Data Source",
    col = "Data Source"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    axis.ticks = element_line(color = "grey80")
  )

ggsave(file.path(paths$figures_dir, "fig_hhsize_pyramid.pdf"),
       fig_hhsize_pyramid, width = 10, height = 8)

# ==============================================================================
# Figure 2: County crowding distributions with NAWS reference lines
# ==============================================================================

fig_crowding_hists <- acs_data %>%
  mutate(region_label = factor(region_labels[as.character(REGION6)],
                               levels = region_map$REGION_NAME)) %>%
  ggplot(aes(x = prop_crowded)) +
  geom_histogram(aes(y = after_stat(density)), fill = "#E41A1C", alpha = 0.7, bins = 50) +
  geom_vline(data = acs_data_regional %>%
               group_by(REGION6) %>%
               summarise(prop_crowded = first(prop_crowded), .groups = "drop") %>%
               mutate(region_label = factor(region_labels[as.character(REGION6)],
                                            levels = region_map$REGION_NAME)),
             aes(xintercept = prop_crowded), lty = "dashed", color = "#E41A1C", linewidth = 0.8) +
  geom_vline(data = naws_crowding %>%
               mutate(region_label = factor(region_labels[as.character(REGION6)],
                                            levels = region_map$REGION_NAME)),
             aes(xintercept = prop_crowded), lty = "dashed", color = "#377EB8", linewidth = 0.8) +
  facet_wrap(~region_label) +
  labs(
    x = "Proportion of Households Crowded",
    y = "Density"
  ) +
  theme_minimal()

ggsave(file.path(paths$figures_dir, "fig_crowding_hists.pdf"),
       fig_crowding_hists, width = 10, height = 6)

# ==============================================================================
# Household summary statistics by region and population
# ==============================================================================

# Agricultural workers (NAWS)
naws_summary <- naws_data %>%
  group_by(REGION6, REGION_NAME) %>%
  summarise(
    mean_hhsize = sum(hhSize * prop),
    prop_hhsize_4plus = sum(prop[hhSize >= 4]),
    prop_crowded = first(prop_crowded),
    .groups = "drop"
  ) %>%
  mutate(population = "Agricultural workers")

# General community (ACS, regional)
acs_summary <- acs_data_regional %>%
  group_by(REGION6) %>%
  summarise(
    mean_hhsize = sum(hhSize * prop),
    prop_hhsize_4plus = sum(prop[hhSize >= 4]),
    prop_crowded = first(prop_crowded),
    .groups = "drop"
  ) %>%
  mutate(
    REGION_NAME = region_labels[as.character(REGION6)],
    population = "General community"
  )

hh_summary <- bind_rows(naws_summary, acs_summary) %>%
  select(REGION6, REGION_NAME, population, mean_hhsize, prop_hhsize_4plus, prop_crowded) %>%
  arrange(REGION6, population)

write_csv(hh_summary, file.path(paths$output_dir, "hh_summary.csv"))

cat("Household summary statistics saved to", file.path(paths$output_dir, "hh_summary.csv"), "\n")

# ==============================================================================
# Mixing matrix elements by region and assortativity (eta)
# ==============================================================================

# Extract agricultural worker population fractions by region
w_A_by_region <- acs_data_regional %>%
  group_by(REGION6) %>%
  summarise(w_A = first(prop_ag_workers), .groups = "drop") %>%
  mutate(region_label = region_labels[as.character(REGION6)])

# Define eta values (sensitivity analysis range + baseline)
eta_values <- c(0, 1/4, 1/3, 1/2, 2/3, 3/4)

# Compute mixing matrix elements for each region x eta combination
mixing_matrix_table <- expand_grid(
  w_A_by_region,
  eta = eta_values
) %>%
  mutate(
    w_C = 1 - w_A,
    m_AA = eta + (1 - eta) * w_A,
    m_AC = (1 - eta) * w_C,
    m_CC = eta + (1 - eta) * w_C,
    m_CA = (1 - eta) * w_A
  ) %>%
  select(region_label, REGION6, w_A, eta, m_AA, m_AC, m_CC, m_CA)

# Save to output
write_csv(mixing_matrix_table, file.path(paths$output_dir, "mixing_matrix_table.csv"))

cat("Mixing matrix table saved to", file.path(paths$output_dir, "mixing_matrix_table.csv"), "\n")
