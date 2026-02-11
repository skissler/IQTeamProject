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

data_regional <- bind_rows(
  mutate(select(naws_data, REGION6, hhSize, prop), SOURCE = "NAWS"),
  mutate(select(acs_data_regional, REGION6, hhSize, prop), SOURCE = "ACS")
)

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
  group_by(REGION6, SOURCE) %>%
  summarise(mean_hhSize = sum(temp), .groups = "drop") %>%
  mutate(lineend = case_when(SOURCE == "NAWS" ~ Inf, TRUE ~ -Inf))

source_colors <- c("ACS" = "#3b82f6", "NAWS" = "#16a34a")

fig_hhsize_pyramid <- ggplot(data_modified, aes(x = hhSize, y = plot_prop, fill = SOURCE)) +
  geom_col(alpha = 0.8) +
  coord_flip() +
  geom_segment(data = data_means, aes(x = mean_hhSize, xend = mean_hhSize, y = 0, yend = lineend, col = SOURCE),
               lty = "dashed") +
  facet_wrap(~REGION6, labeller = label_both) +
  scale_y_continuous(
    labels = function(x) paste0(abs(x) * 100, "%"),
    breaks = pretty_breaks(n = 5)
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
  ggplot(aes(x = prop_crowded)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50) +
  geom_vline(data = naws_crowding, aes(xintercept = prop_crowded), lty = "dashed") +
  facet_wrap(~REGION6) +
  labs(
    x = "Proportion of Households Crowded",
    y = "Density"
  ) +
  theme_minimal()

ggsave(file.path(paths$figures_dir, "fig_crowding_hists.pdf"),
       fig_crowding_hists, width = 10, height = 6)
