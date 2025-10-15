
# To create the side-by-side histogram effect, we will make the proportions
# for one of the sources (ACS) negative. This will plot its bars to the left
# of the central axis.
data_modified <- data_regional %>%
  mutate(
    # Create a new column for plotting. If the SOURCE is 'ACS', make the proportion negative.
    plot_prop = ifelse(SOURCE == "ACS", -prop, prop),
    # Convert household size to a factor for discrete axis labeling
    hhSize = as.factor(hhSize)
  )

data_means <- data_modified %>% 
  mutate(temp = as.numeric(hhSize)*prop) %>% 
  group_by(REGION6, SOURCE) %>% 
  summarise(prop=sum(temp)) %>% 
  mutate(lineend=case_when(SOURCE=="NAWS"~Inf, TRUE~-Inf)) %>% 
  ungroup() 

# Define a custom color palette for the sources
source_colors <- c("ACS" = "#3b82f6", "NAWS" = "#16a34a")

# Generate the plot
fig_hhsize_pyramid <- ggplot(data_modified, aes(x = hhSize, y = plot_prop, fill = SOURCE)) +
  # Use geom_col() as we are plotting pre-calculated proportions (stat = "identity")
  geom_col(alpha=0.8) +
  # Flip the coordinates to make the bars vertical (y-axis becomes horizontal)
  coord_flip() +
  geom_segment(data=data_means, aes(x=prop, xend=prop, y=0, yend=lineend, col=SOURCE), lty="dashed") + 
  # Create a separate plot for each region defined in REGION6
  facet_wrap(~ REGION6, labeller = label_both) +
  # Format the x-axis (originally y-axis) labels to show absolute percentages
  scale_y_continuous(
    labels = function(x) paste0(abs(x) * 100, "%"),
    breaks = pretty_breaks(n = 5)
  ) +
  # Apply the custom color palette
  scale_fill_manual(values = source_colors) +
  scale_color_manual(values = source_colors) +
  # Add informative labels and a title
  labs(
    x = "Household Size",
    y = "Proportion",
    fill = "Data Source",
    col = "Data Source",
  ) +
  # Apply a clean, minimal theme
  theme_minimal(base_size = 12) +
  # Customize theme elements for better readability
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 13, margin = margin(b = 20)),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.major.y = element_blank(), # Remove horizontal grid lines
    axis.ticks = element_line(color = "grey80")
  )

# To save the plot to a file, you can uncomment the following line:
# ggsave("household_size_pyramid_plot.png", plot = pyramid_plot, width = 10, height = 8, dpi = 300)

ggsave(fig_hhsize_pyramid, file="figures/fig_hhsize_pyramid.pdf")