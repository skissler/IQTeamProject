# Optional visualization
library(ggplot2)
library(readr)

results <- read_csv("results/posterior_summaries.csv")

ggplot(results, aes(x = ag_crowded_median, y = ag_kids_median)) +
  geom_point(alpha = 0.5) +
  labs(title = "Estimated Ag Worker Household Characteristics",
       x = "Crowding (Median Estimate)",
       y = "Households with Children (Median Estimate)")