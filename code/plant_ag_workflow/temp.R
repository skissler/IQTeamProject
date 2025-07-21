library(tidyverse)
library(lubridate)

# Example: CA strawberries in Monterey County, using estimated labor hours per month

# Parameters
total_acres <- 8000  # from USDA Quick Stats
labor_hours_per_acre <- 250  # from ag econ estimates (varies by crop)
harvest_months <- c(3, 4, 5, 6, 7)  # March to July

# Define monthly weights (relative intensity of labor demand)
# You can customize this using NASS Crop Progress or UC Extension calendars
monthly_weights <- tibble(
  month = 1:12,
  weight = c(0, 0, 0.15, 0.25, 0.3, 0.2, 0.1, 0, 0, 0, 0, 0)
)

# Calculate labor demand curve
labor_curve <- monthly_weights %>%
  mutate(
    crop = "Strawberries",
    county = "Monterey",
    labor_hours = weight * labor_hours_per_acre * total_acres
  )

# Plot
ggplot(labor_curve, aes(x = month, y = labor_hours)) +
  geom_col(fill = "darkred") +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(
    title = "Estimated Monthly Labor Demand: Strawberries (Monterey County)",
    y = "Labor Hours",
    x = "Month"
  )
