library(tidycensus)
library(dplyr)
library(ggplot2)

# Set year and load API key (if needed)
year <- 2021
readRenviron("~/.Renviron")

# Table B25014 contains counts by occupancy and crowding level
crowding_vars <- c(
  total = "B25014_001",
  crowded_owner_1 = "B25014_005",   # Owner, 1.01 to 1.50 persons/room
  crowded_owner_2 = "B25014_006",   # Owner, 1.51-2.00 persons/room
  crowded_owner_3 = "B25014_007",   # Owner, >2.00 persons/room
  crowded_renter_1 = "B25014_011",  # Renter, 1.01 to 1.50 persons/room
  crowded_renter_2 = "B25014_012",   # Renter, 1.51-2.00 persons/room
  crowded_renter_3 = "B25014_013"   # Renter, >2.00 persons/room
)

# Pull data for counties
acs_crowding <- get_acs(
  geography = "county",
  variables = crowding_vars,
  year = year,
  survey = "acs5",
  output = "wide"
)

# Calculate total crowded households
acs_crowding <- acs_crowding %>%
  mutate(
    crowded = crowded_owner_1E + crowded_owner_2E + crowded_owner_3E +
              crowded_renter_1E + crowded_renter_2E + crowded_renter_3E,
    total = totalE,
    prop_crowded = crowded / total
  ) %>%
  select(GEOID, NAME, total, crowded, prop_crowded)

# Optional: histogram or map


fig_acs_crowding <- ggplot(acs_crowding, aes(x = prop_crowded)) +
  geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
  labs(title = "Distribution of Household Crowding by County",
       x = "Proportion Crowded (>1 person per room)",
       y = "Number of Counties") +
  theme_minimal()
