library(tidycensus)
library(dplyr)

# Load your Census API key if not already set
# census_api_key("YOUR_KEY", install = TRUE)
readRenviron("~/.Renviron")

# Get total population (B01003) for all counties
pop_data <- get_acs(
  geography = "county",
  variables = "B01003_001",  # total population
  year = 2021,
  survey = "acs5"
)

# Clean up the result
pop_data_clean <- pop_data %>%
  select(GEOID, NAME, population = estimate)

# View example
head(pop_data_clean)
