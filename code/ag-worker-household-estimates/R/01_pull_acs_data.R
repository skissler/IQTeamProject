# Load packages
library(tidycensus)
library(dplyr)
library(tidyr)
library(stringr)

# Load API key
# census_api_key("YOUR_CENSUS_API_KEY", install = TRUE, overwrite = TRUE)
readRenviron("~/.Renviron")

year <- 2021
geo <- "county"

# Household crowding
crowding_vars <- c(total = "B25014_001", crowded = "B25014_005")
crowding <- get_acs(geography = geo, variables = crowding_vars, year = year, survey = "acs5", output = "wide") %>%
  transmute(GEOID, NAME, prop_crowded = B25014_005E / B25014_001E)

# Households with kids under 18
children <- get_acs(geography = geo, variables = c(total_households = "B11005_001", with_kids = "B11005_002"),
                    year = year, survey = "acs5", output = "wide") %>%
  transmute(GEOID, prop_with_kids = B11005_002E / B11005_001E)

# Employment in agriculture
employment <- get_acs(geography = geo, table = "C24030", year = year, survey = "acs5") %>%
  filter(variable %in% c("C24030_001", "C24030_002")) %>%
  mutate(category = case_when(variable == "C24030_001" ~ "total", variable == "C24030_002" ~ "agriculture")) %>%
  select(GEOID, NAME, category, estimate) %>%
  pivot_wider(names_from = category, values_from = estimate) %>%
  mutate(prop_ag = agriculture / total, prop_nonag = 1 - prop_ag)

# Merge all
final_data <- crowding %>%
  left_join(children, by = "GEOID") %>%
  left_join(employment %>% select(GEOID, prop_ag, prop_nonag), by = "GEOID") %>%
  mutate(state_abbr = str_extract(NAME, ", [A-Z]{2}$") %>% str_remove(", "))

# Assign NAWS region
get_region <- function(state_abbr) {
  if (state_abbr %in% c("CT", "ME", "MA", "NH", "NJ", "NY", "PA", "RI", "VT")) return("Northeast")
  if (state_abbr %in% c("AL", "AR", "FL", "GA", "KY", "LA", "MS", "NC", "SC", "TN", "VA", "WV")) return("Southeast")
  if (state_abbr %in% c("IL", "IN", "IA", "KS", "MI", "MN", "MO", "NE", "ND", "OH", "OK", "SD", "WI")) return("Midwest")
  if (state_abbr %in% c("AZ", "NM", "TX")) return("Southwest")
  if (state_abbr %in% c("CA", "CO", "ID", "MT", "NV", "OR", "UT", "WA", "WY")) return("West")
  return(NA)
}

final_data$region <- sapply(final_data$state_abbr, get_region)

# Save processed data
saveRDS(final_data, file = "data/processed/final_data.rds")