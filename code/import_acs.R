# //////////////////////////////////////////////////////////////////////////////
# Import packages
# //////////////////////////////////////////////////////////////////////////////

library(tidyverse)
library(tidycensus)

# Load Census key
readRenviron("~/.Renviron")
year <- 2022

# //////////////////////////////////////////////////////////////////////////////
# Household sizes 
# //////////////////////////////////////////////////////////////////////////////

# Table B11016 contains counts by household size for households consisting of relatives (note that households of size 1 are categorized as consisting of non-relatives, thus why the numbering is out of order... weird). 
hhSize_vars <- c(
  total = "B11016_001",   # Total households
  hh2fm = "B11016_003",   # Households size 2, family
  hh3fm = "B11016_004",   # Households size 3, family
  hh4fm = "B11016_005",   # Households size 4, family
  hh5fm = "B11016_006",   # Households size 5, family
  hh6fm = "B11016_007",   # Households size 6, family
  hh7fm = "B11016_008",   # Households size 7+, family
  hh1nf = "B11016_010",   # Households size 1
  hh2nf = "B11016_011",   # Households size 2, non-family
  hh3nf = "B11016_012",   # Households size 3, non-family
  hh4nf = "B11016_013",   # Households size 4, non-family
  hh5nf = "B11016_014",   # Households size 5, non-family
  hh6nf = "B11016_015",   # Households size 6, non-family
  hh7nf = "B11016_016"    # Households size 7, non-family
)

# Extract county-level acs data for 2018-2022
acs_hhsize <- get_acs(
    geography = "county",
    variables = hhSize_vars,
    year = year,
    survey = "acs5",
    output = "wide")

# Calculate household size proportions
acs_hhsize <- acs_hhsize %>%
  mutate(
    hh1 = hh1nfE,
    hh2 = hh2fmE + hh2nfE, 
    hh3 = hh3fmE + hh3nfE, 
    hh4 = hh4fmE + hh4nfE, 
    hh5 = hh5fmE + hh5nfE, 
    hh6 = hh6fmE + hh6nfE, 
    hh7 = hh7fmE + hh7nfE) %>% 
  mutate(
    prop1 = hh1/totalE,
    prop2 = hh2/totalE,
    prop3 = hh3/totalE,
    prop4 = hh4/totalE,
    prop5 = hh5/totalE,
    prop6 = hh6/totalE,
    prop7 = hh7/totalE) %>% 
  select(GEOID, NAME, prop1, prop2, prop3, prop4, prop5, prop6, prop7) %>% 
  pivot_longer(-c("GEOID","NAME")) %>% 
  mutate(hhSize=substr(name, 5, 5)) %>% 
  select(GEOID, NAME, hhSize, prop=value)

# //////////////////////////////////////////////////////////////////////////////
# Crowding
# //////////////////////////////////////////////////////////////////////////////

# Table B25014 contains counts by occupancy type and crowding level.
crowding_vars <- c(
  total = "B25014_001",             # Total households
  crowded_owner_1 = "B25014_005",   # Owner, 1.01 to 1.50 persons/room
  crowded_owner_2 = "B25014_006",   # Owner, 1.51-2.00 persons/room
  crowded_owner_3 = "B25014_007",   # Owner, >2.00 persons/room
  crowded_renter_1 = "B25014_011",  # Renter, 1.01 to 1.50 persons/room
  crowded_renter_2 = "B25014_012",  # Renter, 1.51-2.00 persons/room
  crowded_renter_3 = "B25014_013"   # Renter, >2.00 persons/room
)

# Extract county-level acs data for 2018-2022
acs_crowding <- get_acs(
  geography = "county",
  variables = crowding_vars,
  year = year,
  survey = "acs5",
  output = "wide")

# Calculate total crowded households
acs_crowding <- acs_crowding %>%
  mutate(
    crowded = crowded_owner_1E + crowded_owner_2E + crowded_owner_3E +
              crowded_renter_1E + crowded_renter_2E + crowded_renter_3E,
    total = totalE,
    prop_crowded = crowded / total) %>%
  select(GEOID, NAME, prop_crowded)

# //////////////////////////////////////////////////////////////////////////////
# Agricultural workers
# //////////////////////////////////////////////////////////////////////////////

# Pull agricultural workers and total employment by county
ag_vars <- c(
  total_employed = "C24030_001",   # Total number of people employed
  ag_workers_male = "C24030_004",  # 
  ag_workers_female = "C24030_031"
)

# Extract county-level acs data for 2018-2022
acs_ag <- get_acs(
  geography = "county",
  variables = ag_vars,
  year = year,
  survey = "acs5",
  output = "wide")

# Calculate percent employed in agriculture
acs_ag <- acs_ag %>%
  mutate(
    prop_ag_workers = (ag_workers_maleE + ag_workers_femaleE) / total_employedE
  ) %>%
  select(GEOID, NAME, prop_ag_workers)

# //////////////////////////////////////////////////////////////////////////////
# Population sizes
# //////////////////////////////////////////////////////////////////////////////

# Get total population (B01003) for all counties
acs_pop <- get_acs(
  geography = "county",
  variables = "B01003_001",  # total population
  year = 2022,
  survey = "acs5")

# Clean up the result
acs_pop <- acs_pop %>%
  select(GEOID, NAME, population = estimate)

# //////////////////////////////////////////////////////////////////////////////
# Combine
# //////////////////////////////////////////////////////////////////////////////

# The following data frame contains: 
# GEOID: numeric county identifier
# NAME: name of the county
# hhSize: the household size reflected in the 'prop' column
# prop: proportion of households in county of size "hhSize" 
# prop_crowded: proportion of households in county that have >1 occupant/room
# prop_ag_workers: proportion of employed adults employed in agriculture
# population: county population size

acs_data <- acs_hhsize %>% 
  left_join(acs_crowding, by=c("GEOID","NAME")) %>% 
  left_join(acs_ag, by=c("GEOID","NAME")) %>% 
  left_join(acs_pop, by=c("GEOID","NAME")) 
