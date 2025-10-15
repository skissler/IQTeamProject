# //////////////////////////////////////////////////////////////////////////////
# Import packages
# //////////////////////////////////////////////////////////////////////////////

library(tidyverse)
library(tidycensus)
source('code/utils.R')

# Load Census key
readRenviron("~/.Renviron")
year <- 2022

stateregion <- read_csv("data/stateregion.csv")
stateabbrev <- read_csv("data/stateabbrev.csv")

# Download county geometries (2023 by default)
counties_sf <- counties(cb = TRUE, year = 2022)

# Extract GEOID-to-state mapping
county_lookup <- counties_sf %>%
  select(GEOID, STATEFP, STUSPS, STATE_NAME)  %>% 
  inner_join(stateregion, by="STATE_NAME") 

GEOID_vec <- sort(unique(county_lookup$GEOID))

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
  mutate(hhSize=as.numeric(substr(name, 5, 5))) %>% 
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

# Append the state and region: 
acs_data <- acs_data %>% 
  inner_join(st_drop_geometry(select(county_lookup, GEOID, STUSPS, STATE_NAME, REGION6)), by="GEOID")

# //////////////////////////////////////////////////////////////////////////////
# Append deviations from the mean
# //////////////////////////////////////////////////////////////////////////////

mean_hhSize_dist <- acs_data %>% 
  mutate(propwt=prop*population) %>% 
  group_by(hhSize, REGION6) %>% 
  summarise(propwt=sum(propwt), population=sum(population)) %>% 
  mutate(propwt=propwt/population) %>% 
  group_by(REGION6) %>% 
  mutate(propwt=propwt/sum(propwt)) %>% 
  select(REGION6, hhSize, prop_mean=propwt) %>% 
  arrange(REGION6, hhSize)

mean_prop_crowded <- acs_data %>% 
  mutate(prop_crowded_wt=prop_crowded*population) %>% 
  group_by(REGION6) %>% 
  summarise(prop_crowded_wt=sum(prop_crowded_wt), population=sum(population)) %>% 
  mutate(prop_crowded_wt = prop_crowded_wt/population) %>% 
  select(REGION6, prop_crowded_mean=prop_crowded_wt)

acs_data <- acs_data %>% 
  left_join(mean_hhSize_dist, by=c("REGION6","hhSize")) %>% 
  left_join(mean_prop_crowded, by="REGION6") %>% 
  mutate(hhSize_factor=prop/prop_mean) %>% 
  mutate(crowded_factor=prop_crowded/prop_crowded_mean) %>% 
  select(-prop_mean, -prop_crowded_mean)

# //////////////////////////////////////////////////////////////////////////////
# Append rural/urban designation
# //////////////////////////////////////////////////////////////////////////////

# vars_dhc_2020 <-  load_variables(2020, "dhc")

# Load P2 urban-rural data from 2020 Census
urban_rural <- get_decennial(
  geography = "county",
  table = "P2",
  year = 2020) %>%
  select(GEOID, variable, value) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(
    prop_urban = P2_002N / P2_001N,
    prop_rural = P2_003N / P2_001N,
    urban_rural_category = case_when(
      prop_urban >= .5 ~ "Urban-majority",
      prop_urban < .5 ~ "Rural-majority"
    )
  ) %>% 
  select(GEOID, prop_urban, prop_rural, urban_rural_category)
  
acs_data <- acs_data %>% 
  left_join(select(urban_rural, GEOID, prop_rural), by="GEOID") %>% 
  ungroup() 

# //////////////////////////////////////////////////////////////////////////////
# Generate regional summaries for the acs
# //////////////////////////////////////////////////////////////////////////////

acs_data_regional <- acs_data %>% 
  group_by(REGION6, hhSize) %>% 
  mutate(
    prop=prop*population, 
    prop_crowded=prop_crowded*population,
    prop_ag_workers=prop_ag_workers*population) %>% 
  summarise(
    prop=sum(prop), 
    prop_crowded=sum(prop_crowded), 
    prop_ag_workers=sum(prop_ag_workers), 
    population=sum(population)) %>% 
  mutate(
    prop=prop/population,
    prop_crowded=prop_crowded/population,
    prop_ag_workers=prop_ag_workers/population) %>% 
  ungroup() 

# mean_hhSize_dist <- acs_data %>% 
#   group_by(hhSize) %>% 
#   mutate(propwt=prop*population) %>% 
#   summarise(propwt=sum(propwt), population=sum(population)) %>% 
#   mutate(propwt=propwt/population) %>% 
#   ungroup() %>% 
#   mutate(propwt=propwt/sum(propwt)) %>% 
#   select(hhSize, prop_mean=propwt)

# mean_prop_crowded <- acs_data %>% 
#   mutate(prop_crowded_wt=prop_crowded*population) %>% 
#   summarise(prop_crowded_wt=sum(prop_crowded_wt), population=sum(population)) %>% 
#   mutate(prop_crowded_wt = prop_crowded_wt/population) %>% 
#   pull(prop_crowded_wt)

# acs_data <- acs_data %>% 
#   left_join(mean_hhSize_dist, by="hhSize") %>% 
#   mutate(prop_crowded_mean=mean_prop_crowded) %>% 
#   mutate(hhSize_factor=prop/prop_mean) %>% 
#   mutate(crowded_factor=prop_crowded/prop_crowded_mean) %>% 
#   select(-prop_mean, -prop_crowded_mean)

