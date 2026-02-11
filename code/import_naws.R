# //////////////////////////////////////////////////////////////////////////////
# Import NAWS Data
# //////////////////////////////////////////////////////////////////////////////
# Processes National Agricultural Workers Survey data for:
#   - Household size distributions by NAWS region
#   - Crowding rates by region
#
# Requires: NAWS SAS data file (data/naws_all.sas7bdat)
# Outputs: naws_data, naws_hh, naws_crowding data frames
# //////////////////////////////////////////////////////////////////////////////

# Load setup if not already loaded (allows standalone use or via setup.R)
if (!exists("paths")) {
  source('code/config.R')
  library(tidyverse)
  library(haven)
}

# //////////////////////////////////////////////////////////////////////////////
# Load data
# //////////////////////////////////////////////////////////////////////////////

# Load NAWS data using configured path
naws <- haven::read_sas(paths$naws_data)

# Key variables: 
# FY (year of survey i.e. financial year) 
# HHFAMGRD (number of all relatives on the household grid) 
# REGION6 (NAWS region) 
# PWTYCRD (weight) 

# //////////////////////////////////////////////////////////////////////////////
# Household sizes
# //////////////////////////////////////////////////////////////////////////////

naws_hh <- naws %>%
  select(FY, REGION6, HHFAMGRD, PWTYCRD) %>%
  filter(FY >= data_settings$naws_start_year & FY <= data_settings$naws_end_year) %>% 
  group_by(HHFAMGRD, REGION6) %>% 
  summarise(PWTYCRD=sum(PWTYCRD)) %>% 
  group_by(REGION6) %>% 
  mutate(PWTYCRD_TOT=sum(PWTYCRD)) %>% 
  mutate(HHFAMGRD_PROP=PWTYCRD/PWTYCRD_TOT) %>% 
  arrange(REGION6, HHFAMGRD) %>% 
  select(REGION6, hhSize=HHFAMGRD, prop=HHFAMGRD_PROP) %>%
  left_join(region_map, by="REGION6") %>%
  mutate(hhSize_agg = case_when(hhSize<=6 ~ hhSize, TRUE~7)) %>%
  group_by(REGION6, REGION_NAME, REGION_ABBREV, hhSize_agg) %>%
  summarise(prop=sum(prop)) %>%
  rename(hhSize=hhSize_agg)


# variance?
temp <- naws %>%
  select(FY, REGION6, HHFAMGRD, PWTYCRD) %>%
  filter(FY >= data_settings$naws_start_year & FY <= data_settings$naws_end_year) %>%
  group_by(HHFAMGRD, REGION6) %>%
  summarise(PWTYCRD=sum(PWTYCRD)) %>%
  group_by(REGION6) %>%
  mutate(PWTYCRD_TOT=sum(PWTYCRD)) %>%
  mutate(HHFAMGRD_PROP=PWTYCRD/PWTYCRD_TOT) %>%
  arrange(REGION6, HHFAMGRD) %>%
  select(REGION6, hhSize=HHFAMGRD, prop=HHFAMGRD_PROP) %>%
  left_join(region_map, by="REGION6") %>%
  mutate(hhSize_agg = case_when(hhSize<=6 ~ hhSize, TRUE~7)) %>%
  group_by(REGION6, REGION_NAME, REGION_ABBREV, hhSize_agg) %>%
  summarise(prop=sum(prop)) %>%
  rename(hhSize=hhSize_agg)


# //////////////////////////////////////////////////////////////////////////////
# Crowding
# //////////////////////////////////////////////////////////////////////////////

naws_crowding <- naws %>%
  select(FY, REGION6, CROWDED1, PWTYCRD) %>%
  filter(FY >= data_settings$naws_start_year & FY <= data_settings$naws_end_year) %>% 
  group_by(CROWDED1, REGION6) %>% 
  summarise(PWTYCRD=sum(PWTYCRD)) %>% 
  group_by(REGION6) %>% 
  mutate(PWTYCRD_TOT=sum(PWTYCRD)) %>% 
  mutate(CROWDED1_PROP=PWTYCRD/PWTYCRD_TOT) %>% 
  arrange(REGION6, CROWDED1) %>% 
  filter(CROWDED1==1) %>% 
  select(REGION6, Crowded=CROWDED1, prop=CROWDED1_PROP) %>%
  left_join(region_map, by="REGION6") %>%
  select(REGION6, REGION_NAME, REGION_ABBREV, Crowded, prop_crowded=prop) %>%
  ungroup() 

# //////////////////////////////////////////////////////////////////////////////
# Combine
# //////////////////////////////////////////////////////////////////////////////

# The following data frame contains: 
# REGION6: numeric NAWS region identifier (1-6)
# REGION_NAME: name of the region
# REGION_ABBREV: abbreviated name of the region
# hhSize: the household size reflected in the 'prop' column
# prop: proportion of households in region of size "hhSize"
# prop_crowded: proportion of households in region that have >1 occupant/room

naws_data <- naws_hh %>%
  left_join(select(naws_crowding, REGION6, REGION_NAME, REGION_ABBREV, prop_crowded), by=c("REGION6","REGION_NAME","REGION_ABBREV")) %>%
  ungroup() 
