# //////////////////////////////////////////////////////////////////////////////
# Import packages
# //////////////////////////////////////////////////////////////////////////////

library(tidyverse) 
library(haven)

# //////////////////////////////////////////////////////////////////////////////
# Load data
# //////////////////////////////////////////////////////////////////////////////

# Load data (change path as needed)
naws <- read_sas("data/naws_all.sas7bdat")  # or read_sav()

# Key variables: 
# FY (year of survey i.e. financial year) 
# HHFAMGRD (number of all relatives on the household grid) 
# REGION6 (NAWS region) 
# PWTYCRD (weight) 

region_map <- tibble(REGION6=1:6, 
  REGION_NAME=c(
    "East",
    "Southeast",
    "Midwest",
    "Southwest",
    "Northwest",
    "California"), 
  REGION_ABBREV=c("EA","SE","MW","SW","NW","CA"))

# //////////////////////////////////////////////////////////////////////////////
# Household sizes
# //////////////////////////////////////////////////////////////////////////////

naws_hh <- naws %>% 
  select(FY, REGION6, HHFAMGRD, PWTYCRD) %>% 
  filter(FY >= 2018 & FY <= 2022) %>% 
  group_by(HHFAMGRD, REGION6) %>% 
  summarise(PWTYCRD=sum(PWTYCRD)) %>% 
  group_by(REGION6) %>% 
  mutate(PWTYCRD_TOT=sum(PWTYCRD)) %>% 
  mutate(HHFAMGRD_PROP=PWTYCRD/PWTYCRD_TOT) %>% 
  arrange(REGION6, HHFAMGRD) %>% 
  select(REGION=REGION6, hhSize=HHFAMGRD, prop=HHFAMGRD_PROP) %>% 
  left_join(region_map, by=c("REGION"="REGION6")) %>% 
  mutate(hhSize_agg = case_when(hhSize<=6 ~ hhSize, TRUE~7)) %>% 
  group_by(REGION, REGION_NAME, REGION_ABBREV, hhSize_agg) %>% 
  summarise(prop=sum(prop)) %>% 
  rename(hhSize=hhSize_agg)

# //////////////////////////////////////////////////////////////////////////////
# Crowding
# //////////////////////////////////////////////////////////////////////////////

naws_crowding <- naws %>% 
  select(FY, REGION6, CROWDED1, PWTYCRD) %>% 
  filter(FY >= 2018) %>% 
  group_by(CROWDED1, REGION6) %>% 
  summarise(PWTYCRD=sum(PWTYCRD)) %>% 
  group_by(REGION6) %>% 
  mutate(PWTYCRD_TOT=sum(PWTYCRD)) %>% 
  mutate(CROWDED1_PROP=PWTYCRD/PWTYCRD_TOT) %>% 
  arrange(REGION6, CROWDED1) %>% 
  filter(CROWDED1==1) %>% 
  select(REGION=REGION6, Crowded=CROWDED1, prop=CROWDED1_PROP) %>% 
  left_join(region_map, by=c("REGION"="REGION6")) %>% 
  select(REGION, REGION_NAME, REGION_ABBREV, Crowded, prop_crowded=prop)

# //////////////////////////////////////////////////////////////////////////////
# Combine
# //////////////////////////////////////////////////////////////////////////////

# The following data frame contains: 
# REGION: numeric NAWS region identifier (1-6)
# REGION_NAME: name of the region
# REGION_ABBREV: abbreviated name of the region
# hhSize: the household size reflected in the 'prop' column
# prop: proportion of households in region of size "hhSize" 
# prop_crowded: proportion of households in region that have >1 occupant/room

naws_data <- naws_hh %>% 
  left_join(select(naws_crowding, REGION, REGION_NAME, REGION_ABBREV, prop_crowded), by=c("REGION","REGION_NAME","REGION_ABBREV"))
