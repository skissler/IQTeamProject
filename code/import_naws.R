library(tidyverse) 
library(haven)

# Load data (change path as needed)
naws <- read_sas("data/naws_all.sas7bdat")  # or read_sav()

# Key variables: 
# FY (year of survey i.e. financial year) 
# HHFAMGRD (number of all relatives on the household grid) 
# REGION6 (NAWS region) 
# PWTYCRD (weight) 

naws_hh <- naws %>% 
  select(FY, REGION6, HHFAMGRD, PWTYCRD) %>% 
  filter(FY >= 2018) %>% 
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

naws_data <- naws_hh %>% 
  left_join(select(naws_crowding, REGION, REGION_NAME, REGION_ABBREV, prop_crowded), by=c("REGION","REGION_NAME","REGION_ABBREV"))