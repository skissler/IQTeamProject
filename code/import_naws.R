# Load libraries
library(haven)     # for reading Stata/SAV files
library(dplyr)
library(tidyr)

# Load data (change path as needed)
naws <- read_sas("data/naws_all.sas7bdat")  # or read_sav()

# Check column names
names(naws)

# Key variables:
# - HH_SIZE: number of people in household
# - REGION: NAWS region (1-5)
# - WEIGHT: person-level weight (often called `wgt` or `weight_final`)
# - YEAR: survey year (optional filter)

# Probably what we actually want: 
# FY (year of survey i.e. financial year) 
# HHFAMGRD (number of all relatives on the household grid) 
# REGION6 (NAWS region) 
# PWTYCRD (weight) 
# Wait - should be using NHH (number of persons in HH at time of interview!) 

region_map <- tibble(REGION6=1:6, 
  REGION_NAME=c("East","Southeast","Midwest","Southwest","Northwest","California"), 
  REGION_ABBREV=c("EA","SE","MW","SW","NW","CA"))

naws_hh <- naws %>% 
  select(FY, REGION6, HHFAMGRD, PWTYCRD) %>% 
  filter(FY >= 2018) %>% 
  group_by(HHFAMGRD, REGION6) %>% 
  summarise(PWTYCRD=sum(PWTYCRD)) %>% 
  group_by(REGION6) %>% 
  mutate(PWTYCRD_TOT=sum(PWTYCRD)) %>% 
  mutate(HHFAMGRD_PROP=PWTYCRD/PWTYCRD_TOT) %>% 
  arrange(REGION6, HHFAMGRD) %>% 
  select(Region=REGION6, HHSize=HHFAMGRD, Prop=HHFAMGRD_PROP) %>% 
  left_join(region_map, by=c("Region"="REGION6"))

# Aggregate households 7+ to align with census data: 
naws_hh <- naws_hh %>% 
  mutate(HHSize_agg = case_when(HHSize<=6 ~ HHSize, TRUE~7)) %>% 
  group_by(Region, REGION_NAME, REGION_ABBREV, HHSize_agg) %>% 
  summarise(Prop=sum(Prop)) %>% 
  rename(HHSize=HHSize_agg)

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
  select(Region=REGION6, Crowded=CROWDED1, Prop=CROWDED1_PROP) %>% 
  left_join(region_map, by=c("Region"="REGION6")) %>% 
  select(Region, REGION_NAME, REGION_ABBREV, Crowded, Prop)

fig_naws <- naws_hh %>% 
  ggplot(aes(x=HHSize, y=Prop, col=factor(REGION_ABBREV))) + 
    geom_point() + 
    geom_line() + 
    theme(legend.position="none")

# naws_hh %>% 
#   group_by(HHSize) %>% 
#   summarise(Prop=mean(Prop))


