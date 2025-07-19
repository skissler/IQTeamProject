# Install if needed
# install.packages(c("tidycensus","dplyr","tidyr"))

library(tidycensus)
library(dplyr)
library(tidyr)

# Load your Census key
# census_api_key("YOUR_CENSUS_API_KEY", install = TRUE)
readRenviron("~/.Renviron")

year <- 2021
geos <- c("county", "state")

# Pull table B11016
acs_hhsize <- map_dfr(geos, ~ {
  get_acs(geography = .x,
          table = "B11016",
          survey = "acs5",
          year = year,
          output = "wide") %>%
    mutate(geo = .x)
})

# Nicer variable names: total + sizes 1-7plus
vars <- names(acs_hhsize)[grep("B11016_", names(acs_hhsize))]
# Identify categories by suffix
acat <- tibble(var = vars) %>%
  mutate(size = case_when(
    grepl("_010E$", var) ~ 1,
    grepl("_003E$", var) ~ 2,
    grepl("_004E$", var) ~ 3,
    grepl("_005E$", var) ~ 4,
    grepl("_006E$", var) ~ 5,
    grepl("_007E$", var) ~ 6,
    grepl("_008E$", var) ~ 7  # 7 or more
  ))

# Pivot to long form with counts and compute fractions
hhsize_dist <- acs_hhsize %>%
  pivot_longer(cols = all_of(acat$var),
               names_to = "var", values_to = "count") %>%
  left_join(acat, by = "var") %>%
  filter(!is.na(size)) %>% # added
  group_by(geo, GEOID, NAME) %>%
  mutate(total = sum(count, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(geo, GEOID, NAME, size) %>%
  summarize(count = sum(count), total = first(total), .groups = "drop") %>%
  mutate(fraction = count / total) %>%
  arrange(geo, GEOID, size)

# Wide format: fraction for each household size 1..7plus
hhsize_wide <- hhsize_dist %>%
  select(geo, GEOID, NAME, size, fraction) %>%
  pivot_wider(names_from = size, values_from = fraction, names_prefix = "hhsize_")

# Example output
head(hhsize_wide)

hhsize_dist %>% 
  group_by(size) %>% 
  summarise(fraction=mean(fraction)) %>% 
  filter(!is.na(size)) %>% 
  mutate(fraction=fraction/sum(fraction)) %>% 
  ggplot(aes(x=size, y=fraction)) + 
    geom_point() 

hhsize_dist %>% 
  filter(!is.na(size)) %>% 
  ggplot() + 
    geom_point(aes(x=size, y=fraction, group=NAME), alpha=0.1) + 
    geom_line(data=naws_hh, aes(x=HHSize, y=Prop, col=factor(REGION_ABBREV))) + 
    theme(legend.position="none")












