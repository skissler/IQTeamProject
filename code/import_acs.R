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

# Clean up a bit to align with naws dataset: 
acs_hh <- hhsize_dist %>% 
  filter(geo=="county") %>% 
  select(GEOID, NAME, HHSize=size, Prop=fraction)


fig_acs_hh <- acs_hh %>% 
  ggplot(aes(x=HHSize, y=Prop)) + 
    geom_jitter(width=0.05, alpha=0.02) + 
    expand_limits(y=0) +
    theme_classic() 









