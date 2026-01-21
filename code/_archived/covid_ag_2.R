# Install if needed
# install.packages(c("tidyverse", "lubridate", "tidycensus"))

library(tidyverse)
library(lubridate)
library(tidycensus)

jhu_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
jhu_raw <- read_csv(jhu_url)

jhu_cases <- jhu_raw %>%
  select(-UID, -iso2, -iso3, -code3, -Country_Region, -Lat, -Long_, -Combined_Key) %>% 
  filter(!is.na(FIPS)) %>%
  pivot_longer(cols=-c("FIPS","Admin2","Province_State"),
               names_to = "date", values_to = "cum_cases") %>%
  mutate(date = mdy(date)) %>%
  arrange(FIPS, Admin2, Province_State, date) %>%
  group_by(FIPS, Admin2, Province_State) %>%
  mutate(new_cases = cum_cases - lag(cum_cases, default = 0)) %>%
  ungroup()

jhu_cases %>% 
  ggplot(aes(x=date, y=cum_cases, group=factor(FIPS))) + 
    geom_line() 

# Set up Census API key (only needed once)
# census_api_key("YOUR_KEY", install = TRUE)


temp <- get_acs(geograph="county", 
    variables=c(
      byindustry="C24050_001", 
      bysex="C24030_001",
      agtot="C24050_030",
      agm="C24030_004",
      agf="C24030_031"),
    year=2022,
    survey="acs5")

temp %>% 
  select(-moe) %>% 
  pivot_wider(names_from=variable, values_from=estimate) %>% 
  mutate(totprop=agtot/byindustry) %>% 
  mutate(mfprop=(agm+agf)/bysex)


acs_ag <- get_acs(geography = "county",
                  variables = c(ag_emp = "C24050_027", total_emp = "C24050_001"),
                  year = 2022,
                  survey = "acs5") %>%
  select(GEOID, variable, estimate) %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  mutate(pct_ag = 100 * ag_emp / total_emp,
         ag_cat = ifelse(pct_ag > 20, ">20% ag emp", "<=20%")) %>%
  rename(FIPS = GEOID) %>% 
  mutate(FIPS=as.numeric(FIPS))

acs_pop <- get_acs(geography = "county", variables = "B01003_001", year = 2022) %>%
  select(GEOID, estimate) %>%
  rename(FIPS = GEOID, population = estimate) %>% 
  mutate(FIPS=as.numeric(FIPS))

jhu_merged <- jhu_cases %>%
  left_join(acs_ag, by = "FIPS") %>%
  left_join(acs_pop, by = "FIPS") %>%
  mutate(cases_per_100k = 100000 * new_cases / population) %>% 
  mutate(cum_cases_per_100k = 100000 * cum_cases / population)

plot_df <- jhu_merged %>%
  filter(!is.na(ag_cat)) %>%
  group_by(date, ag_cat) %>%
  summarise(mean_rate = mean(cum_cases_per_100k, na.rm = TRUE), .groups = "drop")

ggplot(plot_df, aes(x = date, y = mean_rate, color = ag_cat)) +
  geom_line(size = 1) +
  labs(title = "Daily COVID-19 Cases per 100k Population",
       subtitle = "Counties grouped by % of employment in agriculture",
       x = NULL, y = "Cases per 100,000",
       color = "County type") +
  theme_minimal() +
  theme(legend.position = "top")
