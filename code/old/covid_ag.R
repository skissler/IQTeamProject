# Install if needed
# install.packages(c("tidyverse", "lubridate", "tidycensus"))

library(tidyverse)
library(lubridate)
library(tidycensus)

jhu_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
jhu_raw <- read_csv(jhu_url)

jhu_cases <- jhu_raw %>%
  select(FIPS, Admin2, Province_State, starts_with("1/")) %>%
  filter(!is.na(FIPS)) %>%
  pivot_longer(cols = starts_with("1/"),
               names_to = "date", values_to = "cum_cases") %>%
  mutate(date = mdy(date)) %>%
  group_by(FIPS) %>%
  arrange(date) %>%
  mutate(new_cases = cum_cases - lag(cum_cases, default = 0)) %>%
  ungroup()

# Set up Census API key (only needed once)
# census_api_key("YOUR_KEY", install = TRUE)

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
  mutate(cases_per_100k = 100000 * new_cases / population)

plot_df <- jhu_merged %>%
  filter(!is.na(ag_cat)) %>%
  group_by(date, ag_cat) %>%
  summarise(mean_rate = mean(cases_per_100k, na.rm = TRUE), .groups = "drop")

ggplot(plot_df, aes(x = date, y = mean_rate, color = ag_cat)) +
  geom_line(size = 1) +
  labs(title = "Daily COVID-19 Cases per 100k Population",
       subtitle = "Counties grouped by % of employment in agriculture",
       x = NULL, y = "Cases per 100,000",
       color = "County type") +
  theme_minimal() +
  theme(legend.position = "top")
