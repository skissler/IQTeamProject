library(tidycensus)
library(dplyr)

# Load API key if needed
# census_api_key("YOUR_KEY", install = TRUE)
readRenviron("~/.Renviron")

# Year and table
year <- 2021
table <- "C24030"

# Pull agricultural workers and total employment by county
ag_vars <- c(
  total_employed = "C24030_001",
  ag_workers_male = "C24030_004",
  ag_workers_female = "C24030_031"
)

acs_ag <- get_acs(
  geography = "county",
  variables = ag_vars,
  year = year,
  survey = "acs5",
  output = "wide"
)

# Calculate percent employed in agriculture
acs_ag <- acs_ag %>%
  mutate(
    prop_ag_workers = (ag_workers_maleE + ag_workers_femaleE) / total_employedE
  ) %>%
  select(GEOID, NAME, prop_ag_workers)

# View result
# head(acs_ag)

fig_acs_ag <- ggplot(acs_ag, aes(x = prop_ag_workers)) +
  geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
  labs(title = "Distribution of Agricultural Worker Propotion by County",
       x = "Proportion Ag Workers",
       y = "Number of Counties") +
  theme_minimal()

