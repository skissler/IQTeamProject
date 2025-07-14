library(tidyverse)
library(readxl)
library(janitor)

calendar_data <- read_excel("data/raw/us_crop_calendars.xlsx")
calendar_clean <- calendar_data %>%
  clean_names() %>%
  pivot_longer(cols = starts_with("month"), names_to = "phase", values_to = "active") %>%
  filter(active == 1)

production <- read_csv("data/processed/county_crop_production.csv")

production_with_calendar <- production %>%
  left_join(calendar_clean, by = c("commodity_desc" = "crop"))

write_csv(production_with_calendar, "output/summary_tables/crop_calendar_by_county.csv")