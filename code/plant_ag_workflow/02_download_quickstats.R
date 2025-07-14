library(tidyverse)
library(rnassqs)
# Sys.setenv(NASSQS_TOKEN = "your_api_key_here")

crops <- c("SOYBEANS", "WHEAT", "CORN", "TOMATOES", "ONIONS", "APPLES")

query_crop_data <- function(crop, year = 2022) {
  rnassqs::nassqs(
    list(
      commodity_desc = crop,
      year = year,
      agg_level_desc = "COUNTY",
      statisticcat_desc = "PRODUCTION",
      unit_desc = "TONS",
      source_desc = "CENSUS"
    )
  )
}

all_crop_data <- map_dfr(crops, safely(query_crop_data))
all_crop_data_clean <- map_dfr(all_crop_data, "result") %>% clean_names()
write_csv(all_crop_data_clean, "data/processed/county_crop_production.csv")