# ==============================================================================
# export_app_data.R - Export Data for Shiny App
# ==============================================================================
# One-time script that sources the main pipeline and writes small CSVs
# to app/data/ for the self-contained Shiny app.
#
# Outputs:
#   - app/data/acs_data_regional.csv  (42 rows, 6 regions x 7 HH sizes)
#   - app/data/naws_data.csv          (42 rows, 6 regions x 7 HH sizes)
#   - app/data/region_map.csv         (6 rows)
#   - app/data/avg_movements_daily.csv (all CA commodities from movements_all.csv)
#
# Run from project root:
#   source('code/export_app_data.R')
# ==============================================================================

# Load dependencies
if (!exists("paths")) {
  source('code/setup.R')
}

# Import data (skip if already loaded)
if (!exists("naws_data")) {
  source('code/import_naws.R')
}
if (!exists("acs_data_regional")) {
  source('code/import_acs.R')
}

# ==============================================================================
# 1. Region Map
# ==============================================================================

write_csv(region_map, "app/data/region_map.csv")
cat("Saved: app/data/region_map.csv\n")

# ==============================================================================
# 2. ACS Regional Data
# ==============================================================================

write_csv(acs_data_regional, "app/data/acs_data_regional.csv")
cat("Saved: app/data/acs_data_regional.csv\n")

# ==============================================================================
# 3. NAWS Data
# ==============================================================================

write_csv(naws_data, "app/data/naws_data.csv")
cat("Saved: app/data/naws_data.csv\n")

# ==============================================================================
# 4. Average Daily Crop Movements (California)
# ==============================================================================

cat("Processing crop movement data...\n")

movements_all <- read_csv(paths$movements_all, show_col_types = FALSE)

# Filter to California origins, aggregate by week
movements <- movements_all %>%
  filter(grepl("California", origin)) %>%
  group_by(begin_date, commodity) %>%
  summarise(lbs = sum(`1_lb_units`), .groups = "drop") %>%
  mutate(begin_date = mdy(begin_date)) %>%
  arrange(commodity, begin_date)

# Average across years
avg_movements <- movements %>%
  mutate(year = year(begin_date)) %>%
  arrange(commodity, begin_date) %>%
  group_by(commodity, year) %>%
  mutate(week = 1:n()) %>%
  group_by(commodity, week) %>%
  summarise(lbs = mean(lbs, na.rm = TRUE), .groups = "drop") %>%
  filter(week <= 52)

# Expand to daily resolution (divide weekly lbs by 7)
avg_movements_daily <- avg_movements %>%
  split(.$commodity) %>%
  map(~ split(., .$week)) %>%
  map(~ map(., ~ cross_join(., tibble(day_of_week = 1:7)))) %>%
  map(~ map(., ~ mutate(., day = (week - 1) * 7 + day_of_week))) %>%
  map(~ bind_rows(.)) %>%
  bind_rows() %>%
  mutate(lbs = lbs / 7) %>%
  select(commodity, week, day, lbs)

write_csv(avg_movements_daily, "app/data/avg_movements_daily.csv")
cat("Saved: app/data/avg_movements_daily.csv\n")

cat("\nAll app data exported successfully.\n")
