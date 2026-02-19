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
#   - app/data/avg_movements_daily.csv (CA commodities from data/movements_*.csv)
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
# Reads all data/movements_*.csv files (one per commodity), filters to
# California origins, averages across years using actual week-of-year, and
# expands to daily resolution.

cat("Processing crop movement data...\n")

# Read all individual movement files (exclude movements_all.csv)
movement_files <- list.files("data", pattern = "^movements_", full.names = TRUE)
movement_files <- movement_files[!grepl("movements_all", movement_files)]

cat("  Found", length(movement_files), "movement files\n")

movements_raw <- map(movement_files, ~ read_csv(.x, show_col_types = FALSE)) %>%
  bind_rows()

# Filter to California origins, restrict to 2018-2025, aggregate by week
movements <- movements_raw %>%
  filter(grepl("California", origin)) %>%
  group_by(begin_date, commodity) %>%
  summarise(lbs = sum(`1_lb_units`), .groups = "drop") %>%
  mutate(begin_date = mdy(begin_date)) %>%
  filter(!is.na(begin_date),
         begin_date >= ymd("2018-01-01"),
         begin_date < ymd("2025-01-01")) %>%
  arrange(commodity, begin_date)

cat("  Commodities with CA data:", length(unique(movements$commodity)), "\n")

# Average across years using actual week-of-year (not sequential 1:n())
avg_movements <- movements %>%
  mutate(week = week(begin_date)) %>%
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

cat("  Commodities:", paste(sort(unique(avg_movements_daily$commodity)), collapse = ", "), "\n")

write_csv(avg_movements_daily, "app/data/avg_movements_daily.csv")
cat("Saved: app/data/avg_movements_daily.csv\n")

cat("\nAll app data exported successfully.\n")
