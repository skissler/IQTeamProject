# ==============================================================================
# crop_calendars.R - Crop Calendar Productivity Analysis
# ==============================================================================
# Estimates agricultural productivity losses by overlaying epidemic dynamics
# with seasonal crop harvesting schedules for California crops.
#
# Analysis steps:
#   1. Load crop movement data (harvest volumes over time)
#   2. Load baseline epidemic simulation for California (Region 6)
#   3. Compute daily symptomatic infections (assuming 1-3 day symptom window)
#   4. Calculate workforce availability (1 - symptomatic proportion)
#   5. Estimate production losses for epidemics peaking on each day of year
#
# Inputs:
#   - data/movements_lettuce.csv     - Weekly lettuce shipments
#   - data/movements_strawberries.csv - Weekly strawberry shipments
#   - data/movements_oranges.csv     - Weekly orange shipments
#   - output/epidf_indiv_full_regional_r0_1.2.csv - Baseline epidemic simulation
#
# Outputs:
#   - figures/crop_movements_raw.pdf      - Raw weekly movement data
#   - figures/crop_movements_averaged.pdf - Averaged weekly patterns
#   - figures/crop_impact_by_peakday.pdf  - Production loss by epidemic timing
#   - output/crop_impact_summary.csv      - Impact summary table
# ==============================================================================

# Load dependencies (skip if already loaded via run_analysis.R)
if (!exists("paths")) {
  source('code/setup.R')
}

# ==============================================================================
# 1. Load and Process Crop Movement Data
# ==============================================================================

cat("Loading crop movement data...\n")

# Read weekly movement data for each crop
lettuce <- read_csv("data/movements_lettuce.csv", show_col_types = FALSE)
strawberries <- read_csv("data/movements_strawberries.csv", show_col_types = FALSE)
oranges <- read_csv("data/movements_oranges.csv", show_col_types = FALSE)

# Combine and filter to California origins
movements <- bind_rows(lettuce, strawberries, oranges) %>%
  filter(grepl("California", origin)) %>%
  group_by(begin_date, commodity) %>%
  summarise(lbs = sum(`1_lb_units`), .groups = "drop") %>%
  mutate(begin_date = mdy(begin_date)) %>%
  arrange(commodity, begin_date)

# Create averaged weekly pattern (across years)
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

cat("  Processed", length(unique(movements$commodity)), "commodities\n")

# ==============================================================================
# 2. Load Epidemic Simulation Data
# ==============================================================================

cat("Loading baseline epidemic simulation...\n")

# Load baseline regional simulation (r0_1.2 with default parameters)
baseline_file <- file.path(paths$output_dir, "epidf_indiv_full_regional_r0_1.2.csv")

if (!file.exists(baseline_file)) {
  stop("Baseline simulation file not found: ", baseline_file,
       "\nRun simulate_regional.R first with baseline parameters.")
}

epidf_indiv_full <- read_csv(baseline_file, show_col_types = FALSE)

# Filter to California (Region 6) for crop analysis
epidf_california <- epidf_indiv_full %>%
  filter(REGION6 == 6)

cat("  Loaded simulation for Region 6 (California)\n")

# ==============================================================================
# 3. Compute Symptomatic Infections
# ==============================================================================
# Assume symptoms last from day 1 to day 3 after infection onset.
# For each day t, sum up all infections that started 1-3 days ago.

cat("Computing symptomatic infection dynamics...\n")

# Calculate new infections per day
symp_temp <- epidf_california %>%
  group_by(subpop) %>%
  arrange(t) %>%
  mutate(Inew = lag(S_indiv) - S_indiv) %>%
  replace_na(list(Inew = 0)) %>%
  mutate(symp_start = t + 1, symp_end = t + 3) %>%
  select(subpop, REGION6, Inew, symp_start, symp_end)

# Compute symptomatic proportion at each time point
epidf_with_symp <- epidf_california %>%
  full_join(symp_temp, by = c("subpop", "REGION6"), relationship = "many-to-many") %>%
  mutate(tosum = case_when(t >= symp_start & t <= symp_end ~ Inew, TRUE ~ 0)) %>%
  group_by(t, subpop) %>%
  summarise(
    S_indiv = first(S_indiv),
    I_indiv = first(I_indiv),
    R_indiv = first(R_indiv),
    REGION6 = first(REGION6),
    symp = sum(tosum),
    .groups = "drop"
  )

# Find peak symptomatic time in community (reference for epidemic timing)
peaktime_community <- epidf_with_symp %>%
  filter(subpop == "C") %>%
  filter(symp == max(symp)) %>%
  pull(t) %>%
  first()

cat("  Community symptomatic peak at day", peaktime_community, "\n")

# ==============================================================================
# 4. Impact Assessment Function
# ==============================================================================

# Number of days in the crop calendar year (52 weeks)
CALENDAR_DAYS <- 364

#' Calculate crop production impact for a given epidemic peak day
#'
#' Maps the epidemic onto the 364-day (52 week) crop calendar. The simulation
#' now runs for a full 365 days, so we have complete coverage of the calendar.
#'
#' @param peakday Day of year when community symptomatic infections peak (1-364)
#' @param avg_movements_daily Daily crop movement data (days 1-364)
#' @param epidf_with_symp Epidemic data with symptomatic proportions
#' @return Tibble with production loss percentages by commodity
get_impact <- function(peakday, avg_movements_daily, epidf_with_symp) {

  # Get peak time from simulation (in simulation days)
  peaktime_sim <- epidf_with_symp %>%
    ungroup() %>%
    filter(subpop == "C") %>%
    filter(symp == max(symp)) %>%
    pull(t) %>%
    first()

  # Get workforce availability from agricultural workers
  # wf = 1 - symptomatic proportion (proportion available to work)
  wf_epidemic <- epidf_with_symp %>%
    filter(subpop == "A") %>%
    mutate(wf = 1 - symp) %>%
    select(t_sim = t, wf) %>%
    ungroup()

  # Calculate offset to align simulation peak with target peakday
  offset <- peakday - peaktime_sim

  # Map simulation days to calendar days (1-364)
  # Use modular arithmetic to wrap around the calendar year
  wf_mapped <- wf_epidemic %>%
    mutate(
      # Shift simulation time by offset, wrap to 1-364
      calendar_day = ((t_sim + offset - 1) %% CALENDAR_DAYS) + 1
    ) %>%
    # Keep only unique calendar days (in case of duplicates from wrapping)
    group_by(calendar_day) %>%
    summarise(wf = first(wf), .groups = "drop")

  # Join workforce data with crop calendar
  labor_shortage_df <- avg_movements_daily %>%
    left_join(wf_mapped, by = c("day" = "calendar_day")) %>%
    # If any days are missing (shouldn't happen with 365-day sim), use full workforce
    mutate(wf = if_else(is.na(wf), 1.0, wf)) %>%
    mutate(lbs_adj = lbs * wf)

  # Summarize total losses by commodity
  impact_df <- labor_shortage_df %>%
    group_by(commodity) %>%
    summarise(
      lbs_total = sum(lbs),
      lbs_adjusted = sum(lbs_adj),
      .groups = "drop"
    ) %>%
    mutate(pct_loss = (1 - lbs_adjusted / lbs_total) * 100)

  return(impact_df)
}

# ==============================================================================
# 5. Calculate Impact Across All Epidemic Timings
# ==============================================================================

cat("Calculating production impact for all epidemic peak days...\n")

# Calculate impact for each possible peak day (1-364)
impact_df_combined <- bind_rows(lapply(1:CALENDAR_DAYS, function(peakday) {
  out <- get_impact(peakday, avg_movements_daily, epidf_with_symp)
  out$peakday <- peakday
  return(out)
}))

cat("  Completed impact calculations for", CALENDAR_DAYS, "peak days\n")

# ==============================================================================
# 6. Generate Figures
# ==============================================================================

cat("Generating figures...\n")

# Color palette for crops
crop_colors <- c(
  "Oranges" = "orange",
  "Strawberries" = "magenta",
  "Lettuce, Iceberg" = "blue"
)

# Figure: Raw weekly movements over time
fig_movements_raw <- movements %>%
  filter(begin_date >= mdy("01-01-2018")) %>%
  filter(begin_date < mdy("01-01-2025")) %>%
  ggplot(aes(x = begin_date, y = lbs / 1e6, col = commodity)) +
  geom_line(linewidth = 0.8, alpha = 0.8) +
  scale_color_manual(values = crop_colors) +
  labs(
    x = "Date",
    y = "Weekly Shipments (Million lbs)",
    color = "Commodity",
    title = "California Crop Shipments Over Time"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(file.path(paths$figures_dir, "crop_movements_raw.pdf"),
       fig_movements_raw, width = 10, height = 5)
cat("  Saved: crop_movements_raw.pdf\n")

# Figure: Averaged weekly pattern
fig_movements_avg <- avg_movements %>%
  ggplot(aes(x = week, y = lbs / 1e6, col = commodity)) +
  geom_line(linewidth = 0.8, alpha = 0.8) +
  scale_color_manual(values = crop_colors) +
  labs(
    x = "Week of Year",
    y = "Average Weekly Shipments (Million lbs)",
    color = "Commodity",
    title = "Average Seasonal Harvest Pattern"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(file.path(paths$figures_dir, "crop_movements_averaged.pdf"),
       fig_movements_avg, width = 8, height = 5)
cat("  Saved: crop_movements_averaged.pdf\n")

# Figure: Production loss by epidemic peak timing
fig_impact <- impact_df_combined %>%
  ggplot(aes(x = peakday, y = pct_loss, col = commodity)) +
  geom_line(linewidth = 0.8, alpha = 0.8) +
  scale_color_manual(values = crop_colors) +
  labs(
    x = "Day of Year (Epidemic Peak)",
    y = "Production Loss (%)",
    color = "Commodity",
    title = "Estimated Production Loss by Epidemic Timing",
    subtitle = "Based on baseline epidemic (R0 = 1.2) in California"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(file.path(paths$figures_dir, "crop_impact_by_peakday.pdf"),
       fig_impact, width = 10, height = 5)
cat("  Saved: crop_impact_by_peakday.pdf\n")

# ==============================================================================
# 7. Save Summary Output
# ==============================================================================

# Summary: min, max, mean loss by commodity
impact_summary <- impact_df_combined %>%
  group_by(commodity) %>%
  summarise(
    min_loss_pct = min(pct_loss),
    max_loss_pct = max(pct_loss),
    mean_loss_pct = mean(pct_loss),
    worst_peakday = peakday[which.max(pct_loss)],
    best_peakday = peakday[which.min(pct_loss)],
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# Verification: Why mean_loss_pct is identical across commodities
# ------------------------------------------------------------------------------
# When averaging over all 364 possible epidemic peak days, each calendar day
# experiences every part of the epidemic exactly once. This means the average
# workforce availability at any calendar day equals mean(wf) regardless of the
# commodity's seasonal production pattern. Therefore:
#   mean_loss = (1 - mean(wf)) * 100
# and this value is independent of the crop harvest schedule.
#
# Compute expected loss from mean workforce availability to verify:
wf_for_verification <- epidf_with_symp %>%
  filter(subpop == "A") %>%
  mutate(wf = 1 - symp) %>%
  summarise(mean_wf = mean(wf)) %>%
  pull(mean_wf)

expected_mean_loss <- (1 - wf_for_verification) * 100
actual_mean_losses <- unique(round(impact_summary$mean_loss_pct, 6))

cat("\n  Verification: Identical mean_loss_pct values are mathematically expected\n")
cat("    Expected from mean(wf):", round(expected_mean_loss, 3), "%\n")
cat("    Actual mean_loss_pct:  ", round(actual_mean_losses, 3), "%\n")
if (length(actual_mean_losses) == 1 && abs(expected_mean_loss - actual_mean_losses) < 0.01) {
  cat("    Status: VERIFIED - means match as expected\n")
} else {
  cat("    Status: WARNING - unexpected deviation from theory\n")
}

write_csv(impact_summary, file.path(paths$output_dir, "crop_impact_summary.csv"))
cat("  Saved: crop_impact_summary.csv\n")

# Print summary
cat("\n", rep("=", 60), "\n", sep = "")
cat("Crop Calendar Analysis Summary\n")
cat(rep("=", 60), "\n\n", sep = "")
print(impact_summary)
cat("\nCrop calendar analysis complete.\n")
