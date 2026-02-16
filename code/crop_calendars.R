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
# Note: This code assumes p_symp = 1 (all infections are symptomatic).
# Because production loss scales linearly with p_symp, you can obtain
# results for any p_symp by multiplying the output losses by p_symp.
#
# Inputs:
#   - data/movements_lettuce.csv     - Weekly lettuce shipments
#   - data/movements_strawberries.csv - Weekly strawberry shipments
#   - data/movements_oranges.csv     - Weekly orange shipments
#   - output/regional_sim_r0_1.5.csv              - Baseline epidemic simulation
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
lettuce <- read_csv(paths$movements_lettuce, show_col_types = FALSE)
strawberries <- read_csv(paths$movements_strawberries, show_col_types = FALSE)
oranges <- read_csv(paths$movements_oranges, show_col_types = FALSE)

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

# Load baseline regional simulation (using default R0 from config)
baseline_file <- file.path(paths$output_dir,
                           paste0("regional_sim_r0_", default_pars$r0, ".csv"))

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

# Check the epidemic wraparound: 
check_wraparound <- function(peakday, avg_movements_daily, epidf_with_symp) {

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

  return(wf_mapped)
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
  expand_limits(y = 0) +
  labs(
    x = "Date",
    y = "Weekly Shipments (Million lbs)",
    color = "Commodity",
    title = "California Crop Shipments Over Time"
  ) +
  theme_classic() +
  theme(legend.position = "bottom")

ggsave(file.path(paths$figures_dir, "crop_movements_raw.pdf"),
       fig_movements_raw, width = 10, height = 5)
ggsave(file.path(paths$figures_dir, "crop_movements_raw.png"),
       fig_movements_raw, width = 10, height = 5, dpi = 300)
cat("  Saved: crop_movements_raw.pdf/.png\n")

# Figure: Averaged weekly pattern
fig_movements_avg <- avg_movements %>%
  ggplot(aes(x = week, y = lbs / 1e6, col = commodity)) +
  geom_line(linewidth = 0.8, alpha = 0.8) +
  scale_color_manual(values = crop_colors) +
  expand_limits(y = 0) +
  labs(
    x = "Week of Year",
    y = "Average Weekly Shipments (Million lbs)",
    color = "Commodity",
    title = "Average Seasonal Harvest Pattern"
  ) +
  theme_classic() +
  theme(legend.position = "bottom")

ggsave(file.path(paths$figures_dir, "crop_movements_averaged.pdf"),
       fig_movements_avg, width = 8, height = 5)
ggsave(file.path(paths$figures_dir, "crop_movements_averaged.png"),
       fig_movements_avg, width = 8, height = 5, dpi = 300)
cat("  Saved: crop_movements_averaged.pdf/.png\n")

# Figure: Normalized movements overlaid with UC Davis harvest information
# Uses day-of-year on x-axis so that month ticks, USDA data, and UC Davis
# reference data all align correctly.
cat("Generating crop validation figure...\n")

# Normalize each commodity so weekly proportions sum to 1, then convert to
# day-of-year x-coordinates (midpoint of each week)
avg_movements_norm <- avg_movements %>%
  group_by(commodity) %>%
  mutate(prop = lbs / sum(lbs)) %>%
  ungroup() %>%
  mutate(day_mid = (week - 0.5) * 7)  # midpoint of week in days

# --- UC Davis reference data ---

month_starts <- cumsum(c(1, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30))  # day 1 of each month
month_days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

# Strawberry monthly harvest proportions (Apr=4, May=5, ..., Oct=10)
# Source: UC Davis Cost and Return Study
# Stairstep: each month shows harvest_prop / (days_in_month / 7) as a flat segment
straw_ucdavis <- tibble(
  month = 4:10,
  harvest_prop = c(0.05, 0.12, 0.25, 0.26, 0.18, 0.12, 0.02)
) %>%
  mutate(
    day_start = month_starts[month],
    day_end = month_starts[month] + month_days[month],
    weekly_prop = harvest_prop / (month_days[month] / 7)
  )

# Orange harvest season: November to June (UC Davis)
# Wraps around year: Nov 1 (day 305) to Jun 30 (day 181)
orange_bar <- tibble(
  xmin = c(month_starts[11], 1),
  xmax = c(365, month_starts[6] + month_days[6])
)

# Lettuce: planting late Dec to mid-Aug, ~100d cool / ~50d warm maturation
# Planting: Dec 20 (day 354) to mid-Aug (day ~227)
# Harvest: shift start by +100d, shift end by +50d
# Harvest start: 354 + 100 = 454 → wraps to day 89 (late Mar)
# Harvest end: 227 + 50 = 277 (early Oct)
lettuce_plant <- tibble(
  xmin = c(354, 1),
  xmax = c(365, month_starts[8] + month_days[8] / 2)  # Dec 20 to mid-Aug
)
lettuce_harvest <- tibble(
  xmin = 89,   # late Mar (Dec 20 + 100 days, wrapped)
  xmax = 277   # early Oct
)

# Bar y-positions (placed near top of the plot for visibility)
max_prop <- max(avg_movements_norm$prop, na.rm = TRUE)
bar_y_orange <- max_prop * 1.08
bar_y_lettuce_plant <- max_prop * 1.00
bar_y_lettuce_harvest <- max_prop * 0.96
bar_height <- max_prop * 0.025

# Month axis ticks (day of year for 1st of each month)
val_month_breaks <- month_starts
val_month_labels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

fig_movements_validated <- ggplot() +
  # Orange harvest season bars (wraps around year)
  geom_rect(data = orange_bar,
            aes(xmin = xmin, xmax = xmax,
                ymin = bar_y_orange - bar_height, ymax = bar_y_orange + bar_height),
            fill = "orange", alpha = 0.4) +
  # Lettuce planting bars (wraps around year)
  geom_rect(data = lettuce_plant,
            aes(xmin = xmin, xmax = xmax,
                ymin = bar_y_lettuce_plant - bar_height,
                ymax = bar_y_lettuce_plant + bar_height),
            fill = "blue", alpha = 0.15) +
  # Lettuce harvest bar
  geom_rect(data = lettuce_harvest,
            aes(xmin = xmin, xmax = xmax,
                ymin = bar_y_lettuce_harvest - bar_height,
                ymax = bar_y_lettuce_harvest + bar_height),
            fill = "blue", alpha = 0.35) +
  # Normalized USDA shipment curves
  geom_line(data = avg_movements_norm,
            aes(x = day_mid, y = prop, color = commodity),
            linewidth = 0.8, alpha = 0.8) +
  # UC Davis strawberry reference (stairstep with vertical risers)
  geom_segment(data = straw_ucdavis,
               aes(x = day_start, xend = day_end,
                   y = weekly_prop, yend = weekly_prop),
               color = "magenta", linewidth = 0.8, linetype = "dashed", alpha = 0.8) +
  # Vertical risers connecting consecutive months
  geom_segment(data = straw_ucdavis %>%
                 mutate(prev_prop = lag(weekly_prop, default = 0)) %>%
                 filter(weekly_prop != prev_prop),
               aes(x = day_start, xend = day_start,
                   y = prev_prop, yend = weekly_prop),
               color = "magenta", linewidth = 0.8, linetype = "dashed", alpha = 0.8) +
  # Closing riser at end of season (last month back to 0)
  geom_segment(data = straw_ucdavis %>% slice_tail(n = 1),
               aes(x = day_end, xend = day_end,
                   y = weekly_prop, yend = 0),
               color = "magenta", linewidth = 0.8, linetype = "dashed", alpha = 0.8) +
  # Annotations
  annotate("text", x = 70, y = bar_y_orange + bar_height * 2.5,
           label = "Oranges: harvest (Nov-Jun)", size = 2.8, hjust = 0,
           color = "darkorange3") +
  annotate("text", x = 290, y = bar_y_lettuce_plant + bar_height * 2.5,
           label = "Lettuce: planting", size = 2.8, hjust = 1,
           color = "blue", alpha = 0.5) +
  annotate("text", x = 290, y = bar_y_lettuce_harvest - bar_height * 3,
           label = "Lettuce: harvest", size = 2.8, hjust = 1,
           color = "blue", alpha = 0.8) +
  scale_color_manual(values = crop_colors) +
  scale_x_continuous(breaks = val_month_breaks, labels = val_month_labels,
                     minor_breaks = NULL, limits = c(1, 365)) +
  expand_limits(y = 0) +
  labs(
    x = NULL,
    y = "Weekly Proportion of Annual Harvest",
    color = "USDA Shipment Data",
    title = "Seasonal Harvest Patterns: USDA Shipment Data vs. UC Davis Harvest Calendars",
    subtitle = "Solid lines: normalized USDA shipments. Dashed segments: UC Davis strawberry harvest proportions.\nHorizontal bars: UC Davis harvest/planting seasons for oranges and lettuce."
  ) +
  theme_classic() +
  theme(legend.position = "bottom",
        plot.subtitle = element_text(size = 9, color = "grey40"))

ggsave(file.path(paths$figures_dir, "crop_movements_validated.pdf"),
       fig_movements_validated, width = 10, height = 6)
ggsave(file.path(paths$figures_dir, "crop_movements_validated.png"),
       fig_movements_validated, width = 10, height = 6, dpi = 300)
cat("  Saved: crop_movements_validated.pdf/.png\n")

# Month axis breaks and labels for impact figures
impact_month_breaks <- cumsum(c(1, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30))
impact_month_labels <- c("Jan", "", "", "Apr", "", "",
                         "Jul", "", "", "Oct", "", "")

# Figure: Production loss by epidemic peak timing (p_symp = 1)
fig_impact <- impact_df_combined %>%
  ggplot(aes(x = peakday, y = pct_loss, col = commodity)) +
  geom_line(linewidth = 0.8, alpha = 0.8) +
  scale_color_manual(values = crop_colors) +
  scale_x_continuous(breaks = impact_month_breaks, labels = impact_month_labels,
                     minor_breaks = NULL) +
  expand_limits(y = 0) +
  labs(
    x = "Epidemic Peak Timing",
    y = "Production Loss (%)",
    color = "Commodity",
    title = "Estimated Production Loss by Epidemic Timing",
    subtitle = paste0("Based on baseline epidemic (R0 = ", default_pars$r0,
                      ") in California, p_symp = 1")
  ) +
  theme_classic() +
  theme(legend.position = "bottom")

ggsave(file.path(paths$figures_dir, "crop_impact_by_peakday.pdf"),
       fig_impact, width = 10, height = 5)
ggsave(file.path(paths$figures_dir, "crop_impact_by_peakday.png"),
       fig_impact, width = 10, height = 5, dpi = 300)
cat("  Saved: crop_impact_by_peakday.pdf/.png\n")

# Figure: Production loss scaled by p_symp = 0.5
fig_impact_psymp <- impact_df_combined %>%
  mutate(pct_loss_scaled = pct_loss * 0.5) %>%
  ggplot(aes(x = peakday, y = pct_loss_scaled, col = commodity)) +
  geom_line(linewidth = 0.8, alpha = 0.8) +
  scale_color_manual(values = crop_colors) +
  scale_x_continuous(breaks = impact_month_breaks, labels = impact_month_labels,
                     minor_breaks = NULL) +
  expand_limits(y = 0) +
  labs(
    x = "Epidemic Peak Timing",
    y = "Production Loss (%)",
    color = "Commodity",
    title = "Estimated Production Loss by Epidemic Timing",
    subtitle = bquote("Based on baseline epidemic (R0 = " * .(default_pars$r0) *
                      ") in California, " * p[symp] ~ "= 0.5")
  ) +
  theme_classic() +
  theme(legend.position = "bottom")

ggsave(file.path(paths$figures_dir, "crop_impact_by_peakday_psymp05.pdf"),
       fig_impact_psymp, width = 10, height = 5)
ggsave(file.path(paths$figures_dir, "crop_impact_by_peakday_psymp05.png"),
       fig_impact_psymp, width = 10, height = 5, dpi = 300)
cat("  Saved: crop_impact_by_peakday_psymp05.pdf/.png\n")

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

# ==============================================================================
# 8. Schematic Figure: Crop Production Loss Calculation
# ==============================================================================
# Illustrates how epidemic dynamics translate into crop productivity losses.
# Uses a single epidemic timing (strawberry worst-case) as an example.

cat("Generating schematic figure...\n")

p_symp <- 0.5

# Choose a representative epidemic timing: worst-case for strawberries
worst_peakday_straw <- impact_summary %>%
  filter(commodity == "Strawberries") %>%
  pull(worst_peakday)

# Get peak time from simulation
peaktime_sim <- epidf_with_symp %>%
  filter(subpop == "C") %>%
  filter(symp == max(symp)) %>%
  pull(t) %>%
  first()

offset <- worst_peakday_straw - peaktime_sim

# Map epidemic onto calendar
epi_mapped <- epidf_with_symp %>%
  filter(subpop == "A") %>%
  mutate(
    calendar_day = ((t + offset - 1) %% CALENDAR_DAYS) + 1,
    symp_adj = symp * p_symp,
    wf = 1 - symp_adj
  ) %>%
  group_by(calendar_day) %>%
  summarise(
    I_indiv = first(I_indiv),
    symp_adj = first(symp_adj),
    wf = first(wf),
    .groups = "drop"
  ) %>%
  arrange(calendar_day)

# Also get community epidemic for panel (b)
epi_mapped_C <- epidf_with_symp %>%
  filter(subpop == "C") %>%
  mutate(calendar_day = ((t + offset - 1) %% CALENDAR_DAYS) + 1) %>%
  group_by(calendar_day) %>%
  summarise(I_indiv = first(I_indiv), .groups = "drop") %>%
  arrange(calendar_day)

# Compute adjusted daily movements
crop_adjusted <- avg_movements_daily %>%
  left_join(epi_mapped %>% select(day = calendar_day, wf), by = "day") %>%
  mutate(wf = if_else(is.na(wf), 1.0, wf)) %>%
  mutate(lbs_adj = lbs * wf)

# Prepare ribbon data: join unadjusted and adjusted movements for shading the gap
ribbon_data <- crop_adjusted %>%
  mutate(lbs_orig = lbs / 1e6, lbs_adj_m = lbs_adj / 1e6)

# Panel (a): Average daily crop movements
panel_a <- avg_movements_daily %>%
  ggplot(aes(x = day, y = lbs / 1e6, color = commodity)) +
  geom_line(linewidth = 0.6, alpha = 0.8) +
  scale_color_manual(values = crop_colors) +
  expand_limits(y = 0) +
  labs(x = NULL, y = "Daily Shipments\n(Million lbs)", color = "Commodity",
       title = "(a) Average daily harvest volume") +
  theme_classic() +
  theme(legend.position = "bottom", axis.text.x = element_blank(),
        plot.title = element_text(size = 10, face = "bold"))

# Panel (b): Epidemic curve (I_indiv) for both subpopulations
panel_b <- ggplot() +
  geom_line(data = epi_mapped, aes(x = calendar_day, y = I_indiv),
            color = "#377EB8", linewidth = 0.8) +
  geom_line(data = epi_mapped_C, aes(x = calendar_day, y = I_indiv),
            color = "#E41A1C", linewidth = 0.8) +
  expand_limits(y = 0) +
  labs(x = NULL, y = "Proportion\nInfected",
       title = "(b) Epidemic curve") +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        plot.title = element_text(size = 10, face = "bold"))

# Panel (d): Symptomatic infections with shading (p_symp = 0.5)
panel_d <- epi_mapped %>%
  ggplot(aes(x = calendar_day, y = symp_adj)) +
  geom_area(fill = "#377EB8", alpha = 0.2) +
  geom_line(color = "#377EB8", linewidth = 0.8) +
  expand_limits(y = 0) +
  labs(x = NULL, y = "Proportion\nSymptomatic",
       title = bquote("(c) Symptomatic agricultural workers (" * p[symp] ~ "= 0.5)")) +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        plot.title = element_text(size = 10, face = "bold"))

# Month axis breaks and labels for 4-panel schematic (defined once, reused below)
month_breaks_sch <- cumsum(c(1, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30))
month_labels_sch <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# Panel (c): Adjusted daily movements
panel_c <- ggplot() +
  # Shaded gap between unadjusted and adjusted
  geom_ribbon(data = ribbon_data,
              aes(x = day, ymin = lbs_adj_m, ymax = lbs_orig, fill = commodity),
              alpha = 0.15) +
  # Original volumes as faint lines
  geom_line(data = avg_movements_daily,
            aes(x = day, y = lbs / 1e6, color = commodity),
            linewidth = 0.4, alpha = 0.3, linetype = "dashed") +
  # Adjusted volumes as solid lines
  geom_line(data = crop_adjusted,
            aes(x = day, y = lbs_adj / 1e6, color = commodity),
            linewidth = 0.6, alpha = 0.8) +
  scale_color_manual(values = crop_colors) +
  scale_fill_manual(values = crop_colors, guide = "none") +
  scale_x_continuous(breaks = month_breaks_sch, labels = month_labels_sch,
                     minor_breaks = NULL) +
  expand_limits(y = 0) +
  labs(x = NULL, y = "Daily Shipments\n(Million lbs)", color = "Commodity",
       title = "(d) Adjusted harvest volume (accounting for workforce loss)") +
  theme_classic() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 10, face = "bold"))

# Combine panels
fig_schematic <- patchwork::wrap_plots(panel_a, panel_b, panel_d, panel_c, ncol = 1) +
  patchwork::plot_annotation(
    title = "Schematic: Translating Epidemic Dynamics into Crop Production Losses",
    subtitle = paste0("Example: epidemic peak aligned to day ", worst_peakday_straw,
                      " (worst case for strawberries), R0 = ", default_pars$r0),
    theme = theme(
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 10, color = "grey40")
    )
  )

ggsave(file.path(paths$figures_dir, "crop_schematic.pdf"),
       fig_schematic, width = 10, height = 12)
ggsave(file.path(paths$figures_dir, "crop_schematic.png"),
       fig_schematic, width = 10, height = 12, dpi = 300)
cat("  Saved: crop_schematic.pdf/.png\n")

# ==============================================================================
# 9. Combined Schematic: Epidemic Overlaid on Crop Movements
# ==============================================================================
# Single-panel figure with crop movements, epidemic curves, and symptomatic
# shading all overlaid. Uses a secondary y-axis for the epidemic/symptomatic
# proportions.

cat("Generating combined schematic figure...\n")

# Scaling factor to map epidemic proportions onto the crop lbs axis
max_lbs <- max(avg_movements_daily$lbs / 1e6)
max_epi <- max(epi_mapped$I_indiv, epi_mapped_C$I_indiv, na.rm = TRUE)
epi_scale <- max_lbs / max_epi * 0.5  # scale so epidemic peak is ~90% of crop max

# Month axis breaks and labels (day 1 = Jan 1)
month_breaks <- cumsum(c(1, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30))  # day 1 of each month
month_labels <- c("Jan", "", "", "Apr", "", "",
                  "Jul", "", "", "Oct", "", "")

fig_schematic_combined <- ggplot() +
  # Symptomatic shading (scaled to secondary axis)
  geom_area(data = epi_mapped,
            aes(x = calendar_day, y = symp_adj * epi_scale),
            fill = "#377EB8", alpha = 0.15) +
  # Shaded gap between unadjusted and adjusted crop movements
  geom_ribbon(data = ribbon_data,
              aes(x = day, ymin = lbs_adj_m, ymax = lbs_orig, fill = commodity),
              alpha = 0.15) +
  # Unadjusted crop movements: solid lines
  geom_line(data = avg_movements_daily,
            aes(x = day, y = lbs / 1e6, color = commodity),
            linewidth = 0.7, alpha = 0.8) +
  # Adjusted crop movements: dashed lines
  geom_line(data = crop_adjusted,
            aes(x = day, y = lbs_adj / 1e6, color = commodity),
            linewidth = 0.7, alpha = 0.8, linetype = "dashed") +
  # Epidemic curves (scaled to secondary axis)
  geom_line(data = epi_mapped,
            aes(x = calendar_day, y = I_indiv * epi_scale),
            color = "#377EB8", linewidth = 0.9, alpha = 0.8) +
  geom_line(data = epi_mapped_C,
            aes(x = calendar_day, y = I_indiv * epi_scale),
            color = "#E41A1C", linewidth = 0.9, alpha = 0.8) +
  # Symptomatic curve outline (scaled)
  geom_line(data = epi_mapped,
            aes(x = calendar_day, y = symp_adj * epi_scale),
            color = "#377EB8", linewidth = 0.6, alpha = 0.5, linetype = "dotted") +
  # Dual axes
  scale_y_continuous(
    name = "Daily Shipments (Million lbs)",
    expand = expansion(mult = c(0, 0.05)),
    sec.axis = sec_axis(~ . / epi_scale, name = "Proportion Infected / Symptomatic")
  ) +
  scale_x_continuous(breaks = month_breaks, labels = month_labels,
                     minor_breaks = NULL) +
  scale_color_manual(values = crop_colors) +
  scale_fill_manual(values = crop_colors, guide = "none") +
  expand_limits(y = 0) +
  labs(
    x = NULL,
    color = "Commodity",
    title = "Crop Production Losses from Epidemic-Driven Workforce Reduction",
    subtitle = paste0("Solid = baseline harvest; Dashed = adjusted for illness. ",
                      "Epidemic peak day ", worst_peakday_straw,
                      ", R0 = ", default_pars$r0,
                      ", p_symp = ", p_symp)
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 9, color = "grey40"),
    axis.title.y.right = element_text(color = "grey40"),
    axis.text.y.right = element_text(color = "grey40")
  )

ggsave(file.path(paths$figures_dir, "crop_schematic_combined.pdf"),
       fig_schematic_combined, width = 10, height = 6)
ggsave(file.path(paths$figures_dir, "crop_schematic_combined.png"),
       fig_schematic_combined, width = 10, height = 6, dpi = 300)
cat("  Saved: crop_schematic_combined.pdf/.png\n")

# ==============================================================================
# 10. Combined Schematic: Epidemic Peaking June 1st
# ==============================================================================

cat("Generating combined schematic (June 1 peak)...\n")

june1_peakday <- 152  # Jan(31) + Feb(28) + Mar(31) + Apr(30) + May(31) + 1

offset_june <- june1_peakday - peaktime_sim

# Map epidemic onto calendar with June 1 peak
epi_mapped_june <- epidf_with_symp %>%
  filter(subpop == "A") %>%
  mutate(
    calendar_day = ((t + offset_june - 1) %% CALENDAR_DAYS) + 1,
    symp_adj = symp * p_symp,
    wf = 1 - symp_adj
  ) %>%
  group_by(calendar_day) %>%
  summarise(
    I_indiv = first(I_indiv),
    symp_adj = first(symp_adj),
    wf = first(wf),
    .groups = "drop"
  ) %>%
  arrange(calendar_day)

epi_mapped_C_june <- epidf_with_symp %>%
  filter(subpop == "C") %>%
  mutate(calendar_day = ((t + offset_june - 1) %% CALENDAR_DAYS) + 1) %>%
  group_by(calendar_day) %>%
  summarise(I_indiv = first(I_indiv), .groups = "drop") %>%
  arrange(calendar_day)

# Adjusted crop movements for June peak
crop_adjusted_june <- avg_movements_daily %>%
  left_join(epi_mapped_june %>% select(day = calendar_day, wf), by = "day") %>%
  mutate(wf = if_else(is.na(wf), 1.0, wf)) %>%
  mutate(lbs_adj = lbs * wf)

ribbon_data_june <- crop_adjusted_june %>%
  mutate(lbs_orig = lbs / 1e6, lbs_adj_m = lbs_adj / 1e6)

# Reuse same epi_scale from section 9
fig_schematic_june <- ggplot() +
  geom_area(data = epi_mapped_june,
            aes(x = calendar_day, y = symp_adj * epi_scale),
            fill = "#377EB8", alpha = 0.15) +
  geom_ribbon(data = ribbon_data_june,
              aes(x = day, ymin = lbs_adj_m, ymax = lbs_orig, fill = commodity),
              alpha = 0.15) +
  geom_line(data = avg_movements_daily,
            aes(x = day, y = lbs / 1e6, color = commodity),
            linewidth = 0.7, alpha = 0.8) +
  geom_line(data = crop_adjusted_june,
            aes(x = day, y = lbs_adj / 1e6, color = commodity),
            linewidth = 0.7, alpha = 0.8, linetype = "dashed") +
  geom_line(data = epi_mapped_june,
            aes(x = calendar_day, y = I_indiv * epi_scale),
            color = "#377EB8", linewidth = 0.9, alpha = 0.8) +
  geom_line(data = epi_mapped_C_june,
            aes(x = calendar_day, y = I_indiv * epi_scale),
            color = "#E41A1C", linewidth = 0.9, alpha = 0.8) +
  geom_line(data = epi_mapped_june,
            aes(x = calendar_day, y = symp_adj * epi_scale),
            color = "#377EB8", linewidth = 0.6, alpha = 0.5, linetype = "dotted") +
  scale_y_continuous(
    name = "Daily Shipments (Million lbs)",
    expand = expansion(mult = c(0, 0.05)),
    sec.axis = sec_axis(~ . / epi_scale, name = "Proportion Infected / Symptomatic")
  ) +
  scale_x_continuous(breaks = month_breaks, labels = month_labels,
                     minor_breaks = NULL) +
  scale_color_manual(values = crop_colors) +
  scale_fill_manual(values = crop_colors, guide = "none") +
  expand_limits(y = 0) +
  labs(
    x = NULL,
    color = "Commodity",
    title = "Crop Production Losses from Epidemic-Driven Workforce Reduction",
    subtitle = paste0("Solid = baseline harvest; Dashed = adjusted for illness. ",
                      "Epidemic peak June 1 (day ", june1_peakday, ")",
                      ", R0 = ", default_pars$r0,
                      ", p_symp = ", p_symp)
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 9, color = "grey40"),
    axis.title.y.right = element_text(color = "grey40"),
    axis.text.y.right = element_text(color = "grey40")
  )

ggsave(file.path(paths$figures_dir, "crop_schematic_june1.pdf"),
       fig_schematic_june, width = 10, height = 6)
ggsave(file.path(paths$figures_dir, "crop_schematic_june1.png"),
       fig_schematic_june, width = 10, height = 6, dpi = 300)
cat("  Saved: crop_schematic_june1.pdf/.png\n")
