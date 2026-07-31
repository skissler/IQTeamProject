# ==============================================================================
# Interactive Epidemic Model Explorer
# ==============================================================================
#
# A Shiny web application for exploring the household-structured epidemic model
# comparing disease dynamics between agricultural workers and the general
# population across NAWS regions.
#
# To run locally:
#   shiny::runApp("app")
#
# To deploy to shinyapps.io:
#   rsconnect::deployApp("app")
#
# ==============================================================================

library(shiny)
library(tidyverse)
library(odin)
library(plotly)
library(markdown)
library(patchwork)
library(pkgbuild)
library(pkgload)

# ==============================================================================
# Load Bundled Data
# ==============================================================================

acs_regional   <- read_csv("data/acs_data_regional.csv", show_col_types = FALSE)
naws_regional  <- read_csv("data/naws_data.csv", show_col_types = FALSE)
region_map     <- read_csv("data/region_map.csv", show_col_types = FALSE)
avg_movements_daily <- read_csv("data/avg_movements_daily.csv", show_col_types = FALSE)

# Region display order (matches manuscript)
region_order <- c("East", "Southeast", "Midwest", "Southwest", "Northwest", "California")

# ==============================================================================
# Calibrated Beta Values
# ==============================================================================
# Produced by code/calibrate_model.R using bisection search
# Full grid of (R0, SAR, fold_diff) -> beta for recalibrated sensitivity runs
calibrated_betas_df <- read_csv("data/calibrated_betas.csv", show_col_types = FALSE)

#' Look up calibrated beta using exact match on all dimensions
#'
#' @param df Data frame with columns r0, sar_crowded, fold_diff, gamma, beta
#' @param r0 Target R0
#' @param sar_crowded SAR for crowded households
#' @param fold_diff Crowding fold difference
#' @param gamma Recovery rate (default 1/5)
#' @return Calibrated beta value (exact match)
lookup_beta_app <- function(df, r0, sar_crowded, fold_diff, gamma = 1/5) {
  row <- df[df$r0 == as.numeric(r0) &
            abs(df$sar_crowded - sar_crowded) < 1e-10 &
            df$fold_diff == fold_diff &
            abs(df$gamma - gamma) < 1e-10, ]
  if (nrow(row) == 0) {
    stop("No calibrated beta found for R0=", r0,
         ", SAR_crowded=", sar_crowded,
         ", fold_diff=", fold_diff,
         ", gamma=", gamma)
  }
  row$beta[1]
}

# ==============================================================================
# Color Convention: A = blue, C = red (matches manuscript)
# ==============================================================================
pop_colors <- c("A" = "#377EB8", "C" = "#E41A1C")
pop_labels <- c("A" = "Agricultural Workers", "C" = "General Population")

crop_colors <- c(
  "Oranges" = "orange",
  "Strawberries" = "magenta",
  "Lettuce, Iceberg" = "blue"
)

# All available commodities from bundled data
all_commodities <- sort(unique(avg_movements_daily$commodity))

# Default commodities to show
default_commodities <- c("Strawberries", "Lettuce, Iceberg", "Oranges")

CALENDAR_DAYS <- 364

# ==============================================================================
# Helper Functions (from code/utils.R)
# ==============================================================================

generate_household_state_table <- function(n_min = 1, n_max = 7, crowding = FALSE) {
  states <- expand.grid(x = 0:n_max, y = 0:n_max, z = 0:n_max) %>%
    dplyr::mutate(hh_size = x + y + z) %>%
    dplyr::filter(hh_size <= n_max, hh_size >= n_min) %>%
    dplyr::arrange(hh_size, x, y, z) %>%
    dplyr::mutate(state_index = dplyr::row_number())

  find_index <- function(x_, y_, z_) {
    idx <- states %>% dplyr::filter(x == x_, y == y_, z == z_) %>% dplyr::pull(state_index)
    if (length(idx) == 0) return(0) else return(idx)
  }

  states <- states %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      rec_index = if (z > 0 && y < n_max) find_index(x, y + 1, z - 1) else 0,
      inf_index = if (y > 0 && x < n_max) find_index(x + 1, y - 1, z) else 0
    ) %>%
    dplyr::ungroup()

  if (crowding) {
    n_states <- nrow(states)
    states_crowded <- states %>%
      dplyr::mutate(state_index = state_index + n_states) %>%
      dplyr::mutate(rec_index = dplyr::case_when(rec_index > 0 ~ rec_index + n_states, TRUE ~ 0)) %>%
      dplyr::mutate(inf_index = dplyr::case_when(inf_index > 0 ~ inf_index + n_states, TRUE ~ 0))
    states <- dplyr::bind_rows(
      dplyr::mutate(states, crowded = 0),
      dplyr::mutate(states_crowded, crowded = 1)
    )
  }
  return(states)
}

adjust_crowding <- function(df, fold_diff = 1, n_max = 7, indexcols = NULL) {
  out <- df %>%
    dplyr::mutate(multiplier = (1 + (fold_diff - 1) * (hhSize - 2) / (n_max - 2))) %>%
    dplyr::mutate(multiplier = dplyr::case_when(hhSize == 1 ~ 0, TRUE ~ multiplier)) %>%
    dplyr::mutate(denom = prop * multiplier) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(indexcols))) %>%
    dplyr::mutate(c = prop_crowded / sum(denom)) %>%
    dplyr::mutate(prop_crowded_adj = c * multiplier) %>%
    dplyr::select(-multiplier, -denom, -c)
  return(out)
}

make_ic_joiner <- function(dat, fold_diff = 1, n_max = 7, indexcols = NULL) {
  dat <- adjust_crowding(dat, fold_diff = fold_diff, n_max = n_max, indexcols = indexcols)
  dat_c0 <- dplyr::mutate(dat, x = hhSize, y = 0, z = 0, crowded = 0,
                            frac = prop * (1 - prop_crowded_adj))
  dat_c1 <- dplyr::mutate(dat, x = hhSize, y = 0, z = 0, crowded = 1,
                            frac = prop * prop_crowded_adj)
  out <- dplyr::bind_rows(dat_c0, dat_c1) %>%
    dplyr::select(x, y, z, hh_size = hhSize, crowded, frac)
  return(out)
}

format_output_hh <- function(model_output, household_states) {
  out <- model_output %>%
    tidyr::pivot_longer(-t, names_to = "state_index", values_to = "prop_hh") %>%
    dplyr::mutate(subpop = substr(state_index, 3, 3)) %>%
    dplyr::mutate(state_index = as.numeric(substr(state_index, 5, nchar(state_index) - 1))) %>%
    dplyr::left_join(
      dplyr::select(household_states, x, y, z, hh_size, state_index, crowded),
      by = "state_index"
    )
  return(out)
}

format_output_indiv <- function(model_output, household_states) {
  out_hh <- format_output_hh(model_output, household_states)
  out <- out_hh %>%
    dplyr::mutate(S_num = prop_hh * x, I_num = prop_hh * y, R_num = prop_hh * z, den = prop_hh * hh_size) %>%
    dplyr::group_by(t, subpop) %>%
    dplyr::summarise(S_num = sum(S_num), I_num = sum(I_num), R_num = sum(R_num), den = sum(den), .groups = "drop") %>%
    dplyr::mutate(S_indiv = S_num / den, I_indiv = I_num / den, R_indiv = R_num / den) %>%
    dplyr::select(t, subpop, S_indiv, I_indiv, R_indiv)
  return(out)
}

calculate_tau <- function(sar, gamma) {
  sar * gamma / (1 - sar)
}

calculate_tau_boost <- function(sar_crowded, gamma, tau) {
  tau_crowded <- calculate_tau(sar_crowded, gamma)
  tau_crowded - tau
}

# Compute p_symp for a subpopulation given obesity prevalence and OR.
# Anchored so that community (obs_C = 0.40) yields p_symp_C = 0.50.
# Returns p_symp_C_anchor when OR = 1 (no effect of obesity).
compute_p_symp <- function(obs, or_obesity,
                            obs_C_anchor    = 0.40,
                            p_symp_C_anchor = 0.50) {
  if (abs(or_obesity - 1) < 1e-10) return(p_symp_C_anchor)
  a  <- (1 - obs_C_anchor) * (or_obesity - 1)
  b  <- (obs_C_anchor - p_symp_C_anchor) * (or_obesity - 1) + 1
  cc <- -p_symp_C_anchor
  disc <- b^2 - 4 * a * cc
  if (disc < 0) stop("No real solution in compute_p_symp")
  p0 <- (-b + sqrt(disc)) / (2 * a)
  p1 <- or_obesity * p0 / (1 + (or_obesity - 1) * p0)
  obs * p1 + (1 - obs) * p0
}

# ==============================================================================
# Pre-generate Household State Table (once at startup)
# ==============================================================================

household_states <- generate_household_state_table(n_min = 1, n_max = 7, crowding = TRUE)
n_states_global <- nrow(household_states)

# ==============================================================================
# Define the Epidemic Model (odin) — matches code/epimodels.R
# Vaccination modeled as a leaky vaccine: force of infection scaled by
# vax_mult_x = 1 - vax_eff * vax_cov_x (passed in from run_simulation).
# ==============================================================================

household_model_twopop_crowding <- odin::odin({
  n_states <- user()
  x[] <- user()
  y[] <- user()
  z[] <- user()
  hh_size[] <- user()
  crowded[] <- user()
  rec_index[] <- user()
  inf_index[] <- user()
  init_C[] <- user()
  init_A[] <- user()

  gamma <- user()
  tau <- user()
  tau_boost <- user()
  beta <- user()
  eps <- user()
  pop_C <- user()
  pop_A <- user()
  vax_mult_C <- user()
  vax_mult_A <- user()

  dim(x) <- n_states
  dim(y) <- n_states
  dim(z) <- n_states
  dim(hh_size) <- n_states
  dim(crowded) <- n_states
  dim(rec_index) <- n_states
  dim(inf_index) <- n_states
  dim(init_C) <- n_states
  dim(init_A) <- n_states
  dim(H_C) <- n_states
  dim(H_A) <- n_states

  initial(H_C[]) <- init_C[i]
  initial(H_A[]) <- init_A[i]

  dim(I_num_C) <- n_states
  dim(I_den_C) <- n_states
  dim(I_num_A) <- n_states
  dim(I_den_A) <- n_states

  I_num_C[] <- H_C[i] * y[i]
  I_den_C[] <- H_C[i] * hh_size[i]
  I_C <- sum(I_num_C) / sum(I_den_C)

  I_num_A[] <- H_A[i] * y[i]
  I_den_A[] <- H_A[i] * hh_size[i]
  I_A <- sum(I_num_A) / sum(I_den_A)

  w_C <- pop_C / (pop_C + pop_A)
  w_A <- pop_A / (pop_C + pop_A)

  m_CC <- (1 - eps) + eps * w_C
  m_CA <- eps * w_A
  m_AC <- eps * w_C
  m_AA <- (1 - eps) + eps * w_A

  lambda_C <- beta * (m_CC * I_C + m_CA * I_A) * vax_mult_C
  lambda_A <- beta * (m_AC * I_C + m_AA * I_A) * vax_mult_A

  deriv(H_C[]) <-
    gamma * (-y[i] * H_C[i] + if (rec_index[i] > 0) (y[i] + 1) * H_C[rec_index[i]] else 0) +
    (tau + tau_boost*crowded[i]) * (-x[i] * y[i] * H_C[i] + if (inf_index[i] > 0) (x[i] + 1) * (y[i] - 1) * H_C[inf_index[i]] else 0) +
    lambda_C * (-x[i] * H_C[i] + if (inf_index[i] > 0) (x[i] + 1) * H_C[inf_index[i]] else 0)

  deriv(H_A[]) <-
    gamma * (-y[i] * H_A[i] + if (rec_index[i] > 0) (y[i] + 1) * H_A[rec_index[i]] else 0) +
    (tau + tau_boost*crowded[i]) * (-x[i] * y[i] * H_A[i] + if (inf_index[i] > 0) (x[i] + 1) * (y[i] - 1) * H_A[inf_index[i]] else 0) +
    lambda_A * (-x[i] * H_A[i] + if (inf_index[i] > 0) (x[i] + 1) * H_A[inf_index[i]] else 0)
})

# ==============================================================================
# Simulation Function
# ==============================================================================

run_simulation <- function(region, r0, eta, sar_crowded,
                           crowding_fold_diff, gamma = 1/5, sim_days = 365,
                           vax_eff = 0.60, vax_cov_C = 0.50, vax_cov_A = 0.40) {

  sar_uncrowded <- 0.20  # Fixed: changing without recalibration is invalid
  eps <- 1 - eta  # Convert eta (assortativity) to eps (mixing parameter)
  beta <- lookup_beta_app(calibrated_betas_df, r0, sar_crowded, crowding_fold_diff, gamma)
  tau <- calculate_tau(sar_uncrowded, gamma)
  tau_boost <- calculate_tau_boost(sar_crowded, gamma, tau)
  init_prev <- 0.001

  # Leaky vaccine: force of infection is multiplied by 1 - vax_eff * vax_cov
  vax_mult_C <- 1 - vax_eff * vax_cov_C
  vax_mult_A <- 1 - vax_eff * vax_cov_A

  # Region-specific community data (ACS)
  acs_region <- acs_regional %>% dplyr::filter(REGION6 == region)
  # Region-specific ag worker data (NAWS)
  naws_region <- naws_regional %>% dplyr::filter(REGION6 == region)

  # Region-specific populations
  pop_total <- acs_region$population[1]
  prop_ag   <- acs_region$prop_ag_workers[1]
  pop_C <- pop_total * (1 - prop_ag)
  pop_A <- pop_total * prop_ag

  # Create initial conditions
  ic_joiner_C <- make_ic_joiner(acs_region, fold_diff = crowding_fold_diff)
  ic_joiner_A <- make_ic_joiner(naws_region, fold_diff = crowding_fold_diff)

  # Seed infection
  ic_joiner_C_inf <- ic_joiner_C %>%
    dplyr::mutate(frac = init_prev * frac * hh_size, y = y + 1, x = x - 1)
  ic_joiner_C$frac <- ic_joiner_C$frac - ic_joiner_C_inf$frac
  ic_joiner_C <- dplyr::bind_rows(ic_joiner_C, ic_joiner_C_inf)

  ic_joiner_A_inf <- ic_joiner_A %>%
    dplyr::mutate(frac = init_prev * frac * hh_size, y = y + 1, x = x - 1)
  ic_joiner_A$frac <- ic_joiner_A$frac - ic_joiner_A_inf$frac
  ic_joiner_A <- dplyr::bind_rows(ic_joiner_A, ic_joiner_A_inf)

  init_C <- household_states %>%
    dplyr::left_join(ic_joiner_C, by = c("x", "y", "z", "hh_size", "crowded")) %>%
    dplyr::arrange(state_index) %>%
    tidyr::replace_na(list(frac = 0)) %>%
    dplyr::pull(frac)

  init_A <- household_states %>%
    dplyr::left_join(ic_joiner_A, by = c("x", "y", "z", "hh_size", "crowded")) %>%
    dplyr::arrange(state_index) %>%
    tidyr::replace_na(list(frac = 0)) %>%
    dplyr::pull(frac)

  # Run model
  mod <- household_model_twopop_crowding$new(
    n_states = n_states_global,
    x = household_states$x,
    y = household_states$y,
    z = household_states$z,
    hh_size = household_states$hh_size,
    crowded = household_states$crowded,
    rec_index = household_states$rec_index,
    inf_index = household_states$inf_index,
    init_C = init_C,
    init_A = init_A,
    gamma = gamma,
    tau = tau,
    tau_boost = tau_boost,
    beta = beta,
    eps = eps,
    pop_C = pop_C,
    pop_A = pop_A,
    vax_mult_C = vax_mult_C,
    vax_mult_A = vax_mult_A
  )

  times <- seq(0, sim_days, by = 1)
  out <- as_tibble(data.frame(mod$run(times)))
  epidf <- format_output_indiv(out, household_states)

  return(epidf)
}

# ==============================================================================
# Crop Impact Functions (adapted from code/crop_calendars.R)
# ==============================================================================

compute_symptomatic <- function(epidf) {
  symp_temp <- epidf %>%
    dplyr::group_by(subpop) %>%
    dplyr::arrange(t) %>%
    dplyr::mutate(Inew = dplyr::lag(S_indiv) - S_indiv) %>%
    tidyr::replace_na(list(Inew = 0)) %>%
    dplyr::mutate(symp_start = t + 1, symp_end = t + 3) %>%
    dplyr::select(subpop, Inew, symp_start, symp_end)

  epidf_with_symp <- epidf %>%
    dplyr::full_join(symp_temp, by = "subpop", relationship = "many-to-many") %>%
    dplyr::mutate(tosum = dplyr::case_when(t >= symp_start & t <= symp_end ~ Inew, TRUE ~ 0)) %>%
    dplyr::group_by(t, subpop) %>%
    dplyr::summarise(
      S_indiv = dplyr::first(S_indiv),
      I_indiv = dplyr::first(I_indiv),
      R_indiv = dplyr::first(R_indiv),
      symp = sum(tosum),
      .groups = "drop"
    )

  return(epidf_with_symp)
}

get_impact_for_app <- function(peakday, p_symp, movements_daily, epidf_with_symp) {

  # Align to peak symptomatic infections in C (matches code/crop_calendars.R)
  peaktime_sim <- epidf_with_symp %>%
    dplyr::ungroup() %>%
    dplyr::filter(subpop == "C") %>%
    dplyr::filter(symp == max(symp)) %>%
    dplyr::pull(t) %>%
    dplyr::first()

  wf_epidemic <- epidf_with_symp %>%
    dplyr::filter(subpop == "A") %>%
    dplyr::mutate(wf = 1 - symp * p_symp) %>%
    dplyr::select(t_sim = t, wf) %>%
    dplyr::ungroup()

  offset <- peakday - peaktime_sim

  wf_mapped <- wf_epidemic %>%
    dplyr::mutate(calendar_day = ((t_sim + offset - 1) %% CALENDAR_DAYS) + 1) %>%
    dplyr::group_by(calendar_day) %>%
    dplyr::summarise(wf = dplyr::first(wf), .groups = "drop")

  labor_shortage_df <- movements_daily %>%
    dplyr::left_join(wf_mapped, by = c("day" = "calendar_day")) %>%
    dplyr::mutate(wf = dplyr::if_else(is.na(wf), 1.0, wf)) %>%
    dplyr::mutate(lbs_adj = lbs * wf)

  impact_df <- labor_shortage_df %>%
    dplyr::group_by(commodity) %>%
    dplyr::summarise(
      lbs_total = sum(lbs, na.rm = TRUE),
      lbs_adjusted = sum(lbs_adj, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(pct_loss = (1 - lbs_adjusted / lbs_total) * 100)

  return(impact_df)
}

get_schematic_data <- function(peakday, p_symp, movements_daily, epidf_with_symp) {

  # Align to peak symptomatic infections in C (matches code/crop_calendars.R)
  peaktime_sim <- epidf_with_symp %>%
    dplyr::ungroup() %>%
    dplyr::filter(subpop == "C") %>%
    dplyr::filter(symp == max(symp)) %>%
    dplyr::pull(t) %>%
    dplyr::first()

  offset <- peakday - peaktime_sim

  epi_mapped_A <- epidf_with_symp %>%
    dplyr::filter(subpop == "A") %>%
    dplyr::mutate(
      calendar_day = ((t + offset - 1) %% CALENDAR_DAYS) + 1,
      symp_adj = symp * p_symp,
      wf = 1 - symp_adj
    ) %>%
    dplyr::group_by(calendar_day) %>%
    dplyr::summarise(
      I_indiv = dplyr::first(I_indiv),
      symp_adj = dplyr::first(symp_adj),
      wf = dplyr::first(wf),
      .groups = "drop"
    ) %>%
    dplyr::arrange(calendar_day)

  epi_mapped_C <- epidf_with_symp %>%
    dplyr::filter(subpop == "C") %>%
    dplyr::mutate(
      calendar_day = ((t + offset - 1) %% CALENDAR_DAYS) + 1,
      symp_adj = symp * p_symp
    ) %>%
    dplyr::group_by(calendar_day) %>%
    dplyr::summarise(
      I_indiv = dplyr::first(I_indiv),
      symp_adj = dplyr::first(symp_adj),
      .groups = "drop"
    ) %>%
    dplyr::arrange(calendar_day)

  crop_adjusted <- movements_daily %>%
    dplyr::left_join(epi_mapped_A %>% dplyr::select(day = calendar_day, wf), by = "day") %>%
    dplyr::mutate(wf = dplyr::if_else(is.na(wf), 1.0, wf)) %>%
    dplyr::mutate(lbs_adj = lbs * wf)

  list(
    epi_A = epi_mapped_A,
    epi_C = epi_mapped_C,
    crop_adjusted = crop_adjusted,
    crop_original = movements_daily
  )
}

# ==============================================================================
# UI
# ==============================================================================

ui <- fluidPage(
  titlePanel("Modeling the impact of respiratory disease outbreaks on the United States agricultural workforce"),
  # tags$p("See the accompanying paper by Bardsley, de Pablo, Canada et al."),
  tags$hr(),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      h4("Transmission Parameters"),

      selectInput("r0", HTML("Basic Reproduction Number (R<sub>0</sub>)"),
                  choices = c("1.2", "1.5", "2", "3"), selected = "1.5"),

      selectInput("infectious_period", "Infectious Period (1/γ, days)",
                  choices = c("3" = 1/3, "5" = 1/5, "10" = 1/10),
                  selected = 1/5),

      selectInput("eta", "Assortativity (η)",
                  choices = c("0 (proportional mixing)" = 0,
                              "1/4" = 0.25,
                              "1/3" = 1/3,
                              "1/2" = 0.5,
                              "2/3 (baseline)" = 2/3,
                              "3/4 (near-assortative)" = 0.75),
                  selected = 2/3),
      helpText("Higher values = more within-group mixing"),

      selectInput("sar_crowded", "SAR in Crowded Households",
                  choices = c("20%" = 0.2, "30%" = 0.3, "40%" = 0.4, "50%" = 0.5, "60%" = 0.6),
                  selected = 0.4),

      selectInput("crowding_fold", "Crowding Fold Difference",
                  choices = c("1" = 1, "2" = 2, "3" = 3),
                  selected = 2),
      helpText("How much more likely large (size 7+) households are to be crowded vs. small (size 2)"),

      tags$hr(),
      h4("Vaccination"),

      selectInput("vax_eff", "Vaccine efficacy",
                  choices = c("20%" = 0.2, "40%" = 0.4, "60% (baseline)" = 0.6, "80%" = 0.8),
                  selected = 0.6),
      helpText("Proportional reduction in force of infection for vaccinated individuals"),

      selectInput("vax_cov_C", "Community vaccination coverage",
                  choices = c("30%" = 0.3, "40%" = 0.4, "50% (baseline)" = 0.5, "60%" = 0.6),
                  selected = 0.5),

      selectInput("vax_cov_A", "Agricultural worker vaccination coverage",
                  choices = c("20%" = 0.2, "40% (baseline)" = 0.4, "60%" = 0.6, "80%" = 0.8),
                  selected = 0.4),

      tags$hr(),
      h4("Comorbidity (Obesity)"),

      selectInput("obs_A", HTML("Agricultural worker obesity prevalence (obs<sub>A</sub>)"),
                  choices = c("40%" = 0.40, "50%" = 0.50, "55% (baseline)" = 0.55,
                              "60%" = 0.60, "70%" = 0.70),
                  selected = 0.55),
      helpText("Community obesity prevalence fixed at 0.40 (obs_C anchor)"),

      selectInput("or_symp", HTML("Obesity OR for symptomatic disease"),
                  choices = c("1.0 (no effect)" = 1.0, "1.5 (baseline)" = 1.5, "3.0" = 3.0),
                  selected = 1.5),

      tags$p(HTML(paste0(
        "Derived: p<sub>symp,A</sub> = ",
        textOutput("p_symp_display_sidebar", inline = TRUE),
        " &nbsp; p<sub>symp,C</sub> = 0.500"
      ))),

      tags$hr(),
      h4("Simulation Settings"),

      sliderInput("sim_days", "Simulation Duration (days)",
                  min = 100, max = 500, value = 365, step = 5),

      actionButton("run_sim", "Run Simulation", class = "btn-primary btn-block")
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        id = "main_tabs",

        # Tab 1: Epidemic Curves
        tabPanel("Epidemic Curves",
                 tags$br(),
                 plotOutput("infection_plot", height = "450px"),
                 tags$br(),
                 plotOutput("cumulative_plot", height = "450px"),
                 tags$br(),
                 plotOutput("prevalence_ratio_plot", height = "450px")
        ),

        # Tab 2: Summary Statistics
        tabPanel("Summary Statistics",
                 tags$br(),
                 tableOutput("summary_table")
        ),

        # Tab 3: Crop Impact
        tabPanel("Crop Impact",
                 tags$br(),
                 fluidRow(
                   column(4,
                     sliderInput("peakday", "Epidemic peak day (symptomatic infections in the community, day of year)",
                                 min = 1, max = 364, value = 152, step = 1),
                     tags$p(HTML(paste0(
                       "Proportion symptomatic (p<sub>symp,A</sub>): ",
                       "<strong>", textOutput("p_symp_display_crop", inline = TRUE), "</strong>"
                     ))),
                     helpText("Derived from obesity prevalence and OR inputs in the sidebar."),
                     tags$hr(),
                     h5("Commodities"),
                     selectizeInput("selected_commodities", "Select Commodities",
                                    choices = all_commodities,
                                    selected = default_commodities,
                                    multiple = TRUE,
                                    options = list(placeholder = "Type to search...")),
                     helpText("Select one or more California commodities to include in the impact analysis.")
                   ),
                   column(8,
                     plotOutput("crop_impact_plot", height = "900px"),
                     tags$br(),
                     h4("Impact Summary"),
                     tableOutput("impact_table")
                   )
                 )
        ),

        # Tab 4: About
        tabPanel("About the Model",
                 tags$br(),
                 includeMarkdown("about.md")
        )
      )
    )
  )
)

# ==============================================================================
# Server
# ==============================================================================

server <- function(input, output, session) {

  # Fixed community symptomatic fraction (anchor used in compute_p_symp)
  p_symp_C <- 0.50

  # ---- Reactive: p_symp for agricultural workers (derived from comorbidity inputs) ----
  p_symp_A <- reactive({
    compute_p_symp(obs = as.numeric(input$obs_A), or_obesity = as.numeric(input$or_symp))
  })

  # ---- Reactive: simulation results (all regions) ----
  sim_results <- reactiveVal(NULL)

  # ---- Reactive: symptomatic dynamics for California only (crop impact only) ----
  # compute_symptomatic() uses a 3-day rolling window designed for workforce modeling.
  # Epidemic curve plots use I_indiv * p_symp directly (matching plot_main_figures.R).
  symp_data <- reactiveVal(NULL)

  # ---- Reactive: movements data (filtered by selected commodities) ----
  movements_data <- reactive({
    req(input$selected_commodities)
    avg_movements_daily %>% dplyr::filter(commodity %in% input$selected_commodities)
  })

  # Helper: run all regions and store results + California symp_data
  run_all_regions <- function(r0, eta, sar_crowded, crowding_fold_diff, gamma,
                               sim_days, vax_eff, vax_cov_C, vax_cov_A) {
    results_list <- lapply(region_map$REGION6, function(reg) {
      res <- run_simulation(
        region = reg, r0 = r0, eta = eta,
        sar_crowded = sar_crowded, crowding_fold_diff = crowding_fold_diff,
        gamma = gamma, sim_days = sim_days,
        vax_eff = vax_eff, vax_cov_C = vax_cov_C, vax_cov_A = vax_cov_A
      )
      res$REGION6 <- reg
      res
    })
    result <- dplyr::bind_rows(results_list) %>%
      dplyr::left_join(region_map, by = "REGION6") %>%
      dplyr::mutate(REGION_NAME = factor(REGION_NAME, levels = region_order))
    sim_results(result)

    ca_result <- result %>%
      dplyr::filter(REGION6 == 6) %>%
      dplyr::select(t, subpop, S_indiv, I_indiv, R_indiv)
    symp_data(compute_symptomatic(ca_result))
  }

  # ---- Run simulation on button click ----
  observeEvent(input$run_sim, {
    withProgress(message = "Running simulation...", value = 0, {
      gamma <- as.numeric(input$infectious_period)
      regions <- region_map$REGION6
      for (i in seq_along(regions)) {
        incProgress(1 / length(regions),
                    detail = paste0("Region: ", region_map$REGION_NAME[region_map$REGION6 == regions[i]]))
      }
      run_all_regions(
        r0 = input$r0, eta = as.numeric(input$eta),
        sar_crowded = as.numeric(input$sar_crowded),
        crowding_fold_diff = as.numeric(input$crowding_fold),
        gamma = gamma, sim_days = input$sim_days,
        vax_eff = as.numeric(input$vax_eff),
        vax_cov_C = as.numeric(input$vax_cov_C),
        vax_cov_A = as.numeric(input$vax_cov_A)
      )
    })
  })

  # ---- Run initial simulation on app load (baseline parameters) ----
  observe({
    if (is.null(sim_results())) {
      run_all_regions(
        r0 = "1.5", eta = 2/3,
        sar_crowded = 0.40, crowding_fold_diff = 2,
        gamma = 1/5, sim_days = 365,
        vax_eff = 0.60, vax_cov_C = 0.50, vax_cov_A = 0.40
      )
    }
  })

  # ---- p_symp display outputs ----
  output$p_symp_display_sidebar <- renderText({
    sprintf("%.3f", p_symp_A())
  })

  output$p_symp_display_crop <- renderText({
    sprintf("%.3f", p_symp_A())
  })

  # ===========================================================================
  # Tab 1: Epidemic Curves
  # ===========================================================================

  # Shared theme for faceted epidemic plots
  facet_theme <- theme_minimal(base_size = 14) +
    theme(legend.position = "bottom",
          strip.text = element_text(face = "bold", size = 14),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 13),
          plot.title = element_text(size = 15))

  # Compute nice x-axis breaks based on simulation duration
  sim_x_scale <- reactive({
    days <- input$sim_days
    step <- if (days > 120) 30 else if (days >= 28) 7 else 1
    scale_x_continuous(breaks = seq(0, days, by = step), minor_breaks = NULL)
  })

  output$infection_plot <- renderPlot({
    req(sim_results())
    df <- sim_results() %>%
      dplyr::mutate(
        p_symp_sub     = dplyr::case_when(subpop == "A" ~ p_symp_A(), TRUE ~ p_symp_C),
        symp_cases_pct = I_indiv * p_symp_sub * 100
      )

    df %>%
      ggplot(aes(x = t, y = symp_cases_pct, color = subpop)) +
      geom_line(linewidth = 1.2, alpha = 0.7) +
      facet_wrap(~REGION_NAME, nrow = 2) +
      scale_color_manual(values = pop_colors, labels = pop_labels) +
      sim_x_scale() +
      coord_cartesian(xlim = c(0, input$sim_days)) +
      labs(title = "Symptomatic Cases Over Time",
           x = "Days since epidemic onset",
           y = "Symptomatic cases (% of population)",
           color = "Population") +
      facet_theme
  })

  output$cumulative_plot <- renderPlot({
    req(sim_results())
    df <- sim_results() %>%
      dplyr::mutate(
        p_symp_sub   = dplyr::case_when(subpop == "A" ~ p_symp_A(), TRUE ~ p_symp_C),
        cum_symp_pct = R_indiv * p_symp_sub * 100
      )

    df %>%
      ggplot(aes(x = t, y = cum_symp_pct, color = subpop)) +
      geom_line(linewidth = 1.2, alpha = 0.7) +
      facet_wrap(~REGION_NAME, nrow = 2) +
      scale_color_manual(values = pop_colors, labels = pop_labels) +
      sim_x_scale() +
      coord_cartesian(xlim = c(0, input$sim_days)) +
      labs(title = "Cumulative Symptomatic Cases Over Time",
           x = "Days since epidemic onset",
           y = "Cumulative symptomatic cases (% of population)",
           color = "Population") +
      facet_theme
  })

  # ===========================================================================
  # Tab 2: Summary Statistics
  # ===========================================================================

  output$summary_table <- renderTable({
    req(sim_results())
    df <- sim_results() %>%
      dplyr::mutate(
        p_symp_sub = dplyr::case_when(subpop == "A" ~ p_symp_A(), TRUE ~ p_symp_C),
        symp_cases  = I_indiv * p_symp_sub
      )

    long <- df %>%
      dplyr::group_by(REGION_NAME, subpop) %>%
      dplyr::summarise(
        peak       = round(max(symp_cases) * 100, 2),
        time_peak  = t[which.max(symp_cases)],
        final_size = round(dplyr::last(R_indiv) * dplyr::first(p_symp_sub) * 100, 2),
        .groups = "drop"
      )

    ag <- long %>% dplyr::filter(subpop == "A") %>%
      dplyr::select(REGION_NAME,
                    `Peak Symptomatic,\nAgricultural Workers (%)` = peak,
                    `Time to Peak,\nAgricultural Workers (days)` = time_peak,
                    `Cumulative Cases,\nAgricultural Workers (%)` = final_size)

    gen <- long %>% dplyr::filter(subpop == "C") %>%
      dplyr::select(REGION_NAME,
                    `Peak Symptomatic,\nCommunity (%)` = peak,
                    `Time to Peak,\nCommunity (days)` = time_peak,
                    `Cumulative Cases,\nCommunity (%)` = final_size)

    dplyr::left_join(ag, gen, by = "REGION_NAME") %>%
      dplyr::select(Region = REGION_NAME,
                    `Peak Symptomatic,\nAgricultural Workers (%)`,
                    `Peak Symptomatic,\nCommunity (%)`,
                    `Time to Peak,\nAgricultural Workers (days)`,
                    `Time to Peak,\nCommunity (days)`,
                    `Cumulative Cases,\nAgricultural Workers (%)`,
                    `Cumulative Cases,\nCommunity (%)`)
  }, striped = TRUE, hover = TRUE, bordered = TRUE,
     sanitize.colnames.function = function(x) gsub("\n", "<br/>", x))


  output$prevalence_ratio_plot <- renderPlot({
    req(sim_results())
    df <- sim_results() %>%
      dplyr::mutate(
        p_symp_sub = dplyr::case_when(subpop == "A" ~ p_symp_A(), TRUE ~ p_symp_C),
        symp_cases  = I_indiv * p_symp_sub
      )

    rel_df <- df %>%
      dplyr::select(t, subpop, symp_cases, REGION_NAME) %>%
      tidyr::pivot_wider(id_cols = c(t, REGION_NAME), names_from = subpop, values_from = symp_cases) %>%
      dplyr::mutate(prevalence_ratio = A / C) %>%
      dplyr::filter(is.finite(prevalence_ratio), C > 0.0001)

    rel_df %>%
      ggplot(aes(x = t, y = prevalence_ratio)) +
      geom_line(linewidth = 1.2, alpha = 0.7, color = "#984ea3") +
      geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
      facet_wrap(~REGION_NAME, nrow = 2) +
      sim_x_scale() +
      coord_cartesian(xlim = c(0, input$sim_days)) +
      labs(title = "Symptomatic Case Ratio (Agricultural Workers / General Population)",
           x = "Days since epidemic onset", y = "Symptomatic Case Ratio") +
      facet_theme +
      theme(legend.position = "none")
  })

  # ===========================================================================
  # Tab 3: Crop Impact
  # ===========================================================================

  # Reactive: impact across all peak days
  impact_all_peakdays <- reactive({
    req(symp_data(), movements_data())
    edf <- symp_data()
    mvd <- movements_data()
    psymp <- p_symp_A()

    dplyr::bind_rows(lapply(1:CALENDAR_DAYS, function(pd) {
      out <- get_impact_for_app(pd, psymp, mvd, edf)
      out$peakday <- pd
      out
    }))
  })

  output$crop_impact_plot <- renderPlot({
    req(impact_all_peakdays(), symp_data(), movements_data())
    idf <- impact_all_peakdays()
    psymp <- p_symp_A()
    sch <- get_schematic_data(input$peakday, psymp, movements_data(), symp_data())

    # Shared x-axis config
    month_breaks <- cumsum(c(1, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30))
    month_labels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

    # Determine commodity colors (shared by top plot and panel b)
    all_commodities_idf <- unique(idf$commodity)
    all_commodities_sch <- unique(sch$crop_original$commodity)
    all_commodities_union <- union(all_commodities_idf, all_commodities_sch)
    known <- intersect(all_commodities_union, names(crop_colors))
    unknown <- setdiff(all_commodities_union, names(crop_colors))
    extra_colors <- if (length(unknown) > 0) setNames(scales::hue_pal()(length(unknown)), unknown) else character(0)
    all_colors <- c(crop_colors[known], extra_colors)

    # --- Top plot: Production loss by peak day ---
    idf_at_peak <- idf %>% dplyr::filter(peakday == input$peakday)

    plot_top <- idf %>%
      ggplot(aes(x = peakday, y = pct_loss, color = commodity)) +
      geom_line(linewidth = 0.8, alpha = 0.8) +
      geom_vline(xintercept = input$peakday, linetype = "dotted", color = "gray40", linewidth = 0.5) +
      geom_point(data = idf_at_peak, aes(x = peakday, y = pct_loss, color = commodity),
                 size = 3) +
      scale_color_manual(values = all_colors) +
      scale_x_continuous(breaks = month_breaks, labels = month_labels, minor_breaks = NULL) +
      expand_limits(y = 0) +
      labs(x = NULL, y = "Production Loss (%)",
           color = "Commodity",
           title = paste0("Production Loss by Peak Day (p_symp,A = ",
                          round(psymp, 3), ")")) +
      theme_classic(base_size = 12) +
      theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

    # --- Panel (a): Symptomatic disease curves ---
    symp_lines <- dplyr::bind_rows(
      sch$epi_A %>% dplyr::mutate(group = "Agricultural Workers (A)"),
      sch$epi_C %>% dplyr::mutate(group = "General Population (C)")
    )
    symp_colors <- c("Agricultural Workers (A)" = "#377EB8",
                     "General Population (C)" = "#E41A1C")

    panel_a <- ggplot() +
      geom_area(data = sch$epi_A, aes(x = calendar_day, y = 100*symp_adj),
                fill = "#377EB8", alpha = 0.1) +
      geom_area(data = sch$epi_C, aes(x = calendar_day, y = 100*symp_adj),
                fill = "#E41A1C", alpha = 0.1) +
      geom_line(data = symp_lines,
                aes(x = calendar_day, y = 100*symp_adj, color = group),
                linewidth = 0.8) +
      scale_color_manual(values = symp_colors) +
      expand_limits(y = 0) +
      scale_x_continuous(breaks = month_breaks, labels = month_labels, minor_breaks = NULL) +
      labs(x = NULL, y = "Symptomatic cases\n(% of population)", color = NULL,
           title = "Work-limiting symptomatic cases") +
      theme_classic(base_size = 12) +
      theme(plot.title = element_text(face = "bold"),
            legend.position = "bottom")

    # --- Panel (b): Adjusted movements ---
    ribbon_data <- sch$crop_adjusted %>%
      dplyr::mutate(lbs_orig = lbs / 1e6, lbs_adj_m = lbs_adj / 1e6)

    panel_b <- ggplot() +
      geom_ribbon(data = ribbon_data,
                  aes(x = day, ymin = lbs_adj_m, ymax = lbs_orig, fill = commodity),
                  alpha = 0.15) +
      geom_line(data = sch$crop_original,
                aes(x = day, y = lbs / 1e6, color = commodity, linetype = "Actual"),
                linewidth = 0.6, alpha = 0.8) +
      geom_line(data = sch$crop_adjusted,
                aes(x = day, y = lbs_adj / 1e6, color = commodity, linetype = "Adjusted"),
                linewidth = 0.6, alpha = 0.4) +
      scale_color_manual(values = all_colors) +
      scale_fill_manual(values = all_colors, guide = "none") +
      scale_linetype_manual(values = c("Actual" = "solid", "Adjusted" = "dashed")) +
      scale_x_continuous(breaks = month_breaks, labels = month_labels, minor_breaks = NULL) +
      expand_limits(y = 0) +
      labs(x = NULL, y = "Daily Shipments\n(Million lbs)", color = "Commodity", linetype = NULL,
           title = "Adjusted harvest volume (accounting for workforce loss)") +
      theme_classic(base_size = 12) +
      theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

    patchwork::wrap_plots(plot_top, panel_a, panel_b, ncol = 1, axes = "collect_x")
  })

  output$impact_table <- renderTable({
    req(symp_data(), movements_data())
    impact <- get_impact_for_app(input$peakday, p_symp_A(), movements_data(), symp_data())
    impact %>%
      dplyr::mutate(
        `Total Production (Million lbs)` = round(lbs_total / 1e6, 1),
        `Adjusted Production (Million lbs)` = round(lbs_adjusted / 1e6, 1),
        `Production Loss (%)` = round(pct_loss, 2)
      ) %>%
      dplyr::select(Commodity = commodity,
                    `Total Production (Million lbs)`,
                    `Adjusted Production (Million lbs)`,
                    `Production Loss (%)`)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
}

# ==============================================================================
# Run App
# ==============================================================================

shinyApp(ui = ui, server = server)
