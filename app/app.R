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

# ==============================================================================
# Load Bundled Data
# ==============================================================================

acs_regional   <- read_csv("data/acs_data_regional.csv", show_col_types = FALSE)
naws_regional  <- read_csv("data/naws_data.csv", show_col_types = FALSE)
region_map     <- read_csv("data/region_map.csv", show_col_types = FALSE)
avg_movements_daily <- read_csv("data/avg_movements_daily.csv", show_col_types = FALSE)

# Region choices for dropdown (named vector: display name -> REGION6 code)
region_choices <- setNames(region_map$REGION6, region_map$REGION_NAME)

# ==============================================================================
# Calibrated Beta Values
# ==============================================================================
# Produced by code/calibrate_model.R using bisection search
calibrated_betas <- c("1.2" = 0.1546, "1.5" = 0.2108, "2" = 0.3078, "3" = 0.5054)

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

# ==============================================================================
# Pre-generate Household State Table (once at startup)
# ==============================================================================

household_states <- generate_household_state_table(n_min = 1, n_max = 7, crowding = TRUE)
n_states_global <- nrow(household_states)

# ==============================================================================
# Define the Epidemic Model (odin) — single beta, matches code/epimodels.R
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

  lambda_C <- beta * (m_CC * I_C + m_CA * I_A)
  lambda_A <- beta * (m_AC * I_C + m_AA * I_A)

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

run_simulation <- function(region, r0, eta, sar_uncrowded, sar_crowded,
                           crowding_fold_diff, sim_days = 365) {

  gamma <- 1 / 5
  eps <- 1 - eta  # Convert eta (assortativity) to eps (mixing parameter)
  beta <- calibrated_betas[as.character(r0)]
  tau <- calculate_tau(sar_uncrowded, gamma)
  tau_boost <- calculate_tau_boost(sar_crowded, gamma, tau)
  init_prev <- 0.001

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
    pop_A = pop_A
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
      lbs_total = sum(lbs),
      lbs_adjusted = sum(lbs_adj),
      .groups = "drop"
    ) %>%
    dplyr::mutate(pct_loss = (1 - lbs_adjusted / lbs_total) * 100)

  return(impact_df)
}

get_schematic_data <- function(peakday, p_symp, movements_daily, epidf_with_symp) {

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
    dplyr::mutate(calendar_day = ((t + offset - 1) %% CALENDAR_DAYS) + 1) %>%
    dplyr::group_by(calendar_day) %>%
    dplyr::summarise(I_indiv = dplyr::first(I_indiv), .groups = "drop") %>%
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
  titlePanel("Household-Structured Epidemic Model Explorer"),
  tags$p("Comparing disease dynamics between agricultural workers and the general population"),
  tags$hr(),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      h4("Region"),
      selectInput("region", "NAWS Region",
                  choices = region_choices, selected = 6),

      tags$hr(),
      h4("Transmission Parameters"),

      selectInput("r0", "Basic Reproduction Number (R0)",
                  choices = c("1.2", "1.5", "2", "3"), selected = "1.5"),

      sliderInput("eta", "Assortativity (eta)",
                  min = 0, max = 1, value = 0.67, step = 0.01),
      helpText("1 = assortative (groups don't mix), 0 = proportional mixing"),

      tags$hr(),
      h4("Household Transmission (SAR)"),

      sliderInput("sar_uncrowded", "SAR in Uncrowded Households",
                  min = 0.10, max = 0.40, value = 0.20, step = 0.02),

      sliderInput("sar_crowded", "SAR in Crowded Households",
                  min = 0.20, max = 0.70, value = 0.40, step = 0.02),

      tags$hr(),
      h4("Crowding Structure"),

      sliderInput("crowding_fold", "Crowding Fold Difference",
                  min = 1, max = 4, value = 2, step = 0.5),
      helpText("How much more likely large households are to be crowded vs small"),

      tags$hr(),
      h4("Simulation Settings"),

      sliderInput("sim_days", "Simulation Duration (days)",
                  min = 100, max = 500, value = 365, step = 50),

      actionButton("run_sim", "Run Simulation", class = "btn-primary btn-block")
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        id = "main_tabs",

        # Tab 1: Epidemic Curves
        tabPanel("Epidemic Curves",
                 tags$br(),
                 plotlyOutput("infection_plot", height = "400px"),
                 tags$br(),
                 plotlyOutput("cumulative_plot", height = "400px")
        ),

        # Tab 2: Summary Statistics
        tabPanel("Summary Statistics",
                 tags$br(),
                 fluidRow(
                   column(6, tableOutput("summary_table")),
                   column(6, plotOutput("comparison_plot", height = "350px"))
                 ),
                 tags$hr(),
                 h4("Interpretation"),
                 textOutput("interpretation")
        ),

        # Tab 3: Prevalence Ratio
        tabPanel("Prevalence Ratio",
                 tags$br(),
                 plotlyOutput("prevalence_ratio_plot", height = "400px"),
                 tags$hr(),
                 helpText("Prevalence ratio shows how much higher prevalence is among ",
                          "agricultural workers compared to the general population over time.")
        ),

        # Tab 4: Crop Impact
        tabPanel("Crop Impact",
                 tags$br(),
                 fluidRow(
                   column(4,
                     sliderInput("peakday", "Epidemic Peak Day (day of year)",
                                 min = 1, max = 364, value = 152, step = 1),
                     sliderInput("p_symp", "Proportion Symptomatic",
                                 min = 0.1, max = 1.0, value = 0.5, step = 0.05),
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
                     plotlyOutput("impact_by_peakday_plot", height = "350px"),
                     tags$br(),
                     plotOutput("schematic_plot", height = "500px"),
                     tags$br(),
                     h4("Impact Summary"),
                     tableOutput("impact_table")
                   )
                 )
        ),

        # Tab 5: About
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

  # ---- Reactive: simulation results ----
  sim_results <- reactiveVal(NULL)

  # ---- Reactive: symptomatic data (computed from sim results) ----
  symp_data <- reactiveVal(NULL)

  # ---- Reactive: movements data (filtered by selected commodities) ----
  movements_data <- reactive({
    req(input$selected_commodities)
    avg_movements_daily %>% dplyr::filter(commodity %in% input$selected_commodities)
  })

  # ---- Run simulation on button click ----
  observeEvent(input$run_sim, {
    withProgress(message = "Running simulation...", {
      result <- run_simulation(
        region = as.numeric(input$region),
        r0 = input$r0,
        eta = input$eta,
        sar_uncrowded = input$sar_uncrowded,
        sar_crowded = input$sar_crowded,
        crowding_fold_diff = input$crowding_fold,
        sim_days = input$sim_days
      )
      sim_results(result)

      incProgress(0.7, detail = "Computing symptomatic dynamics...")
      symp <- compute_symptomatic(result)
      symp_data(symp)
    })
  })

  # ---- Run initial simulation on app load ----
  observe({
    if (is.null(sim_results())) {
      result <- run_simulation(
        region = 6, r0 = "1.5", eta = 0.67,
        sar_uncrowded = 0.20, sar_crowded = 0.40,
        crowding_fold_diff = 2, sim_days = 365
      )
      sim_results(result)
      symp <- compute_symptomatic(result)
      symp_data(symp)
    }
  })

  # ===========================================================================
  # Tab 1: Epidemic Curves
  # ===========================================================================

  output$infection_plot <- renderPlotly({
    req(sim_results())
    df <- sim_results()

    p <- df %>%
      ggplot(aes(x = t, y = I_indiv * 100, color = subpop)) +
      geom_line(linewidth = 1) +
      scale_color_manual(values = pop_colors, labels = pop_labels) +
      labs(title = "Disease Prevalence Over Time",
           x = "Days", y = "Infected (%)", color = "Population") +
      theme_minimal() +
      theme(legend.position = "bottom")

    ggplotly(p) %>% layout(legend = list(orientation = "h", y = -0.15))
  })

  output$cumulative_plot <- renderPlotly({
    req(sim_results())
    df <- sim_results()

    p <- df %>%
      ggplot(aes(x = t, y = R_indiv * 100, color = subpop)) +
      geom_line(linewidth = 1) +
      scale_color_manual(values = pop_colors, labels = pop_labels) +
      labs(title = "Cumulative Final Size",
           x = "Days", y = "Cumulative Infected (%)", color = "Population") +
      theme_minimal() +
      theme(legend.position = "bottom")

    ggplotly(p) %>% layout(legend = list(orientation = "h", y = -0.15))
  })

  # ===========================================================================
  # Tab 2: Summary Statistics
  # ===========================================================================

  output$summary_table <- renderTable({
    req(sim_results())
    df <- sim_results()

    df %>%
      dplyr::group_by(subpop) %>%
      dplyr::summarise(
        `Peak Prevalence (%)` = round(max(I_indiv) * 100, 2),
        `Time to Peak (days)` = t[which.max(I_indiv)],
        `Final Size (%)` = round(dplyr::last(R_indiv) * 100, 2),
        .groups = "drop"
      ) %>%
      dplyr::mutate(Population = dplyr::if_else(subpop == "A", "Agricultural Workers", "General Population")) %>%
      dplyr::select(Population, everything(), -subpop)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  output$comparison_plot <- renderPlot({
    req(sim_results())
    df <- sim_results()

    summary_df <- df %>%
      dplyr::group_by(subpop) %>%
      dplyr::summarise(
        peak = max(I_indiv) * 100,
        final_size = dplyr::last(R_indiv) * 100,
        .groups = "drop"
      ) %>%
      dplyr::mutate(Population = dplyr::if_else(subpop == "A", "Agricultural\nWorkers", "General\nPopulation"))

    summary_df %>%
      tidyr::pivot_longer(cols = c(peak, final_size), names_to = "metric", values_to = "value") %>%
      dplyr::mutate(metric = dplyr::if_else(metric == "peak", "Peak Prevalence (%)", "Final Size (%)")) %>%
      ggplot(aes(x = Population, y = value, fill = Population)) +
      geom_col(width = 0.6) +
      facet_wrap(~metric, scales = "free_y") +
      scale_fill_manual(values = c("Agricultural\nWorkers" = "#377EB8", "General\nPopulation" = "#E41A1C")) +
      labs(y = "Percent", x = NULL) +
      theme_minimal() +
      theme(legend.position = "none", strip.text = element_text(face = "bold"))
  })

  output$interpretation <- renderText({
    req(sim_results())
    df <- sim_results()

    summary_df <- df %>%
      dplyr::group_by(subpop) %>%
      dplyr::summarise(
        peak = max(I_indiv) * 100,
        final_size = dplyr::last(R_indiv) * 100,
        .groups = "drop"
      )

    ag_fs <- summary_df$final_size[summary_df$subpop == "A"]
    gen_fs <- summary_df$final_size[summary_df$subpop == "C"]
    diff <- ag_fs - gen_fs

    region_name <- region_map$REGION_NAME[region_map$REGION6 == as.numeric(input$region)]

    paste0(
      "In the ", region_name, " region, agricultural workers experience a final size of ",
      round(ag_fs, 1), "% compared to ", round(gen_fs, 1),
      "% in the general population — a difference of ", round(diff, 1),
      " percentage points. This differential is driven by higher household crowding ",
      "rates among agricultural workers, which increases within-household transmission."
    )
  })

  # ===========================================================================
  # Tab 3: Prevalence Ratio
  # ===========================================================================

  output$prevalence_ratio_plot <- renderPlotly({
    req(sim_results())
    df <- sim_results()

    rel_df <- df %>%
      dplyr::select(t, subpop, I_indiv) %>%
      tidyr::pivot_wider(names_from = subpop, values_from = I_indiv) %>%
      dplyr::mutate(prevalence_ratio = A / C) %>%
      dplyr::filter(is.finite(prevalence_ratio), C > 0.001)

    p <- rel_df %>%
      ggplot(aes(x = t, y = prevalence_ratio)) +
      geom_line(linewidth = 1, color = "#984ea3") +
      geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
      labs(title = "Prevalence Ratio (Agricultural Workers / General Population)",
           x = "Days", y = "Prevalence Ratio") +
      theme_minimal()

    ggplotly(p)
  })

  # ===========================================================================
  # Tab 4: Crop Impact
  # ===========================================================================

  # Reactive: impact across all peak days
  impact_all_peakdays <- reactive({
    req(symp_data(), movements_data())
    edf <- symp_data()
    mvd <- movements_data()

    dplyr::bind_rows(lapply(1:CALENDAR_DAYS, function(pd) {
      out <- get_impact_for_app(pd, input$p_symp, mvd, edf)
      out$peakday <- pd
      out
    }))
  })

  output$impact_by_peakday_plot <- renderPlotly({
    req(impact_all_peakdays())
    idf <- impact_all_peakdays()

    month_breaks <- cumsum(c(1, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30))
    month_labels <- c("Jan", "", "", "Apr", "", "", "Jul", "", "", "Oct", "", "")

    # Determine colors: use crop_colors for known, generate for new
    all_commodities <- unique(idf$commodity)
    known <- intersect(all_commodities, names(crop_colors))
    unknown <- setdiff(all_commodities, names(crop_colors))
    extra_colors <- if (length(unknown) > 0) setNames(scales::hue_pal()(length(unknown)), unknown) else character(0)
    all_colors <- c(crop_colors[known], extra_colors)

    p <- idf %>%
      ggplot(aes(x = peakday, y = pct_loss, color = commodity)) +
      geom_line(linewidth = 0.8, alpha = 0.8) +
      scale_color_manual(values = all_colors) +
      scale_x_continuous(breaks = month_breaks, labels = month_labels, minor_breaks = NULL) +
      expand_limits(y = 0) +
      labs(x = "Epidemic Peak Timing", y = "Production Loss (%)",
           color = "Commodity",
           title = paste0("Production Loss by Peak Day (p_symp = ", input$p_symp, ")")) +
      theme_minimal() +
      theme(legend.position = "bottom")

    ggplotly(p) %>% layout(legend = list(orientation = "h", y = -0.15))
  })

  output$schematic_plot <- renderPlot({
    req(symp_data(), movements_data())

    sch <- get_schematic_data(input$peakday, input$p_symp, movements_data(), symp_data())

    # Determine colors
    all_commodities <- unique(sch$crop_original$commodity)
    known <- intersect(all_commodities, names(crop_colors))
    unknown <- setdiff(all_commodities, names(crop_colors))
    extra_colors <- if (length(unknown) > 0) setNames(scales::hue_pal()(length(unknown)), unknown) else character(0)
    all_colors <- c(crop_colors[known], extra_colors)

    ribbon_data <- sch$crop_adjusted %>%
      dplyr::mutate(lbs_orig = lbs / 1e6, lbs_adj_m = lbs_adj / 1e6)

    month_breaks <- cumsum(c(1, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30))
    month_labels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

    # Panel (a): Crop movements
    panel_a <- sch$crop_original %>%
      ggplot(aes(x = day, y = lbs / 1e6, color = commodity)) +
      geom_line(linewidth = 0.6, alpha = 0.8) +
      scale_color_manual(values = all_colors) +
      expand_limits(y = 0) +
      labs(x = NULL, y = "Daily Shipments\n(Million lbs)", color = "Commodity",
           title = "(a) Average daily harvest volume") +
      theme_classic(base_size = 12) +
      theme(legend.position = "bottom", axis.text.x = element_blank(),
            plot.title = element_text(face = "bold"))

    # Panel (b): Epidemic curve
    panel_b <- ggplot() +
      geom_line(data = sch$epi_A, aes(x = calendar_day, y = I_indiv),
                color = "#377EB8", linewidth = 0.8) +
      geom_line(data = sch$epi_C, aes(x = calendar_day, y = I_indiv),
                color = "#E41A1C", linewidth = 0.8) +
      expand_limits(y = 0) +
      labs(x = NULL, y = "Proportion\nInfected",
           title = "(b) Epidemic curve (blue=A, red=C)") +
      theme_classic(base_size = 12) +
      theme(axis.text.x = element_blank(), plot.title = element_text(face = "bold"))

    # Panel (c): Symptomatic
    panel_c <- sch$epi_A %>%
      ggplot(aes(x = calendar_day, y = symp_adj)) +
      geom_area(fill = "#377EB8", alpha = 0.2) +
      geom_line(color = "#377EB8", linewidth = 0.8) +
      expand_limits(y = 0) +
      labs(x = NULL, y = "Proportion\nSymptomatic",
           title = paste0("(c) Symptomatic ag workers (p_symp = ", input$p_symp, ")")) +
      theme_classic(base_size = 12) +
      theme(axis.text.x = element_blank(), plot.title = element_text(face = "bold"))

    # Panel (d): Adjusted movements
    panel_d <- ggplot() +
      geom_ribbon(data = ribbon_data,
                  aes(x = day, ymin = lbs_adj_m, ymax = lbs_orig, fill = commodity),
                  alpha = 0.15) +
      geom_line(data = sch$crop_original,
                aes(x = day, y = lbs / 1e6, color = commodity),
                linewidth = 0.4, alpha = 0.3, linetype = "dashed") +
      geom_line(data = sch$crop_adjusted,
                aes(x = day, y = lbs_adj / 1e6, color = commodity),
                linewidth = 0.6, alpha = 0.8) +
      scale_color_manual(values = all_colors) +
      scale_fill_manual(values = all_colors, guide = "none") +
      scale_x_continuous(breaks = month_breaks, labels = month_labels, minor_breaks = NULL) +
      expand_limits(y = 0) +
      labs(x = NULL, y = "Daily Shipments\n(Million lbs)", color = "Commodity",
           title = "(d) Adjusted harvest volume (accounting for workforce loss)") +
      theme_classic(base_size = 12) +
      theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

    patchwork::wrap_plots(panel_a, panel_b, panel_c, panel_d, ncol = 1)
  })

  output$impact_table <- renderTable({
    req(symp_data(), movements_data())
    impact <- get_impact_for_app(input$peakday, input$p_symp, movements_data(), symp_data())
    impact %>%
      dplyr::mutate(
        `Total Production (M lbs)` = round(lbs_total / 1e6, 1),
        `Adjusted Production (M lbs)` = round(lbs_adjusted / 1e6, 1),
        `Production Loss (%)` = round(pct_loss, 2)
      ) %>%
      dplyr::select(Commodity = commodity,
                    `Total Production (M lbs)`,
                    `Adjusted Production (M lbs)`,
                    `Production Loss (%)`)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
}

# ==============================================================================
# Run App
# ==============================================================================

shinyApp(ui = ui, server = server)
