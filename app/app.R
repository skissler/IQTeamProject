# ==============================================================================
# Interactive Epidemic Model Explorer
# ==============================================================================
#
# A Shiny web application for exploring the household-structured epidemic model
# comparing disease dynamics between agricultural workers and the general population.
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
# Load Model Components
# ==============================================================================

# Source utilities (need to adjust path for app context)
# These functions are copied/adapted from the main codebase

#' Generate Household State Table for Epidemic Model
generate_household_state_table <- function(n_min = 1, n_max = 8, crowding = FALSE) {
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

#' Adjust Crowding Proportions by Household Size
adjust_crowding <- function(df, fold_diff = 1, indexcols = NULL) {
  out <- df %>%
    dplyr::mutate(multiplier = (1 + (fold_diff - 1) * (hhSize - 2) / 5)) %>%
    dplyr::mutate(multiplier = dplyr::case_when(hhSize == 1 ~ 0, TRUE ~ multiplier)) %>%
    dplyr::mutate(denom = prop * multiplier) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(indexcols))) %>%
    dplyr::mutate(c = prop_crowded / sum(denom)) %>%
    dplyr::mutate(prop_crowded_adj = c * multiplier) %>%
    dplyr::select(-multiplier, -denom, -c)
  return(out)
}

#' Create Initial Condition Joiner Table
make_ic_joiner <- function(dat, fold_diff = 1, indexcols = NULL) {
  dat <- adjust_crowding(dat, fold_diff = fold_diff, indexcols = indexcols)
  dat_c0 <- dplyr::mutate(dat, x = hhSize, y = 0, z = 0, crowded = 0,
                          frac = prop * (1 - prop_crowded_adj))
  dat_c1 <- dplyr::mutate(dat, x = hhSize, y = 0, z = 0, crowded = 1,
                          frac = prop * prop_crowded_adj)
  out <- dplyr::bind_rows(dat_c0, dat_c1) %>%
    dplyr::select(x, y, z, hh_size = hhSize, crowded, frac)
  return(out)
}

#' Format Model Output at Individual Level
format_output_indiv <- function(model_output, household_states) {
  out_hh <- model_output %>%
    tidyr::pivot_longer(-t, names_to = "state_index", values_to = "prop_hh") %>%
    dplyr::mutate(subpop = substr(state_index, 3, 3)) %>%
    dplyr::mutate(state_index = as.numeric(substr(state_index, 5, nchar(state_index) - 1))) %>%
    dplyr::left_join(
      dplyr::select(household_states, x, y, z, hh_size, state_index, crowded),
      by = "state_index"
    )

  out <- out_hh %>%
    dplyr::mutate(S_num = prop_hh * x, I_num = prop_hh * y, R_num = prop_hh * z, den = prop_hh * hh_size) %>%
    dplyr::group_by(t, subpop) %>%
    dplyr::summarise(S_num = sum(S_num), I_num = sum(I_num), R_num = sum(R_num), den = sum(den), .groups = "drop") %>%
    dplyr::mutate(S_indiv = S_num / den, I_indiv = I_num / den, R_indiv = R_num / den) %>%
    dplyr::select(t, subpop, S_indiv, I_indiv, R_indiv)
  return(out)
}

# ==============================================================================
# Define the Epidemic Model (odin)
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
  tau_C <- user()
  tau_A <- user()
  tau_boost <- user()
  beta_C <- user()
  beta_A <- user()
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

  lambda_C <- beta_C * (m_CC * I_C + m_CA * I_A)
  lambda_A <- beta_A * (m_AC * I_C + m_AA * I_A)

  deriv(H_C[]) <-
    gamma * (-y[i] * H_C[i] + if (rec_index[i] > 0) (y[i] + 1) * H_C[rec_index[i]] else 0) +
    (tau_C + tau_boost*crowded[i]) * (-x[i] * y[i] * H_C[i] + if (inf_index[i] > 0) (x[i] + 1) * (y[i] - 1) * H_C[inf_index[i]] else 0) +
    lambda_C * (-x[i] * H_C[i] + if (inf_index[i] > 0) (x[i] + 1) * H_C[inf_index[i]] else 0)

  deriv(H_A[]) <-
    gamma * (-y[i] * H_A[i] + if (rec_index[i] > 0) (y[i] + 1) * H_A[rec_index[i]] else 0) +
    (tau_A + tau_boost*crowded[i]) * (-x[i] * y[i] * H_A[i] + if (inf_index[i] > 0) (x[i] + 1) * (y[i] - 1) * H_A[inf_index[i]] else 0) +
    lambda_A * (-x[i] * H_A[i] + if (inf_index[i] > 0) (x[i] + 1) * H_A[inf_index[i]] else 0)
})

# ==============================================================================
# Sample Regional Data (simplified for app)
# ==============================================================================
# In production, this would load from the actual data files.
# Here we use representative distributions for demonstration.

# Representative household distributions (simplified)
create_sample_data <- function() {
  # Community population (general US - from ACS patterns)
  community_data <- tibble(
    hhSize = 1:7,
    prop = c(0.28, 0.34, 0.15, 0.13, 0.06, 0.025, 0.015),
    prop_crowded = c(0, 0.02, 0.03, 0.04, 0.06, 0.08, 0.12)
  )

  # Agricultural workers (from NAWS patterns - higher crowding)
  ag_worker_data <- tibble(
    hhSize = 1:7,
    prop = c(0.15, 0.25, 0.18, 0.18, 0.12, 0.07, 0.05),
    prop_crowded = c(0, 0.08, 0.12, 0.18, 0.25, 0.32, 0.40)
  )

  list(community = community_data, agricultural = ag_worker_data)
}

sample_data <- create_sample_data()

# ==============================================================================
# Simulation Function
# ==============================================================================

run_simulation <- function(r0, epsilon, sar_uncrowded, sar_crowded, crowding_fold_diff,
                           infectious_period = 5, sim_days = 365) {

  gamma <- 1 / infectious_period

  # Calculate tau from SAR: SAR = tau / (tau + gamma) => tau = SAR * gamma / (1 - SAR)
  tau_base <- sar_uncrowded * gamma / (1 - sar_uncrowded)
  tau_crowded <- sar_crowded * gamma / (1 - sar_crowded)
  tau_boost <- tau_crowded - tau_base

 # Calculate beta from R0 (using calibrated scalars)
  # These are approximate - in production would use exact calibration
  beta_scalar <- case_when(
    r0 <= 1.2 ~ 0.765,
    r0 <= 1.5 ~ 0.765 + (r0 - 1.2) * (1.05 - 0.765) / 0.3,
    r0 <= 2.0 ~ 1.05 + (r0 - 1.5) * (1.53 - 1.05) / 0.5,
    r0 <= 3.0 ~ 1.53 + (r0 - 2.0) * (2.52 - 1.53) / 1.0,
    TRUE ~ 2.52
  )
  beta <- beta_scalar * gamma

  # Generate household states
  household_states <- generate_household_state_table(n_min = 1, n_max = 7, crowding = TRUE)
  n_states <- nrow(household_states)

  # Create initial conditions
  ic_joiner_C <- make_ic_joiner(sample_data$community, fold_diff = crowding_fold_diff)
  ic_joiner_A <- make_ic_joiner(sample_data$agricultural, fold_diff = crowding_fold_diff)

  init_prev <- 0.001

  # Seed infection
  ic_joiner_C_inf <- ic_joiner_C %>%
    mutate(frac = init_prev * frac * hh_size, y = y + 1, x = x - 1)
  ic_joiner_C$frac <- ic_joiner_C$frac - ic_joiner_C_inf$frac
  ic_joiner_C <- bind_rows(ic_joiner_C, ic_joiner_C_inf)

  ic_joiner_A_inf <- ic_joiner_A %>%
    mutate(frac = init_prev * frac * hh_size, y = y + 1, x = x - 1)
  ic_joiner_A$frac <- ic_joiner_A$frac - ic_joiner_A_inf$frac
  ic_joiner_A <- bind_rows(ic_joiner_A, ic_joiner_A_inf)

  init_C <- household_states %>%
    left_join(ic_joiner_C, by = c("x", "y", "z", "hh_size", "crowded")) %>%
    arrange(state_index) %>%
    replace_na(list(frac = 0)) %>%
    pull(frac)

  init_A <- household_states %>%
    left_join(ic_joiner_A, by = c("x", "y", "z", "hh_size", "crowded")) %>%
    arrange(state_index) %>%
    replace_na(list(frac = 0)) %>%
    pull(frac)

  # Run model
  mod <- household_model_twopop_crowding$new(
    n_states = n_states,
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
    tau_C = tau_base,
    tau_A = tau_base,
    tau_boost = tau_boost,
    beta_C = beta,
    beta_A = beta,
    eps = epsilon,
    pop_C = 1000000,
    pop_A = 50000
  )

  times <- seq(0, sim_days, by = 1)
  out <- as_tibble(data.frame(mod$run(times)))
  epidf <- format_output_indiv(out, household_states)

  return(epidf)
}

# ==============================================================================
# UI
# ==============================================================================

ui <- fluidPage(
  # App title
  titlePanel("Household-Structured Epidemic Model Explorer"),
  tags$p("Comparing disease dynamics between agricultural workers and the general population"),
  tags$hr(),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      h4("Transmission Parameters"),

      sliderInput("r0", "Basic Reproduction Number (R0)",
                  min = 1.1, max = 3.5, value = 1.5, step = 0.1),

      sliderInput("epsilon", "Assortativity (epsilon)",
                  min = 0, max = 1, value = 0.33, step = 0.05,
                  post = ""),
      helpText("0 = complete assortativity (groups don't mix)",
               "1 = proportional mixing"),

      tags$hr(),
      h4("Household Transmission (SAR)"),

      sliderInput("sar_uncrowded", "SAR in Uncrowded Households",
                  min = 0.10, max = 0.40, value = 0.20, step = 0.02,
                  post = ""),

      sliderInput("sar_crowded", "SAR in Crowded Households",
                  min = 0.20, max = 0.70, value = 0.40, step = 0.02,
                  post = ""),

      tags$hr(),
      h4("Crowding Structure"),

      sliderInput("crowding_fold", "Crowding Fold Difference",
                  min = 1, max = 4, value = 2, step = 0.5),
      helpText("How much more likely large households are to be crowded vs small households"),

      tags$hr(),
      h4("Simulation Settings"),

      sliderInput("sim_days", "Simulation Duration (days)",
                  min = 100, max = 500, value = 365, step = 50),

      actionButton("run_sim", "Run Simulation", class = "btn-primary btn-block"),

      tags$hr(),
      tags$small(
        tags$b("About:"), "This model compares respiratory disease spread in ",
        "agricultural workers vs. the general population, accounting for ",
        "differences in household size distributions and crowding rates."
      )
    ),

    mainPanel(
      width = 9,

      tabsetPanel(
        tabPanel("Epidemic Curves",
                 tags$br(),
                 plotlyOutput("infection_plot", height = "400px"),
                 tags$br(),
                 plotlyOutput("cumulative_plot", height = "400px")
        ),

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

        tabPanel("Relative Risk",
                 tags$br(),
                 plotlyOutput("relative_risk_plot", height = "400px"),
                 tags$hr(),
                 helpText("Relative infection rate shows how much higher prevalence is among ",
                          "agricultural workers compared to the general population over time.")
        ),

        tabPanel("About the Model",
                 tags$br(),
                 includeMarkdown("about.md") %||% tags$div(
                   h4("Household-Structured SIR Model"),
                   tags$p("This model implements the House & Keeling (2008) household-structured
                          transmission framework with two populations."),
                   h5("Key Features:"),
                   tags$ul(
                     tags$li("Tracks household composition (susceptible, infected, recovered)"),
                     tags$li("Two populations: community (C) and agricultural workers (A)"),
                     tags$li("Assortative mixing controlled by epsilon parameter"),
                     tags$li("Crowded households have elevated transmission rates")
                   ),
                   h5("Parameters:"),
                   tags$ul(
                     tags$li(tags$b("R0:"), " Basic reproduction number (community transmission)"),
                     tags$li(tags$b("Epsilon:"), " Mixing parameter (0=assortative, 1=proportional)"),
                     tags$li(tags$b("SAR:"), " Secondary attack rate within households"),
                     tags$li(tags$b("Crowding fold:"), " Relative crowding probability by household size")
                   ),
                   h5("Reference:"),
                   tags$p("House, T. & Keeling, M.J. (2008). Deterministic epidemic models with
                          explicit household structure. Mathematical Biosciences, 213(1), 29-39.")
                 )
        )
      )
    )
  )
)

# ==============================================================================
# Server
# ==============================================================================

server <- function(input, output, session) {

  # Reactive value to store simulation results
  sim_results <- reactiveVal(NULL)

  # Run simulation when button is clicked
  observeEvent(input$run_sim, {
    withProgress(message = "Running simulation...", {
      result <- run_simulation(
        r0 = input$r0,
        epsilon = input$epsilon,
        sar_uncrowded = input$sar_uncrowded,
        sar_crowded = input$sar_crowded,
        crowding_fold_diff = input$crowding_fold,
        sim_days = input$sim_days
      )
      sim_results(result)
    })
  })

  # Run initial simulation on app load
  observe({
    if (is.null(sim_results())) {
      result <- run_simulation(
        r0 = 1.5, epsilon = 0.33, sar_uncrowded = 0.20,
        sar_crowded = 0.40, crowding_fold_diff = 2, sim_days = 365
      )
      sim_results(result)
    }
  })

  # Infection prevalence plot
  output$infection_plot <- renderPlotly({
    req(sim_results())
    df <- sim_results()

    p <- df %>%
      ggplot(aes(x = t, y = I_indiv * 100, color = subpop)) +
      geom_line(linewidth = 1) +
      scale_color_manual(
        values = c("A" = "#e41a1c", "C" = "#377eb8"),
        labels = c("A" = "Agricultural Workers", "C" = "General Population")
      ) +
      labs(
        title = "Disease Prevalence Over Time",
        x = "Days",
        y = "Infected (%)",
        color = "Population"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")

    ggplotly(p) %>% layout(legend = list(orientation = "h", y = -0.15))
  })

  # Cumulative infections plot
  output$cumulative_plot <- renderPlotly({
    req(sim_results())
    df <- sim_results()

    p <- df %>%
      ggplot(aes(x = t, y = R_indiv * 100, color = subpop)) +
      geom_line(linewidth = 1) +
      scale_color_manual(
        values = c("A" = "#e41a1c", "C" = "#377eb8"),
        labels = c("A" = "Agricultural Workers", "C" = "General Population")
      ) +
      labs(
        title = "Cumulative Attack Rate",
        x = "Days",
        y = "Cumulative Infected (%)",
        color = "Population"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")

    ggplotly(p) %>% layout(legend = list(orientation = "h", y = -0.15))
  })

  # Relative risk plot
  output$relative_risk_plot <- renderPlotly({
    req(sim_results())
    df <- sim_results()

    rel_df <- df %>%
      select(t, subpop, I_indiv) %>%
      pivot_wider(names_from = subpop, values_from = I_indiv) %>%
      mutate(relative_risk = A / C) %>%
      filter(is.finite(relative_risk), C > 0.001)  # Filter out early noise

    p <- rel_df %>%
      ggplot(aes(x = t, y = relative_risk)) +
      geom_line(linewidth = 1, color = "#984ea3") +
      geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
      labs(
        title = "Relative Infection Rate (Agricultural / General)",
        x = "Days",
        y = "Relative Risk"
      ) +
      theme_minimal()

    ggplotly(p)
  })

  # Summary statistics table
  output$summary_table <- renderTable({
    req(sim_results())
    df <- sim_results()

    summary_df <- df %>%
      group_by(subpop) %>%
      summarise(
        `Peak Prevalence (%)` = round(max(I_indiv) * 100, 2),
        `Time to Peak (days)` = t[which.max(I_indiv)],
        `Final Attack Rate (%)` = round(last(R_indiv) * 100, 2),
        .groups = "drop"
      ) %>%
      mutate(Population = ifelse(subpop == "A", "Agricultural Workers", "General Population")) %>%
      select(Population, everything(), -subpop)

    summary_df
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  # Comparison bar plot
  output$comparison_plot <- renderPlot({
    req(sim_results())
    df <- sim_results()

    summary_df <- df %>%
      group_by(subpop) %>%
      summarise(
        peak = max(I_indiv) * 100,
        attack_rate = last(R_indiv) * 100,
        .groups = "drop"
      ) %>%
      mutate(Population = ifelse(subpop == "A", "Agricultural\nWorkers", "General\nPopulation"))

    summary_df %>%
      pivot_longer(cols = c(peak, attack_rate), names_to = "metric", values_to = "value") %>%
      mutate(metric = ifelse(metric == "peak", "Peak Prevalence (%)", "Final Attack Rate (%)")) %>%
      ggplot(aes(x = Population, y = value, fill = Population)) +
      geom_col(width = 0.6) +
      facet_wrap(~metric, scales = "free_y") +
      scale_fill_manual(values = c("Agricultural\nWorkers" = "#e41a1c", "General\nPopulation" = "#377eb8")) +
      labs(y = "Percent", x = NULL) +
      theme_minimal() +
      theme(legend.position = "none",
            strip.text = element_text(face = "bold"))
  })

  # Interpretation text
  output$interpretation <- renderText({
    req(sim_results())
    df <- sim_results()

    summary_df <- df %>%
      group_by(subpop) %>%
      summarise(
        peak = max(I_indiv) * 100,
        attack_rate = last(R_indiv) * 100,
        .groups = "drop"
      )

    ag_attack <- summary_df$attack_rate[summary_df$subpop == "A"]
    gen_attack <- summary_df$attack_rate[summary_df$subpop == "C"]
    diff <- ag_attack - gen_attack

    paste0(
      "Under these parameters, agricultural workers experience a final attack rate of ",
      round(ag_attack, 1), "% compared to ", round(gen_attack, 1),
      "% in the general population - a difference of ", round(diff, 1),
      " percentage points. This differential is driven by higher household crowding ",
      "rates among agricultural workers, which increases within-household transmission."
    )
  })
}

# ==============================================================================
# Run App
# ==============================================================================

shinyApp(ui = ui, server = server)
