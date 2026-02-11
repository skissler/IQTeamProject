# code/utils.R
# Helper functions for the influenza-agriculture impact analysis
#
# This file contains documented versions of all utility functions.
# Functions are organized by category:
#   1. Geographic/Spatial functions
#   2. Household state generation
#   3. Initial condition helpers
#   4. Output formatting
#   5. Miscellaneous

# Note: This file expects code/config.R to be loaded first (via setup.R)
# for region_map and other configuration values.

# ==============================================================================
# 1. GEOGRAPHIC / SPATIAL FUNCTIONS
# ==============================================================================

#' Get US State Boundaries for Lower 48 States
#'
#' Downloads state boundaries from the Census Bureau and filters to the
#' contiguous United States (excludes Alaska, Hawaii, and territories).
#'
#' @param internal Logical. If TRUE (default), returns individual state
#'   geometries as an sf object. If FALSE, returns a single unioned geometry.
#' @return An sf object with state boundaries in Albers Equal Area projection
#'   (EPSG:5070). Contains columns: GEOID, STUSPS (state abbreviation), NAME.
#'
#' @examples
#' states <- get_state_boundaries()
#' us_outline <- get_state_boundaries(internal = FALSE)
get_state_boundaries <- function(internal = TRUE) {

  # Download US state boundaries (cartographic boundary file for smaller size)
  states_sf <- tigris::states(cb = TRUE, year = 2020) %>%
    st_transform(5070)  # Albers Equal Area projection

  # Exclude non-contiguous states and territories
  excluded <- c("02", "15", "72", "60", "66", "69", "78")

  lower48_sf <- states_sf %>%
    filter(!STATEFP %in% excluded) %>%
    select(GEOID, STUSPS, NAME)

  if (internal) {
    return(lower48_sf)
  } else {
    # Return single unioned geometry
    return(st_union(lower48_sf))
  }
}


# ==============================================================================
# 2. HOUSEHOLD STATE GENERATION
# ==============================================================================

#' Generate Household State Table for Epidemic Model
#'
#' Creates a table of all possible household states (combinations of
#' susceptible, infected, recovered individuals) for the household-structured
#' epidemic model. Optionally includes crowding status.
#'
#' @param n_min Integer. Minimum household size to include. Default 1.
#' @param n_max Integer. Maximum household size to include. Default 7.
#' @param crowding Logical. If TRUE, duplicate states for crowded/non-crowded
#'   households. Default FALSE.
#'
#' @return A tibble with columns:
#'   - x: Number of susceptible individuals
#'   - y: Number of infected individuals
#'   - z: Number of recovered individuals
#'   - hh_size: Total household size (x + y + z)
#'   - state_index: Unique index for this state (1 to N)
#'   - rec_index: Index of the source state that transitions into this one
#'       via a recovery event (y->z), or 0 if no such source exists
#'   - inf_index: Index of the source state that transitions into this one
#'       via an infection event (x->y), or 0 if no such source exists
#'   - crowded: (if crowding=TRUE) 0 for non-crowded, 1 for crowded households
#'
#' @details
#' The household model tracks the composition of households rather than
#' individuals. Each state represents a household with x susceptible,
#' y infected, and z recovered members. Transition indices allow efficient
#' lookup of the new state after infection or recovery events.
#'
#' When crowding=TRUE, the state space is doubled: states 1:N are non-crowded,
#' states (N+1):(2N) are crowded versions of the same configurations.
#'
#' @examples
#' # Basic state table for households of size 1-7
#' states <- generate_household_state_table(n_max = 7)
#'
#' # With crowding
#' states_crowded <- generate_household_state_table(n_max = 7, crowding = TRUE)
generate_household_state_table <- function(n_min = 1, n_max = 7, crowding = FALSE) {

  # Create all possible (x, y, z) combinations
  states <- expand.grid(
    x = 0:n_max,
    y = 0:n_max,
    z = 0:n_max
  ) %>%
    mutate(hh_size = x + y + z) %>%
    filter(hh_size <= n_max) %>%
    filter(hh_size >= n_min) %>%
    arrange(hh_size, x, y, z) %>%
    mutate(state_index = row_number())

  # Helper to find state index for a given (x, y, z)
  find_index <- function(x_, y_, z_) {
    idx <- states %>%
      filter(x == x_, y == y_, z == z_) %>%
      pull(state_index)
    if (length(idx) == 0) return(0) else return(idx)
  }

  # Compute transition indices
  states <- states %>%
    rowwise() %>%
    mutate(
      rec_index = if (z > 0 && y < n_max) find_index(x, y + 1, z - 1) else 0,
      inf_index = if (y > 0 && x < n_max) find_index(x + 1, y - 1, z) else 0
    ) %>%
    ungroup()

  # If crowding is enabled, duplicate states with crowding indicator
  if (crowding) {
    n_states <- nrow(states)

    states_crowded <- states %>%
      mutate(state_index = state_index + n_states) %>%
      mutate(rec_index = case_when(rec_index > 0 ~ rec_index + n_states, TRUE ~ 0)) %>%
      mutate(inf_index = case_when(inf_index > 0 ~ inf_index + n_states, TRUE ~ 0))

    states <- bind_rows(
      mutate(states, crowded = 0),
      mutate(states_crowded, crowded = 1)
    )
  }

  return(states)
}


# ==============================================================================
# 3. INITIAL CONDITION HELPERS
# ==============================================================================

#' Adjust Crowding Proportions by Household Size
#'
#' Adjusts the proportion of crowded households based on household size,
#' applying a linear scaling where larger households are more likely to be
#' crowded.
#'
#' @param df Data frame. Must contain columns: hhSize, prop, prop_crowded.
#' @param fold_diff Numeric. Fold difference in crowding probability between
#'   maximum-size and size-2 households. Default 1 (no adjustment).
#' @param n_max Integer. Maximum household size. Default 7.
#' @param indexcols Character vector. Column names to group by when calculating
#'   the adjustment. Default NULL.
#'
#' @return Data frame with additional column prop_crowded_adj containing the
#'   adjusted crowding proportions by household size.
#'
#' @details
#' The adjustment assumes crowding probability scales linearly with household
#' size. A fold_diff of 2 means households of size n_max are twice as likely
#' to be crowded as households of size 2.
adjust_crowding <- function(df, fold_diff = 1, n_max = 7, indexcols = NULL) {

  out <- df %>%
    mutate(multiplier = (1 + (fold_diff - 1) * (hhSize - 2) / (n_max - 2))) %>%
    mutate(multiplier = case_when(hhSize == 1 ~ 0, TRUE ~ multiplier)) %>%
    mutate(denom = prop * multiplier) %>%
    group_by(across(all_of(indexcols))) %>%
    mutate(c = prop_crowded / sum(denom)) %>%
    mutate(prop_crowded_adj = c * multiplier) %>%
    select(-multiplier, -denom, -c)

  return(out)
}


#' Create Initial Condition Joiner Table
#'
#' Converts a household size distribution (one row per size, with an overall
#' crowding rate) into a table that can be joined to the household state table
#' to set initial conditions for the epidemic model.
#'
#' @param dat Data frame. Must contain columns: hhSize, prop, prop_crowded.
#'   - hhSize: Household size (e.g., 1 through n_max)
#'   - prop: Fraction of households of that size (should sum to 1)
#'   - prop_crowded: Overall proportion of households that are crowded
#'     (constant across rows within a group)
#' @param fold_diff Numeric. Crowding fold difference (see adjust_crowding).
#'   Default 1.
#' @param n_max Integer. Maximum household size. Default 7.
#' @param indexcols Character vector. Grouping columns. Default NULL.
#'
#' @return A tibble with columns: x, y, z, hh_size, crowded, frac.
#'   - x: Number of susceptible individuals (equals hh_size at t=0)
#'   - y: Number of infected individuals (0 at t=0)
#'   - z: Number of recovered individuals (0 at t=0)
#'   - hh_size: Household size
#'   - crowded: Crowding indicator (0 or 1)
#'   - frac: Fraction of households in this (size, crowded) combination
#'
#' @details
#' The function works in three steps:
#'
#' 1. Calls adjust_crowding() to compute size-specific crowding probabilities
#'    (prop_crowded_adj), distributing the overall crowding rate across sizes
#'    with a linear gradient controlled by fold_diff.
#'
#' 2. Splits each household size into two rows: non-crowded (crowded=0) with
#'    frac = prop * (1 - prop_crowded_adj), and crowded (crowded=1) with
#'    frac = prop * prop_crowded_adj. The fracs sum to 1 across all rows.
#'
#' 3. Sets the initial disease state to fully susceptible: x = hh_size,
#'    y = 0, z = 0.
#'
#' The output is designed to be left-joined to the household state table
#' (from generate_household_state_table) on (x, y, z, hh_size, crowded).
#' Unmatched states receive frac = 0 via replace_na, meaning those household
#' configurations are initially empty.
make_ic_joiner <- function(dat, fold_diff = 1, n_max = 7, indexcols = NULL) {

  dat <- adjust_crowding(dat, fold_diff = fold_diff, n_max = n_max, indexcols = indexcols)

  dat_c0 <- mutate(dat, x = hhSize, y = 0, z = 0, crowded = 0,
                    frac = prop * (1 - prop_crowded_adj))
  dat_c1 <- mutate(dat, x = hhSize, y = 0, z = 0, crowded = 1,
                    frac = prop * prop_crowded_adj)

  out <- bind_rows(dat_c0, dat_c1) %>%
    select(x, y, z, hh_size = hhSize, crowded, frac)

  return(out)
}


# ==============================================================================
# 4. OUTPUT FORMATTING
# ==============================================================================

#' Format Model Output at Household Level
#'
#' Converts raw ODE output from the household model to a tidy data frame
#' with household-level information.
#'
#' @param model_output Data frame. Output from mod$run() containing columns
#'   named like "H_C[1]", "H_A[1]", etc. for household states.
#' @param household_states Data frame. Output from generate_household_state_table().
#'
#' @return A tibble with columns:
#'   - t: Time
#'   - state_index: Household state index
#'   - prop_hh: Proportion of households in this state
#'   - subpop: Population indicator ("C" = community, "A" = agricultural)
#'   - x, y, z: Household composition
#'   - hh_size: Household size
#'   - crowded: Crowding indicator
format_output_hh <- function(model_output, household_states) {

  out <- model_output %>%
    pivot_longer(-t, names_to = "state_index", values_to = "prop_hh") %>%
    mutate(subpop = substr(state_index, 3, 3)) %>%
    mutate(state_index = substr(state_index, 5, nchar(state_index) - 1)) %>%
    mutate(state_index = as.numeric(state_index)) %>%
    left_join(
      select(household_states, x, y, z, hh_size, state_index, crowded),
      by = "state_index"
    )

  return(out)
}


#' Format Model Output at Individual Level
#'
#' Converts raw ODE output from the household model to a tidy data frame
#' with individual-level prevalence (S, I, R proportions).
#'
#' @param model_output Data frame. Output from mod$run().
#' @param household_states Data frame. Output from generate_household_state_table().
#'
#' @return A tibble with columns:
#'   - t: Time
#'   - subpop: Population indicator ("C" = community, "A" = agricultural)
#'   - S_indiv: Proportion of individuals susceptible
#'   - I_indiv: Proportion of individuals infected
#'   - R_indiv: Proportion of individuals recovered
#'
#' @details
#' This function aggregates household-level output to individual-level by
#' weighting each household state by its size and computing the fraction
#' of individuals in each disease state.
format_output_indiv <- function(model_output, household_states) {

  out_hh <- format_output_hh(model_output, household_states)

  out <- out_hh %>%
    mutate(
      S_num = prop_hh * x,
      I_num = prop_hh * y,
      R_num = prop_hh * z,
      den = prop_hh * hh_size
    ) %>%
    group_by(t, subpop) %>%
    summarise(
      S_num = sum(S_num),
      I_num = sum(I_num),
      R_num = sum(R_num),
      den = sum(den),
      .groups = "drop"
    ) %>%
    mutate(
      S_indiv = S_num / den,
      I_indiv = I_num / den,
      R_indiv = R_num / den
    ) %>%
    select(t, subpop, S_indiv, I_indiv, R_indiv)

  return(out)
}


# ==============================================================================
# 5. EPIDEMIOLOGICAL PARAMETER HELPERS
# ==============================================================================

#' Calculate tau from SAR
#'
#' Derives the within-household transmission rate (tau) needed to achieve a
#' target secondary attack rate (SAR).
#'
#' @param sar Target secondary attack rate (proportion, e.g., 0.20)
#' @param gamma Recovery rate
#' @return tau value (within-household transmission rate)
#'
#' @details
#' In the House & Keeling household model with exponentially distributed
#' infectious periods, the SAR for a 2-person household is derived from
#' competing exponentials (infection at rate tau vs recovery at rate gamma):
#'
#'   SAR = tau / (tau + gamma)
#'
#' Solving for tau:
#'   tau = SAR * gamma / (1 - SAR)
calculate_tau <- function(sar, gamma) {
  sar * gamma / (1 - sar)
}

#' Calculate tau_boost for a target SAR in crowded households
#'
#' Derives the additional transmission rate needed to boost from the uncrowded
#' SAR to the crowded SAR.
#'
#' @param sar_crowded Target SAR for crowded households (proportion, e.g., 0.40)
#' @param gamma Recovery rate
#' @param tau Baseline tau for uncrowded households
#' @return tau_boost value to add to tau for crowded households
calculate_tau_boost <- function(sar_crowded, gamma, tau) {
  tau_crowded <- calculate_tau(sar_crowded, gamma)
  tau_boost <- tau_crowded - tau
  return(tau_boost)
}


# ==============================================================================
# 6. MISCELLANEOUS
# ==============================================================================

#' Sinusoidal Time-Varying Transmission Rate
#'
#' Returns a seasonally-varying transmission rate multiplier based on a
#' sinusoidal function with 180-day period.
#'
#' @param t Numeric. Time in days.
#'
#' @return Numeric. Transmission rate multiplier (oscillates around 1).
#'
#' @examples
#' # Multiplier at day 0
#' rtsin(0)  # Returns 1
#'
#' # Multiplier at day 45 (peak)
#' rtsin(45)  # Returns 1.2
rtsin <- function(t) {
  out <- 0.2 * sin(2 * pi * t / 180) + 1
  return(out)
}
