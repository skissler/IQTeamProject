# code/utils_documented.R
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
    sf::st_transform(5070)  # Albers Equal Area projection

  # Exclude non-contiguous states and territories
  excluded <- c("02", "15", "72", "60", "66", "69", "78")

  lower48_sf <- states_sf %>%
    dplyr::filter(!STATEFP %in% excluded) %>%
    dplyr::select(GEOID, STUSPS, NAME)

  if (internal) {
    return(lower48_sf)
  } else {
    # Return single unioned geometry
    return(sf::st_union(lower48_sf))
  }
}


#' Get Population-Weighted 3-Digit ZIP Code Centroids
#'
#' Calculates population-weighted centroids for 3-digit ZIP code areas,
#' filtered to the contiguous United States. Uses 5-digit ZCTA data and
#' ACS population estimates.
#'
#' @return An sf object with columns: zip3, population, and point geometry
#'   in Albers Equal Area projection (EPSG:5070).
#'
#' @details
#' This function:
#' 1. Downloads 5-digit ZCTA boundaries from tigris
#' 2. Joins population data from ACS
#' 3. Calculates centroids of each 5-digit ZCTA
#' 4. Aggregates to 3-digit level using population-weighted means
#' 5. Filters to points within the Lower 48 states
#'
#' @examples
#' zip3_centroids <- get_zip3_centroids()
get_zip3_centroids <- function() {

  # Get ZCTA (ZIP Code Tabulation Area) shapefiles

zcta_sf <- tigris::zctas(year = 2020)

  # Get population data from ACS
  pop_data <- tidycensus::get_acs(
    geography = "zcta",
    variables = "B01003_001",  # Total population
    year = 2021,
    geometry = FALSE
  )

  # Join population to ZCTAs
  zcta_sf <- zcta_sf %>%
    dplyr::left_join(pop_data, by = c("ZCTA5CE20" = "GEOID")) %>%
    dplyr::filter(!is.na(estimate))

  # Create 3-digit ZIP codes
  zcta_sf <- zcta_sf %>%
    dplyr::mutate(zip3 = substr(ZCTA5CE20, 1, 3))

  # Compute centroids of each 5-digit ZCTA
  zcta_sf_centroids <- zcta_sf %>%
    dplyr::mutate(
      centroid = sf::st_centroid(geometry),
      lon = sf::st_coordinates(centroid)[, 1],
      lat = sf::st_coordinates(centroid)[, 2]
    ) %>%
    sf::st_drop_geometry()

  # Aggregate to 3-digit ZIP using population-weighted centroids
  zip3_df <- zcta_sf_centroids %>%
    dplyr::group_by(zip3) %>%
    dplyr::summarise(
      population = sum(estimate, na.rm = TRUE),
      lon = weighted.mean(lon, estimate, na.rm = TRUE),
      lat = weighted.mean(lat, estimate, na.rm = TRUE)
    ) %>%
    dplyr::ungroup()

  # Convert to sf points
  zip3_points <- zip3_df %>%
    dplyr::filter(!is.na(lon)) %>%
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    sf::st_transform(crs = 5070)

  # Filter to Lower 48 states
  lower48_union <- get_state_boundaries(internal = FALSE)
  inside <- sf::st_within(zip3_points, lower48_union, sparse = FALSE)[, 1]
  zip3_points_filtered <- zip3_points[inside, ]

  return(zip3_points_filtered)
}


#' Calculate Force of Infection for Spatial Spread Model
#'
#' Computes the force of infection for a gravity-model spatial spread,
#' accounting for population size and distance between locations.
#'
#' @param infvec Integer vector. Indices of currently infected locations.
#' @param popvec Numeric vector. Population of each location (normalized).
#' @param distmat Matrix. Distance matrix between locations (in km).
#' @param b0 Numeric. Baseline transmission rate. Default 0.
#' @param bd Numeric. Distance-dependent transmission coefficient. Default 0.77.
#' @param mu Numeric. Population size exponent. Default 0.23.
#' @param rho Numeric. Distance decay parameter (km). Default 96.
#'
#' @return Numeric. The force of infection.
#'
#' @details
#' The force of infection follows a gravity model:
#' FOI = b0 + bd * (pop^mu) * sum(exp(-dist/rho)) / normalizer
get_foi <- function(infvec, popvec, distmat, b0 = 0, bd = 0.77, mu = 0.23, rho = 96) {

  distmat <- units::drop_units(distmat)
  expvec <- colSums(exp(-distmat / rho))
  foi <- b0 + bd * (popvec^mu) * (sum(expvec[infvec])) / (sum(expvec) - 1)

  return(foi)
}


#' Simulate Spatial Outbreak Spread
#'
#' Runs a stochastic simulation of outbreak spread across geographic locations
#' using a gravity model for transmission.
#'
#' @param seed Integer. Index of the seed location where outbreak starts.
#' @param geodf An sf object. Must contain 'population' column and point geometry.
#' @param b0 Numeric. Baseline transmission rate. Default 0.
#' @param bd Numeric. Distance-dependent transmission coefficient. Default 0.77.
#' @param mu Numeric. Population size exponent. Default 0.23.
#' @param rho Numeric. Distance decay parameter (km). Default 96.
#'
#' @return A tibble with columns: t (time step), loc (location index).
#'
#' @examples
#' # zip3_points <- get_zip3_centroids()
#' # outbreak <- simulate_outbreak(seed = 100, geodf = zip3_points)
simulate_outbreak <- function(seed, geodf, b0 = 0, bd = 0.77, mu = 0.23, rho = 96) {

  distmat <- units::drop_units(units::set_units(sf::st_distance(geodf), "km"))
  popvec <- geodf$population / mean(geodf$population)
  denominator <- colSums(exp(-distmat / rho)) - 1

  infvec <- c(seed)
  tvec <- c(0)

  for (t in 1:100) {
    if (length(infvec) > 1) {
      numerator <- colSums(exp(-distmat[infvec, ] / rho))
    } else {
      numerator <- exp(-distmat[infvec, ] / rho)
    }

    foi <- b0 + bd * (popvec^mu) * numerator / denominator
    pinf <- 1 - exp(-foi)
    draw <- runif(length(pinf))

    newinf <- setdiff(which(draw < pinf), infvec)
    infvec <- c(infvec, newinf)
    tvec <- c(tvec, rep(t, length(newinf)))
  }

  out_df <- tibble::tibble(t = tvec, loc = infvec)
  return(out_df)
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
#' @param n_max Integer. Maximum household size to include. Default 8.
#' @param crowding Logical. If TRUE, duplicate states for crowded/non-crowded
#'   households. Default FALSE.
#'
#' @return A tibble with columns:
#'   - x: Number of susceptible individuals
#'   - y: Number of infected individuals
#'   - z: Number of recovered individuals
#'   - hh_size: Total household size (x + y + z)
#'   - state_index: Unique index for this state (1 to N)
#'   - rec_index: Index of state after a recovery event (y->z), or 0 if impossible
#'   - inf_index: Index of state after an infection event (x->y), or 0 if impossible
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
generate_household_state_table <- function(n_min = 1, n_max = 8, crowding = FALSE) {

  # Create all possible (x, y, z) combinations
  states <- expand.grid(
    x = 0:n_max,
    y = 0:n_max,
    z = 0:n_max
  ) %>%
    dplyr::mutate(hh_size = x + y + z) %>%
    dplyr::filter(hh_size <= n_max) %>%
    dplyr::filter(hh_size >= n_min) %>%
    dplyr::arrange(hh_size, x, y, z) %>%
    dplyr::mutate(state_index = dplyr::row_number())

  # Helper to find state index for a given (x, y, z)
  find_index <- function(x_, y_, z_) {
    idx <- states %>%
      dplyr::filter(x == x_, y == y_, z == z_) %>%
      dplyr::pull(state_index)
    if (length(idx) == 0) return(0) else return(idx)
  }

  # Compute transition indices
  states <- states %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      rec_index = if (z > 0 && y < n_max) find_index(x, y + 1, z - 1) else 0,
      inf_index = if (y > 0 && x < n_max) find_index(x + 1, y - 1, z) else 0
    ) %>%
    dplyr::ungroup()

  # If crowding is enabled, duplicate states with crowding indicator
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
#'   maximum-size (7) and size-2 households. Default 1 (no adjustment).
#' @param indexcols Character vector. Column names to group by when calculating
#'   the adjustment. Default NULL.
#'
#' @return Data frame with additional column prop_crowded_adj containing the
#'   adjusted crowding proportions by household size.
#'
#' @details
#' The adjustment assumes crowding probability scales linearly with household
#' size. A fold_diff of 2 means households of size 7 are twice as likely to
#' be crowded as households of size 2.
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
#'
#' Generates a table for joining household size distributions to household
#' states, accounting for crowding. Used to construct initial conditions
#' for the epidemic model.
#'
#' @param dat Data frame. Must contain columns: hhSize, prop, prop_crowded.
#' @param fold_diff Numeric. Crowding fold difference (see adjust_crowding).
#'   Default 1.
#' @param indexcols Character vector. Grouping columns. Default NULL.
#'
#' @return A tibble with columns: x, y, z, hh_size, crowded, frac.
#'   - x, y, z: Household composition (all susceptible at t=0, so y=z=0)
#'   - hh_size: Household size
#'   - crowded: Crowding indicator (0 or 1)
#'   - frac: Fraction of population in this state
#'
#' @details
#' At time 0, all households are fully susceptible (x = hh_size, y = z = 0).
#' The frac column gives the proportion of households in each (size, crowded)
#' combination, which can be joined to the household state table.
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
    tidyr::pivot_longer(-t, names_to = "state_index", values_to = "prop_hh") %>%
    dplyr::mutate(subpop = substr(state_index, 3, 3)) %>%
    dplyr::mutate(state_index = substr(state_index, 5, nchar(state_index) - 1)) %>%
    dplyr::mutate(state_index = as.numeric(state_index)) %>%
    dplyr::left_join(
      dplyr::select(household_states, x, y, z, hh_size, state_index, crowded),
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
    dplyr::mutate(
      S_num = prop_hh * x,
      I_num = prop_hh * y,
      R_num = prop_hh * z,
      den = prop_hh * hh_size
    ) %>%
    dplyr::group_by(t, subpop) %>%
    dplyr::summarise(
      S_num = sum(S_num),
      I_num = sum(I_num),
      R_num = sum(R_num),
      den = sum(den),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      S_indiv = S_num / den,
      I_indiv = I_num / den,
      R_indiv = R_num / den
    ) %>%
    dplyr::select(t, subpop, S_indiv, I_indiv, R_indiv)

  return(out)
}


# ==============================================================================
# 5. MISCELLANEOUS
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
