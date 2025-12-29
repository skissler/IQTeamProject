# //////////////////////////////////////////////////////////////////////////////
# Import
# //////////////////////////////////////////////////////////////////////////////

library(tidyverse)
library(odin)
library(purrr) # For the map function
source('code/utils.R')
source('code/epimodels.R')

source('code/import_naws.R')
source('code/import_acs.R')

# //////////////////////////////////////////////////////////////////////////////
# Define a Simulation Function for a SINGLE County
# //////////////////////////////////////////////////////////////////////////////

# By moving the loop's logic into a function, we can easily apply it
# to all counties without a slow `for` loop.
run_simulation_for_county <- function(geoid, household_states_df, county_lookup_df, acs_data_df) {
  
  # Get region and other county-specific data
  region <- county_lookup_df %>%
    filter(GEOID == geoid) %>%
    pull(REGION6) %>%
    first()
  
  county_data <- acs_data_df %>%
    filter(GEOID == geoid)
  
  hhSize_factor <- county_data %>% arrange(hhSize) %>% pull(hhSize_factor)
  crowded_factor <- county_data %>% arrange(hhSize) %>% pull(crowded_factor)
  
  # Get household distribution for the region
  hh_dist_C <- get_hh_dist(region, "ACS", crowding_fold_diff, adjust_hhvars, hhSize_factor, crowded_factor)
  hh_dist_A <- get_hh_dist(region, "NAWS", crowding_fold_diff, adjust_hhvars, hhSize_factor, crowded_factor)
  
  # Calculate initial conditions
  n_states <- nrow(household_states_df)
  ic_joiner_C <- get_initial_conditions(hh_dist_C, init_prev = 0.001)
  ic_joiner_A <- get_initial_conditions(hh_dist_A, init_prev = 0.001)
  
  init_C <- household_states_df %>%
    left_join(ic_joiner_C, by = c("x", "y", "z", "hh_size", "crowded")) %>%
    arrange(state_index) %>%
    replace_na(list(frac = 0)) %>%
    pull(frac)
  
  init_A <- household_states_df %>%
    left_join(ic_joiner_A, by = c("x", "y", "z", "hh_size", "crowded")) %>%
    arrange(state_index) %>%
    replace_na(list(frac = 0)) %>%
    pull(frac)
  
  # Get population data
  pop_cty <- county_data %>% pull(population) %>% first()
  prop_ag <- county_data %>% pull(prop_ag_workers) %>% first()
  
  # Initialize and run the Odin model
  mod <- household_model_twopop_crowding$new(
    n_states = n_states,
    x = household_states_df$x,
    y = household_states_df$y,
    z = household_states_df$z,
    hh_size = household_states_df$hh_size,
    crowded = household_states_df$crowded,
    rec_index = household_states_df$rec_index,
    inf_index = household_states_df$inf_index,
    init_C = init_C,
    init_A = init_A,
    gamma = 1/5,
    tau_C = (1/4)*(1/5),
    tau_A = (1/4)*(1/5),
    beta_C = 6/5,
    beta_A = 6/5,
    pop_C = pop_cty * (1 - prop_ag),
    pop_A = pop_cty * prop_ag,
    eps = 1.0,
    debug_enable = TRUE
  )
  
  t <- seq(0, 365, length.out=366)
  model_output <- mod$run(t) %>% as_tibble()
  
  # Format and return the results for this county
  epidf_indiv <- format_output_indiv(model_output, household_states_df) %>%
    mutate(GEOID = geoid, REGION6 = region)
  
  return(epidf_indiv)
}

# //////////////////////////////////////////////////////////////////////////////
# Run the Simulation Efficiently
# //////////////////////////////////////////////////////////////////////////////

# Define key variables
max_hh_size <- 7
crowding_fold_diff <- 2
adjust_hhvars <- TRUE
init_prev <- 0.001

# Load household state definitions
household_states <- generate_household_state_table(n_min = 1, n_max = max_hh_size, crowding = TRUE)

# --- THE EFFICIENT WAY ---

# 1. MAP: Run the simulation for each county and store results in a list.
# We use purrr::map which is equivalent to lapply.
# The .progress = TRUE option gives you a nice progress bar!
list_of_results <- purrr::map(
  GEOID_vec, 
  ~ run_simulation_for_county(
      geoid = .x, 
      household_states_df = household_states,
      county_lookup_df = county_lookup,
      acs_data_df = acs_data
    ),
  .progress = TRUE
)

# 2. BIND: Combine the list of data frames into a single tibble, one time.
epidf_indiv_full <- bind_rows(list_of_results)

# Your `epidf_indiv_full` data frame is now ready for analysis and plotting.
