# code/epimodels.R
# Household-structured epidemic model for influenza-agriculture impact analysis
#
# This file defines the primary compartmental disease model using the odin
# package for ODE specification. The model implements the House & Keeling (2008)
# household-structured transmission framework with two populations and crowding.
#
# Note: This file expects code/utils.R to be loaded for helper functions.
# In practice, source code/setup.R which handles all dependencies.

library(tidyverse)
library(odin)
source('code/utils.R')

# ==============================================================================
# TWO-POPULATION MODEL WITH CROWDING
# ==============================================================================
#
#' Two-Population Household Model with Crowding Effects
#'
#' The primary model used in the analysis. Extends a household-structured SIR
#' model to two populations (community and agricultural workers) with
#' assortative mixing and household crowding effects.
#'
#' @section Model Structure:
#' The population is divided into households of varying sizes. Each household
#' is characterized by its composition: (x, y, z) where:
#' - x = number of susceptible members
#' - y = number of infected members
#' - z = number of recovered members
#' - household size n = x + y + z
#'
#' Two populations (C = community, A = agricultural workers) each follow
#' household-structured dynamics. Populations are coupled through a mixing
#' matrix that determines the force of infection from each group.
#'
#' @section Mathematical Formulation:
#' For each household state (x, y, z), the dynamics are:
#'
#' \deqn{dH(x,y,z)/dt = Recovery + Within-HH infection + Between-HH infection}
#'
#' Where:
#' - Recovery: gamma * [(y+1)*H(x,y+1,z-1) - y*H(x,y,z)]
#' - Within-HH: vax_mult * (tau + tau_boost*crowded) * [(x+1)(y-1)*H(x+1,y-1,z) - xy*H(x,y,z)]
#' - Between-HH: lambda * [(x+1)*H(x+1,y-1,z) - x*H(x,y,z)]
#'
#' Vaccination reduces susceptibility, so the group-specific multiplier
#' vax_mult (= 1 - vax_eff * vax_cov) scales BOTH the within-household and
#' between-household force of infection acting on susceptibles.
#'
#' @section Mixing Matrix:
#' The mixing parameter epsilon (eps) controls assortativity:
#' - eps = 0: Complete assortativity (no between-group contact)
#' - eps = 1: Proportional mixing (contacts proportional to population size)
#'
#' Mixing matrix entries:
#' - m_CC = (1-eps) + eps*w_C  (C contacts C)
#' - m_CA = eps*w_A            (C contacts A)
#' - m_AC = eps*w_C            (A contacts C)
#' - m_AA = (1-eps) + eps*w_A  (A contacts A)
#'
#' Where w_C = pop_C/(pop_C+pop_A) and w_A = pop_A/(pop_C+pop_A)
#'
#' Force of infection (between-household):
#' - lambda_C = beta * vax_mult_C * (m_CC*I_C + m_CA*I_A)
#' - lambda_A = beta * vax_mult_A * (m_AC*I_C + m_AA*I_A)
#'
#' @section Crowding Effect:
#' Within-household transmission rate becomes:
#' - Uncrowded (crowded=0): tau
#' - Crowded (crowded=1): tau + tau_boost
#'
#' This models the increased secondary attack rate observed in crowded
#' households (>1 person per room).
#'
#' @section State Space:
#' The state space is doubled compared to non-crowding models:
#' - States 1:N represent uncrowded households
#' - States (N+1):(2N) represent crowded households with same compositions
#'
#' Use generate_household_state_table(crowding=TRUE) to create the state table.
#'
#' @section Parameters:
#' \describe{
#'   \item{n_states}{Number of household states (2x base states for crowding)}
#'   \item{x[], y[], z[]}{Household composition vectors}
#'   \item{hh_size[]}{Household sizes}
#'   \item{crowded[]}{Crowding indicator (0 or 1) for each state}
#'   \item{rec_index[]}{Index of source state that flows into this one via recovery (y->z)}
#'   \item{inf_index[]}{Index of source state that flows into this one via infection (x->y)}
#'   \item{init_C[], init_A[]}{Initial distributions for each population}
#'   \item{gamma}{Recovery rate (1/infectious period)}
#'   \item{tau}{Baseline within-HH transmission rate (shared by both populations)}
#'   \item{tau_boost}{Additional transmission rate for crowded households}
#'   \item{beta}{Between-household transmission rate}
#'   \item{eps}{Assortativity parameter (0 = assortative, 1 = proportional)}
#'   \item{pop_C, pop_A}{Population sizes (for mixing matrix weights)}
#'   \item{vax_mult_C}{Vaccination susceptibility multiplier for community: 1 - vax_eff * vax_cov_C (1 = no vaccination)}
#'   \item{vax_mult_A}{Vaccination susceptibility multiplier for agricultural workers: 1 - vax_eff * vax_cov_A (1 = no vaccination)}
#' }
#'
#' @section Typical Parameter Values:
#' \describe{
#'   \item{gamma}{1/5 (5-day infectious period)}
#'   \item{tau}{0.05 (20% SAR in uncrowded households)}
#'   \item{tau_boost}{0.083 (boosting to 40% SAR in crowded households)}
#'   \item{beta}{0.153 for R0=1.2 (calibrated)}
#'   \item{eps}{0.33 (moderate assortativity)}
#' }
#'
#' @section Usage:
#' ```r
#' # Generate state table with crowding
#' states <- generate_household_state_table(n_max = 7, crowding = TRUE)
#'
#' # Create initial conditions using make_ic_joiner()
#' ic_C <- make_ic_joiner(acs_data, fold_diff = 2)
#' ic_A <- make_ic_joiner(naws_data, fold_diff = 2)
#'
#' # Initialize model
#' mod <- household_model_twopop_crowding$new(
#'   n_states = nrow(states),
#'   x = states$x, y = states$y, z = states$z,
#'   hh_size = states$hh_size,
#'   crowded = states$crowded,
#'   rec_index = states$rec_index,
#'   inf_index = states$inf_index,
#'   init_C = init_C, init_A = init_A,
#'   gamma = 1/5,
#'   tau = 0.05, tau_boost = 0.083,
#'   beta = 0.153,
#'   eps = 0.33,
#'   pop_C = 1000000, pop_A = 50000,
#'   vax_mult_C = 1, vax_mult_A = 1  # no vaccination
#' )
#'
#' # Run simulation
#' times <- seq(0, 365, by = 1)
#' out <- mod$run(times)
#'
#' # Format output
#' epidf_hh <- format_output_hh(out, states)
#' epidf_indiv <- format_output_indiv(out, states)
#' ```
#'
#' @references
#' House, T. & Keeling, M.J. (2008). Deterministic epidemic models with explicit
#' household structure. Mathematical Biosciences, 213(1), 29-39.
#'
household_model_twopop_crowding <- odin::odin({

  # ============================================================================
  # Parameters
  # ============================================================================

  # Structural parameters (from household state table)
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

  # Epidemiological parameters
  gamma <- user()       # Recovery rate
  tau <- user()         # Baseline within-HH transmission (shared by both populations)
  tau_boost <- user()   # Additional transmission for crowded HHs
  beta <- user()        # Between-household transmission rate
  eps <- user()         # Assortativity (0=assortative, 1=proportional)
  pop_C <- user()       # Community population size
  pop_A <- user()       # Agricultural population size

  # Vaccination multipliers: 1 - vax_eff * vax_cov (1 = no vaccination)
  vax_mult_C <- user()  # Susceptibility reduction for community
  vax_mult_A <- user()  # Susceptibility reduction for agricultural workers

  # ============================================================================
  # Dimension declarations
  # ============================================================================

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

  # ============================================================================
  # Initial conditions
  # ============================================================================

  initial(H_C[]) <- init_C[i]
  initial(H_A[]) <- init_A[i]

  # ============================================================================
  # Derived quantities: Prevalence in each population
  # ============================================================================

  dim(I_num_C) <- n_states
  dim(I_den_C) <- n_states
  dim(I_num_A) <- n_states
  dim(I_den_A) <- n_states

  # Prevalence = sum(infected individuals) / sum(all individuals)
  I_num_C[] <- H_C[i] * y[i]
  I_den_C[] <- H_C[i] * hh_size[i]
  I_C <- sum(I_num_C) / sum(I_den_C)

  I_num_A[] <- H_A[i] * y[i]
  I_den_A[] <- H_A[i] * hh_size[i]
  I_A <- sum(I_num_A) / sum(I_den_A)

  # ============================================================================
  # Mixing matrix
  # ============================================================================
  # eps=0: Complete assortativity (groups only contact own group)
  # eps=1: Proportional mixing (contacts proportional to population size)

  w_C <- pop_C / (pop_C + pop_A)
  w_A <- pop_A / (pop_C + pop_A)

  m_CC <- (1 - eps) + eps * w_C
  m_CA <- eps * w_A
  m_AC <- eps * w_C
  m_AA <- (1 - eps) + eps * w_A

  # ============================================================================
  # Force of infection
  # ============================================================================

  lambda_C <- beta * vax_mult_C * (m_CC * I_C + m_CA * I_A)
  lambda_A <- beta * vax_mult_A * (m_AC * I_C + m_AA * I_A)

  # ============================================================================
  # Differential equations: Community population
  # ============================================================================
  # Three transition types:
  #   1. Recovery: infected -> recovered (rate gamma per infected)
  #   2. Within-HH infection: susceptible -> infected from HH member
  #      Rate: (tau + tau_boost*crowded) per S-I pair
  #   3. Between-HH infection: susceptible -> infected from community
  #      Rate: lambda per susceptible

  deriv(H_C[]) <-
    # Recovery transitions
    gamma * (
      -y[i] * H_C[i] +
      if (rec_index[i] > 0) (y[i] + 1) * H_C[rec_index[i]] else 0
    ) +
    # Within-household infection (with crowding boost)
    # Vaccination reduces susceptibility, so vax_mult scales the within-HH
    # force of infection on susceptibles just as it does the between-HH term.
    vax_mult_C * (tau + tau_boost*crowded[i]) * (
      -x[i] * y[i] * H_C[i] +
      if (inf_index[i] > 0) (x[i] + 1) * (y[i] - 1) * H_C[inf_index[i]] else 0
    ) +
    # Between-household infection
    lambda_C * (
      -x[i] * H_C[i] +
      if (inf_index[i] > 0) (x[i] + 1) * H_C[inf_index[i]] else 0
    )

  # ============================================================================
  # Differential equations: Agricultural worker population
  # ============================================================================

  deriv(H_A[]) <-
    # Recovery transitions
    gamma * (
      -y[i] * H_A[i] +
      if (rec_index[i] > 0) (y[i] + 1) * H_A[rec_index[i]] else 0
    ) +
    # Within-household infection (with crowding boost)
    # Vaccination reduces susceptibility, so vax_mult scales the within-HH
    # force of infection on susceptibles just as it does the between-HH term.
    vax_mult_A * (tau + tau_boost*crowded[i]) * (
      -x[i] * y[i] * H_A[i] +
      if (inf_index[i] > 0) (x[i] + 1) * (y[i] - 1) * H_A[inf_index[i]] else 0
    ) +
    # Between-household infection
    lambda_A * (
      -x[i] * H_A[i] +
      if (inf_index[i] > 0) (x[i] + 1) * H_A[inf_index[i]] else 0
    )

})
