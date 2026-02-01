# code/epimodels.R
# Household-structured epidemic models for influenza-agriculture impact analysis
#
# This file defines four progressively complex compartmental disease models
# using the odin package for ODE specification. Models implement the
# House & Keeling (2008) household-structured transmission framework.
#
# Models (in order of complexity):
#   1. basic_model - Simple SIR (baseline reference)
#   2. household_model - Household-stratified SIR (single population)
#   3. household_model_twopop - Two populations with assortative mixing
#   4. household_model_twopop_crowding - Adds household crowding effects (PRIMARY MODEL)
#
# Note: This file expects code/utils_documented.R to be loaded for helper functions.
# In practice, source code/setup.R which handles all dependencies.

library(tidyverse)
library(odin)
source('code/utils_documented.R')

# ==============================================================================
# 1. BASIC SIR MODEL
# ==============================================================================
#
#' Basic SIR Compartmental Model
#'
#' A simple susceptible-infectious-recovered model without household structure.
#' Included as a baseline reference for comparison with household models.
#'
#' @section Mathematical Formulation:
#' \deqn{dS/dt = -\beta S I}
#' \deqn{dI/dt = \beta S I - \gamma I}
#' \deqn{dR/dt = \gamma I}
#'
#' @section Parameters:
#' \describe{
#'   \item{beta}{Transmission rate (default: 6/5 = 1.2, giving R0 = beta/gamma = 6)}
#'   \item{gamma}{Recovery rate (default: 1/5, i.e., 5-day infectious period)}
#'   \item{init_S}{Initial proportion susceptible}
#'   \item{init_I}{Initial proportion infected}
#'   \item{init_R}{Initial proportion recovered}
#' }
#'
#' @section State Variables:
#' \describe{
#'   \item{S}{Proportion of population susceptible}
#'   \item{I}{Proportion of population infected}
#'   \item{R}{Proportion of population recovered}
#' }
#'
#' @section Usage:
#' ```r
#' mod <- basic_model$new(beta = 1.2, gamma = 0.2, init_S = 0.999, init_I = 0.001, init_R = 0)
#' times <- seq(0, 100, by = 1)
#' out <- mod$run(times)
#' ```
#'
#' @references
#' Keeling, M.J. & Rohani, P. (2008). Modeling Infectious Diseases in Humans and Animals.
#'
basic_model <- odin::odin({

  beta <- user(6/5)
  gamma <- user(1/5)
  init_S <- user()
  init_I <- user()
  init_R <- user()

  initial(S) <- init_S
  initial(I) <- init_I
  initial(R) <- init_R

  deriv(S) <- -beta*S*I
  deriv(I) <- beta*S*I - gamma*I
  deriv(R) <- gamma*I

})


# ==============================================================================
# 2. HOUSEHOLD-STRUCTURED SIR MODEL (Single Population)
# ==============================================================================
#
#' Household-Structured SIR Model
#'
#' Implements the House & Keeling (2008) household-structured transmission model
#' for a single population. Tracks the distribution of households across disease
#' states rather than individuals.
#'
#' @section Model Structure:
#' The population is divided into households of varying sizes. Each household
#' is characterized by its composition: (x, y, z) where:
#' - x = number of susceptible members
#' - y = number of infected members
#' - z = number of recovered members
#' - household size n = x + y + z
#'
#' Transmission occurs through two routes:
#' 1. Within-household transmission at rate tau (per susceptible-infected pair)
#' 2. Between-household transmission at rate beta * I (community force of infection)
#'
#' @section Mathematical Formulation:
#' For each household state (x, y, z), the dynamics are:
#'
#' \deqn{dH(x,y,z)/dt = Recovery + Within-HH infection + Between-HH infection}
#'
#' Where:
#' - Recovery: gamma * [(y+1)*H(x,y+1,z-1) - y*H(x,y,z)]
#' - Within-HH: tau * [(x+1)(y-1)*H(x+1,y-1,z) - xy*H(x,y,z)]
#' - Between-HH: beta*I * [(x+1)*H(x+1,y-1,z) - x*H(x,y,z)]
#'
#' @section Parameters:
#' \describe{
#'   \item{n_states}{Number of household states (from generate_household_state_table)}
#'   \item{x[]}{Vector of susceptible counts for each state}
#'   \item{y[]}{Vector of infected counts for each state}
#'   \item{z[]}{Vector of recovered counts for each state}
#'   \item{hh_size[]}{Vector of household sizes for each state}
#'   \item{rec_index[]}{Index of state after recovery transition (y->z)}
#'   \item{inf_index[]}{Index of state after infection transition (x->y)}
#'   \item{init_vec[]}{Initial distribution of households across states}
#'   \item{gamma}{Recovery rate (1/infectious period)}
#'   \item{tau}{Within-household transmission rate}
#'   \item{beta}{Between-household transmission rate}
#' }
#'
#' @section State Variables:
#' \describe{
#'   \item{H[]}{Proportion of households in each state}
#'   \item{I}{Overall prevalence (proportion of individuals infected)}
#' }
#'
#' @section Usage:
#' ```r
#' # Generate state table
#' states <- generate_household_state_table(n_max = 7)
#'
#' # Set initial conditions (all susceptible)
#' init <- rep(0, nrow(states))
#' init[states$x == states$hh_size] <- household_size_distribution
#'
#' # Create model
#' mod <- household_model$new(
#'   n_states = nrow(states),
#'   x = states$x, y = states$y, z = states$z,
#'   hh_size = states$hh_size,
#'   rec_index = states$rec_index,
#'   inf_index = states$inf_index,
#'   init_vec = init,
#'   gamma = 1/5, tau = 0.05, beta = 0.15
#' )
#' ```
#'
#' @references
#' House, T. & Keeling, M.J. (2008). Deterministic epidemic models with explicit
#' household structure. Mathematical Biosciences, 213(1), 29-39.
#'
household_model <- odin::odin({

  # Parameters
  n_states <- user()
  x[] <- user()
  y[] <- user()
  z[] <- user()
  hh_size[] <- user()
  rec_index[] <- user()
  inf_index[] <- user()
  init_vec[] <- user()

  gamma <- user()
  tau <- user()
  beta <- user()

  dim(x) <- n_states
  dim(y) <- n_states
  dim(z) <- n_states
  dim(hh_size) <- n_states
  dim(rec_index) <- n_states
  dim(inf_index) <- n_states
  dim(init_vec) <- n_states

  dim(H) <- n_states

  # Calculate overall prevalence I = sum(y*H) / sum(n*H)
  dim(I_num) <- n_states
  dim(I_den) <- n_states
  I_num[] <- H[i] * y[i]
  I_den[] <- H[i] * hh_size[i]
  I <- sum(I_num)/sum(I_den)

  initial(H[]) <- init_vec[i]

  # Household dynamics: recovery + within-HH infection + between-HH infection
  deriv(H[]) <- gamma*(
      -y[i]*H[i] +
      if(rec_index[i]>0) (y[i]+1)*H[rec_index[i]] else 0) +
    tau*(
      -x[i]*y[i]*H[i] +
      if(inf_index[i]>0) (x[i]+1)*(y[i]-1)*H[inf_index[i]] else 0) +
    beta*I*(
      -x[i]*H[i] +
      if(inf_index[i]>0) (x[i]+1)*H[inf_index[i]] else 0)

})


# ==============================================================================
# 3. TWO-POPULATION HOUSEHOLD MODEL
# ==============================================================================
#
#' Two-Population Household-Structured Model
#'
#' Extends the household model to two populations (community and agricultural
#' workers) with assortative mixing between groups. Each population has its
#' own household size distribution and transmission parameters.
#'
#' @section Model Structure:
#' Two populations (C = community, A = agricultural workers) each follow
#' household-structured dynamics. Populations are coupled through a mixing
#' matrix that determines the force of infection from each group.
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
#' Force of infection:
#' - lambda_C = beta * (m_CC*I_C + m_CA*I_A)
#' - lambda_A = beta * (m_AC*I_C + m_AA*I_A)
#'
#' @section Parameters:
#' \describe{
#'   \item{n_states}{Number of household states}
#'   \item{x[], y[], z[]}{Household composition vectors}
#'   \item{hh_size[]}{Household sizes}
#'   \item{rec_index[], inf_index[]}{Transition indices}
#'   \item{init_C[], init_A[]}{Initial distributions for each population}
#'   \item{gamma}{Recovery rate (shared)}
#'   \item{tau}{Within-household transmission rate (shared by both populations)}
#'   \item{beta}{Between-household transmission rate}
#'   \item{eps}{Assortativity parameter (0 = assortative, 1 = proportional)}
#'   \item{pop_C, pop_A}{Population sizes (for mixing matrix weights)}
#' }
#'
#' @section State Variables:
#' \describe{
#'   \item{H_C[]}{Household distribution for community population}
#'   \item{H_A[]}{Household distribution for agricultural workers}
#'   \item{I_C}{Prevalence in community population}
#'   \item{I_A}{Prevalence in agricultural worker population}
#' }
#'
#' @references
#' House, T. & Keeling, M.J. (2008). Deterministic epidemic models with explicit
#' household structure. Mathematical Biosciences, 213(1), 29-39.
#'
household_model_twopop <- odin::odin({

  # Parameters
  n_states <- user()
  x[] <- user()
  y[] <- user()
  z[] <- user()
  hh_size[] <- user()
  rec_index[] <- user()
  inf_index[] <- user()
  init_C[] <- user()
  init_A[] <- user()

  gamma <- user()
  tau <- user()
  beta <- user()
  eps <- user()
  pop_C <- user()
  pop_A <- user()

  dim(x) <- n_states
  dim(y) <- n_states
  dim(z) <- n_states
  dim(hh_size) <- n_states
  dim(rec_index) <- n_states
  dim(inf_index) <- n_states
  dim(init_C) <- n_states
  dim(init_A) <- n_states

  dim(H_C) <- n_states
  dim(H_A) <- n_states

  initial(H_C[]) <- init_C[i]
  initial(H_A[]) <- init_A[i]

  # Calculate prevalence in each population
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

  # Mixing matrix: eps=0 is totally assortative, eps=1 is proportional mixing
  w_C <- pop_C / (pop_C + pop_A)
  w_A <- pop_A / (pop_C + pop_A)

  m_CC <- (1 - eps) + eps * w_C
  m_CA <- eps * w_A
  m_AC <- eps * w_C
  m_AA <- (1 - eps) + eps * w_A

  # Force of infection for each population
  lambda_C <- beta * (m_CC * I_C + m_CA * I_A)
  lambda_A <- beta * (m_AC * I_C + m_AA * I_A)

  # Community dynamics: recovery + within-HH + between-HH
  deriv(H_C[]) <-
    gamma * (
      -y[i] * H_C[i] +
      if (rec_index[i] > 0) (y[i] + 1) * H_C[rec_index[i]] else 0
    ) +
    tau * (
      -x[i] * y[i] * H_C[i] +
      if (inf_index[i] > 0) (x[i] + 1) * (y[i] - 1) * H_C[inf_index[i]] else 0
    ) +
    lambda_C * (
      -x[i] * H_C[i] +
      if (inf_index[i] > 0) (x[i] + 1) * H_C[inf_index[i]] else 0
    )

  # Agricultural worker dynamics
  deriv(H_A[]) <-
    gamma * (
      -y[i] * H_A[i] +
      if (rec_index[i] > 0) (y[i] + 1) * H_A[rec_index[i]] else 0
    ) +
    tau * (
      -x[i] * y[i] * H_A[i] +
      if (inf_index[i] > 0) (x[i] + 1) * (y[i] - 1) * H_A[inf_index[i]] else 0
    ) +
    lambda_A * (
      -x[i] * H_A[i] +
      if (inf_index[i] > 0) (x[i] + 1) * H_A[inf_index[i]] else 0
    )

})


# ==============================================================================
# 4. TWO-POPULATION MODEL WITH CROWDING (PRIMARY MODEL)
# ==============================================================================
#
#' Two-Population Household Model with Crowding Effects
#'
#' The primary model used in the analysis. Extends the two-population model
#' to include household crowding as a modifier of within-household transmission.
#' Crowded households experience elevated transmission rates.
#'
#' @section Model Structure:
#' Same as household_model_twopop, but with an additional crowding indicator
#' for each household state. Crowded households have within-household
#' transmission rate (tau + tau_boost) instead of tau.
#'
#' @section Crowding Effect:
#' Within-household transmission rate becomes:
#' - Uncrowded (crowded=0): tau_C or tau_A
#' - Crowded (crowded=1): tau_C + tau_boost or tau_A + tau_boost
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
#'   \item{rec_index[], inf_index[]}{Transition indices}
#'   \item{init_C[], init_A[]}{Initial distributions for each population}
#'   \item{gamma}{Recovery rate (1/infectious period)}
#'   \item{tau}{Baseline within-HH transmission rate (shared by both populations)}
#'   \item{tau_boost}{Additional transmission rate for crowded households}
#'   \item{beta}{Between-household transmission rate}
#'   \item{eps}{Assortativity parameter}
#'   \item{pop_C, pop_A}{Population sizes}
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
#'   pop_C = 1000000, pop_A = 50000
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
#' Madewell, Z.J. et al. (2020). Household Transmission of SARS-CoV-2: A Systematic
#' Review and Meta-analysis. JAMA Network Open, 3(12), e2031756.
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

  lambda_C <- beta * (m_CC * I_C + m_CA * I_A)
  lambda_A <- beta * (m_AC * I_C + m_AA * I_A)

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
    (tau + tau_boost*crowded[i]) * (
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
    (tau + tau_boost*crowded[i]) * (
      -x[i] * y[i] * H_A[i] +
      if (inf_index[i] > 0) (x[i] + 1) * (y[i] - 1) * H_A[inf_index[i]] else 0
    ) +
    # Between-household infection
    lambda_A * (
      -x[i] * H_A[i] +
      if (inf_index[i] > 0) (x[i] + 1) * H_A[inf_index[i]] else 0
    )

})
