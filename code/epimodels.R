library(tidyverse)
library(odin)
source('code/utils.R')

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

  dim(I_num) <- n_states
  dim(I_den) <- n_states
  I_num[] <- H[i] * y[i]
  I_den[] <- H[i] * hh_size[i]
  I <- sum(I_num)/sum(I_den)

  initial(H[]) <- init_vec[i]

  deriv(H[]) <- gamma*(
      -y[i]*H[i] + 
      if(rec_index[i]>0) (y[i]+1)*H[rec_index[i]] else 0) + 
    tau*(
      -x[i]*y[i]*H[i] + 
      if(inf_index[i]>0) (x[i]+1)*(y[i]-1)*H[inf_index[i]] else 0) + 
    beta*I*(
      -x[i]*H[i] + 
      if(inf_index[i]>0) (x[i]+1)*H[inf_index[i]] else 0)
  # print("I: {I}")

  }, debug_enable=TRUE)

# Continue from here -- how should we specify the initial distributions of household sizes? How to deal with population sizes? How to make sure we've got the right mixing matrix approach? 

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
  tau_C <- user()
  tau_A <- user()
  beta_C <- user()
  beta_A <- user()
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

  # Total infected and population size in each group
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

  # Mixing matrix entries: eps=0 is totally assortative, eps=1 is proportional mixing: 
  w_C <- pop_C / (pop_C + pop_A)
  w_A <- pop_A / (pop_C + pop_A)

  m_CC <- (1 - eps) + eps * w_C
  m_CA <- eps * w_A
  m_AC <- eps * w_C
  m_AA <- (1 - eps) + eps * w_A

  # Force of infection
  lambda_C <- beta_C * (m_CC * I_C + m_CA * I_A)
  lambda_A <- beta_A * (m_AC * I_C + m_AA * I_A)

  # Community dynamics
  deriv(H_C[]) <-
    gamma * (
      -y[i] * H_C[i] +
      if (rec_index[i] > 0) (y[i] + 1) * H_C[rec_index[i]] else 0
    ) +
    tau_C * (
      -x[i] * y[i] * H_C[i] +
      if (inf_index[i] > 0) (x[i] + 1) * (y[i] - 1) * H_C[inf_index[i]] else 0
    ) +
    lambda_C * (
      -x[i] * H_C[i] +
      if (inf_index[i] > 0) (x[i] + 1) * H_C[inf_index[i]] else 0
    )

  # Ag worker dynamics
  deriv(H_A[]) <-
    gamma * (
      -y[i] * H_A[i] +
      if (rec_index[i] > 0) (y[i] + 1) * H_A[rec_index[i]] else 0
    ) +
    tau_A * (
      -x[i] * y[i] * H_A[i] +
      if (inf_index[i] > 0) (x[i] + 1) * (y[i] - 1) * H_A[inf_index[i]] else 0
    ) +
    lambda_A * (
      -x[i] * H_A[i] +
      if (inf_index[i] > 0) (x[i] + 1) * H_A[inf_index[i]] else 0
    )

}, debug_enable = TRUE)


household_model_twopop_crowding <- odin::odin({

  # Parameters
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

  # Total infected and population size in each group
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

  # print("I_C: {I_C}")
  # print("I_A: {I_A}")

  # Mixing matrix entries: eps=0 is totally assortative, eps=1 is proportional mixing: 
  w_C <- pop_C / (pop_C + pop_A)
  w_A <- pop_A / (pop_C + pop_A)

  m_CC <- (1 - eps) + eps * w_C
  m_CA <- eps * w_A
  m_AC <- eps * w_C
  m_AA <- (1 - eps) + eps * w_A

  # Force of infection
  lambda_C <- beta_C * (m_CC * I_C + m_CA * I_A)
  lambda_A <- beta_A * (m_AC * I_C + m_AA * I_A)

  # Community dynamics
  deriv(H_C[]) <-
    gamma * (
      -y[i] * H_C[i] +
      if (rec_index[i] > 0) (y[i] + 1) * H_C[rec_index[i]] else 0
    ) +
    (tau_C + tau_boost*crowded[i]) * (
      -x[i] * y[i] * H_C[i] +
      if (inf_index[i] > 0) (x[i] + 1) * (y[i] - 1) * H_C[inf_index[i]] else 0
    ) +
    lambda_C * (
      -x[i] * H_C[i] +
      if (inf_index[i] > 0) (x[i] + 1) * H_C[inf_index[i]] else 0
    )

  # Ag worker dynamics
  deriv(H_A[]) <-
    gamma * (
      -y[i] * H_A[i] +
      if (rec_index[i] > 0) (y[i] + 1) * H_A[rec_index[i]] else 0
    ) +
    (tau_A + tau_boost*crowded[i]) * (
      -x[i] * y[i] * H_A[i] +
      if (inf_index[i] > 0) (x[i] + 1) * (y[i] - 1) * H_A[inf_index[i]] else 0
    ) +
    lambda_A * (
      -x[i] * H_A[i] +
      if (inf_index[i] > 0) (x[i] + 1) * H_A[inf_index[i]] else 0
    )

}, debug_enable = TRUE)


