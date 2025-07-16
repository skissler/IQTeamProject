library(tidyverse)

generate_household_state_table <- function(n_max = 8) {
  # Create all possible (x, y, z) combinations where x + y + z <= n_max
  states <- expand.grid(
    x = 0:n_max,
    y = 0:n_max,
    z = 0:n_max
  ) %>%
    dplyr::mutate(
      hh_size = x + y + z
    ) %>%
    dplyr::filter(hh_size <= n_max) %>%
    dplyr::filter(hh_size>0) %>% 
    dplyr::arrange(hh_size, x, y, z) %>%
    dplyr::mutate(
      state_index = dplyr::row_number()
    )

  # Helper function to get state index
  find_index <- function(x_, y_, z_) {
    idx <- states %>%
      dplyr::filter(x == x_, y == y_, z == z_) %>%
      dplyr::pull(state_index)
    if (length(idx) == 0) return(0) else return(idx)
  }

  # Compute transitions
  states <- states %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      rec_index = if (z > 0 && y < n_max) find_index(x, y + 1, z - 1) else 0,
      inf_index  = if (y > 0 && x < n_max) find_index(x + 1, y - 1, z) else 0
    ) %>%
    dplyr::ungroup()

  return(states)
}

household_states <- generate_household_state_table(8)

household_model <- odin::odin({

  # Parameters
  n_states <- user()

  dim(x) <- n_states
  dim(y) <- n_states
  dim(z) <- n_states
  dim(hh_size) <- n_states
  dim(rec_index) <- n_states
  dim(inf_index) <- n_states
  dim(init_vec) <- n_states

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

  dim(H) <- n_states

  # Total prevalence across all households
  dim(S_num) <- n_states
  dim(I_num) <- n_states
  dim(R_num) <- n_states
  dim(den) <- n_states

  S_num[] <- H[i] * x[i]
  I_num[] <- H[i] * y[i]
  R_num[] <- H[i] * z[i]
  den[] <- H[i] * hh_size[i]

  S <- sum(S_num)/sum(den)
  I <- sum(I_num)/sum(den)
  R <- sum(R_num)/sum(den)

  initial(H[]) <- init_vec[i]
  # initial(H[]) <- 1/n_states

  deriv(H[]) <- gamma*(
      -y[i]*H[i] + 
      if(rec_index[i]>0) (y[i]+1)*H[rec_index[i]] else 0
      ) + 
    tau*(
      -x[i]*y[i]*H[i] + 
      if(inf_index[i]>0) (x[i]+1)*(y[i]-1)*H[inf_index[i]] else 0
      ) + 
    beta*I*(-x[i]*H[i] + 
      if(inf_index[i]>0) (x[i]+1)*H[inf_index[i]] else 0
      )

  # deriv(H[]) <- gamma*(
  #     -y[i]*H[i] + 
  #     if(rec_index[i]>0, (y[i]+1)*H[rec_index[i]], 0)
  #     ) + 
  #   tau*(
  #     -x[i]*y[i]*H[i] + 
  #     if(inf_index[i]>0, (x[i]+1)*(y[i]-1)*H[inf_index[i]], 0)
  #     ) + 
  #   beta*I*(-x[i]*H[i] + 
  #     if(inf_index[i]>0, (x[i]+1)*H[inf_index[i]], 0)
  #     )


  })


# Load household state definitions
# household_states <- read.csv("household_states.csv")
n_states <- nrow(household_states)

# Start with 1% households in the "4 w/ 1 infected state", the rest fully susceptible
init_vec <- household_states %>% 
  mutate(init=case_when(y==0 & z==0 ~ 1, TRUE~0)) %>% 
  mutate(init=init*(1 - 0.01)/sum(init)) %>% 
  mutate(init=case_when(x==3 & y==1 & z==0 ~ 0.01, TRUE~init)) %>% 
  pull(init)

# Initialize model
mod <- household_model$new(
  n_states = n_states,
  x = household_states$x,
  y = household_states$y,
  z = household_states$z,
  hh_size = household_states$hh_size,
  rec_index = household_states$rec_index,
  inf_index = household_states$inf_index,
  init_vec = init_vec,
  gamma = 1/5,
  tau = (2/3)*(1/5), # 3*(6/5)*(1/5), 
  beta = (6/5)*(1/5)
)

# Simulate
times <- seq(0, 100, by = 1)
out <- mod$run(times)

small <- household_states %>% 
  mutate(small=case_when(hh_size<=3~1, TRUE~0)) %>% 
  pull(small)

# Summarize expected infections over time
I_by_time <- sapply(1:length(times), function(t) {
  sum(out[t, 2:(n_states + 1)] * household_states$y)
})

I_prop_by_time <- sapply(1:length(times), function(t) {
  sum(out[t, 2:(n_states + 1)] * household_states$y) / sum(out[t, 2:(n_states + 1)] * (household_states$x + household_states$y + household_states$z))
})

I_prop_by_time_small <- sapply(1:length(times), function(t) {
  sum(out[t, 2:(n_states + 1)] * household_states$y * small) / sum(out[t, 2:(n_states + 1)] * (household_states$x + household_states$y + household_states$z))
})

I_prop_by_time_big <- sapply(1:length(times), function(t) {
  sum(out[t, 2:(n_states + 1)] * household_states$y * (-small+1)) / sum(out[t, 2:(n_states + 1)] * (household_states$x + household_states$y + household_states$z))
})

# Summarize expected infections over time
N_by_time <- sapply(1:length(times), function(t) {
  sum(out[t, 2:(n_states + 1)])
})

plot(times, I_prop_by_time, type = "l", ylab = "Expected Infected Individuals", xlab = "Time")
lines(times, I_prop_by_time_big)
lines(times, I_prop_by_time_small)

plot(times, I_prop_by_time_big, type="l")
lines(times, 7*I_prop_by_time_small)