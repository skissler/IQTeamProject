library(tidyverse)
library(odin)
source('code/utils.R')

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
  print("I: {I}")
    
  }, debug_enable=TRUE)


# Load household state definitions
household_states <- generate_household_state_table(8)
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
out <- as_tibble(data.frame(mod$run(times)))

epidf_hh <- out %>% 
  pivot_longer(-t, names_to="state_index", values_to="prop_hh") %>% 
  mutate(state_index=substr(state_index,3,nchar(state_index)-1)) %>% 
  mutate(state_index=as.numeric(state_index)) %>% 
  left_join(select(household_states, x, y, z, hh_size, state_index), by="state_index") 

epidf_indiv <- epidf_hh %>% 
  mutate(S_num=prop_hh*x, I_num = prop_hh*y, R_num=prop_hh*z, den=prop_hh*hh_size) %>% 
  group_by(t) %>% 
  summarise(S_num=sum(S_num), I_num=sum(I_num), R_num=sum(R_num), den=sum(den)) %>% 
  mutate(S_indiv=S_num/den, I_indiv=I_num/den, R_indiv=R_num/den) %>% 
  select(t, S_indiv, I_indiv, R_indiv)

fig_indiv <- epidf_indiv %>% 
  pivot_longer(-t) %>% 
  ggplot(aes(x=t, y=value, col=name)) + 
    geom_line() + 
    expand_limits(y=0)

# temp %>% 
#   mutate(small=case_when(hh_size<=3~1, TRUE~0)) %>% 
#   group_by(t, small) %>% 
#   summarise(I_indiv = sum(I_indiv)) %>% 
#   ggplot(aes(x=t, y=I_indiv, group=small)) + 
#     geom_line()

# small <- household_states %>% 
#   mutate(small=case_when(hh_size<=3~1, TRUE~0)) %>% 
#   pull(small)

# # Summarize expected infections over time
# I_by_time <- sapply(1:length(times), function(t) {
#   sum(out[t, 2:(n_states + 1)] * household_states$y)
# })

# S_prop_by_time <- sapply(1:length(times), function(t) {
#   sum(out[t, 2:(n_states + 1)] * household_states$x) / sum(out[t, 2:(n_states + 1)] * (household_states$x + household_states$y + household_states$z))
# })

# I_prop_by_time <- sapply(1:length(times), function(t) {
#   sum(out[t, 2:(n_states + 1)] * household_states$y) / sum(out[t, 2:(n_states + 1)] * (household_states$x + household_states$y + household_states$z))
# })

# R_prop_by_time <- sapply(1:length(times), function(t) {
#   sum(out[t, 2:(n_states + 1)] * household_states$z) / sum(out[t, 2:(n_states + 1)] * (household_states$x + household_states$y + household_states$z))
# })

# I_prop_by_time_small <- sapply(1:length(times), function(t) {
#   sum(out[t, 2:(n_states + 1)] * household_states$y * small) / sum(out[t, 2:(n_states + 1)] * (household_states$x + household_states$y + household_states$z))
# })

# I_prop_by_time_big <- sapply(1:length(times), function(t) {
#   sum(out[t, 2:(n_states + 1)] * household_states$y * (-small+1)) / sum(out[t, 2:(n_states + 1)] * (household_states$x + household_states$y + household_states$z))
# })

# # Summarize expected infections over time
# N_by_time <- sapply(1:length(times), function(t) {
#   sum(out[t, 2:(n_states + 1)])
# })

# plot(times, I_prop_by_time, type = "l", ylab = "Expected Infected Individuals", xlab = "Time")
# lines(times, I_prop_by_time_big)
# lines(times, I_prop_by_time_small)

# plot(times, I_prop_by_time_big, type="l")
# lines(times, I_prop_by_time_small)