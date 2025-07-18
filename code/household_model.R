library(tidyverse)
library(odin)
source('code/utils.R')
source('code/epimodels.R')

# Load household state definitions
household_states <- generate_household_state_table(n_min=4, n_max=4)
n_states <- nrow(household_states)

# Start with 1% households in the "4 w/ 1 infected state", the rest fully susceptible
init_vec <- household_states %>% 
  mutate(init=case_when(y==0 & z==0 ~ 1, TRUE~0)) %>% 
  mutate(init=init*(1 - 0.01)/sum(init)) %>% 
  # mutate(init=case_when(x==3 & y==1 & z==0 ~ 0.01, TRUE~init)) %>% 
  mutate(init=case_when(x==0 & y==4 & z==0 ~ 0.01, TRUE~init)) %>% 
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
  tau = (2/3)*(1/5), 
  beta =(6/5)*(1/5)
)

mod_basic <- basic_model$new(
  init_S = 0.99,
  init_I = 0.01, 
  init_R = 0)

# Simulate
times <- seq(0, 100, by = 1)
out <- as_tibble(data.frame(mod$run(times)))
out_basic <- as_tibble(data.frame(mod_basic$run(times)))

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
  mutate(name=substr(name,1,1)) %>% 
  ggplot(aes(x=t, y=value, col=name)) + 
    geom_line() + 
    expand_limits(y=0)

fig_basic <- out_basic %>% 
  pivot_longer(-t) %>% 
  ggplot(aes(x=t, y=value, col=name)) + 
    geom_line() 



# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
# Let's try this now with two populations: 
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 

# Load household state definitions
household_states <- generate_household_state_table(n_min=1, n_max=8)
n_states <- nrow(household_states)

# For community: have all households size 4. Start with 1% of households infected.
init_C <- household_states %>% 
  mutate(init=case_when(
    x==4 & y==0 & z==0 ~ 0.99, 
    x==3 & y==1 & z==0 ~ 0.01,
    TRUE~0)) %>% 
  pull(init)

# For ag workers: have all households size 8. Start with 1% of households infected.
init_A <- household_states %>% 
  mutate(init=case_when(
    x==8 & y==0 & z==0 ~ 0.99, 
    x==7 & y==1 & z==0 ~ 0.01,
    TRUE~0)) %>% 
  pull(init)


# Initialize model
mod_twopop <- household_model_twopop$new(
  n_states = n_states,
  x = household_states$x,
  y = household_states$y,
  z = household_states$z,
  hh_size = household_states$hh_size,
  rec_index = household_states$rec_index,
  inf_index = household_states$inf_index,
  init_C = init_C,
  init_A = init_A,
  gamma = 1/5,
  tau_C = (2/3)*(1/5), 
  tau_A = (2/3)*(1/5), 
  beta_C = (6/5)*(1/5), #(6/5)*(1/5),
  beta_A = (6/5)*(1/5), #(6/5)*(1/5),
  eps = .8,
  pop_C = 5000,
  pop_A = 1000
)

# Simulate
times <- seq(0, 100, by = 1)
out_twopop <- as_tibble(data.frame(mod_twopop$run(times)))

epidf_hh_twopop <- out_twopop %>% 
  pivot_longer(-t, names_to="state_index", values_to="prop_hh") %>% 
  mutate(subpop=substr(state_index,3,3)) %>% 
  mutate(state_index=substr(state_index,5,nchar(state_index)-1)) %>% 
  mutate(state_index=as.numeric(state_index)) %>% 
  left_join(select(household_states, x, y, z, hh_size, state_index), by="state_index") 

epidf_indiv_twopop <- epidf_hh_twopop %>% 
  mutate(S_num=prop_hh*x, I_num = prop_hh*y, R_num=prop_hh*z, den=prop_hh*hh_size) %>% 
  group_by(t, subpop) %>% 
  summarise(S_num=sum(S_num), I_num=sum(I_num), R_num=sum(R_num), den=sum(den)) %>% 
  mutate(S_indiv=S_num/den, I_indiv=I_num/den, R_indiv=R_num/den) %>% 
  select(t, subpop, S_indiv, I_indiv, R_indiv)

fig_indiv_twopop <- epidf_indiv_twopop %>% 
  pivot_longer(c("S_indiv", "I_indiv", "R_indiv")) %>% 
  mutate(name=substr(name,1,1)) %>% 
  filter(name=="I") %>% 
  ggplot(aes(x=t, y=value, col=name, lty=subpop)) + 
    geom_line() + 
    expand_limits(y=0)


fig_rel_inf <- epidf_indiv_twopop %>% 
  select(t, subpop, I_indiv) %>% 
  pivot_wider(names_from="subpop", values_from="I_indiv") %>% 
  mutate(rel_inf=A/C) %>% 
  ggplot(aes(x=t, y=rel_inf)) + 
    geom_line() + 
    expand_limits(y=0)




