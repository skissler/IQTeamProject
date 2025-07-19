# //////////////////////////////////////////////////////////////////////////////
# Import
# //////////////////////////////////////////////////////////////////////////////

library(tidyverse)
library(odin)
source('code/utils.R')
source('code/epimodels.R')

source('code/import_naws.R')
source('code/import_acs.R')

# //////////////////////////////////////////////////////////////////////////////
# Derive national dataset 
# //////////////////////////////////////////////////////////////////////////////

nat_data <- acs_data %>% 
	mutate(
		prop=prop*population, 
		prop_crowded=prop_crowded*population) %>%
	group_by(hhSize) %>% 
	summarise(
		prop=sum(prop), 
		prop_crowded=sum(prop_crowded), 
		population=sum(population)) %>% 
	mutate(prop=prop/population, prop_crowded=prop_crowded/population)

# //////////////////////////////////////////////////////////////////////////////
# Try a national calibration
# //////////////////////////////////////////////////////////////////////////////

# Define key variables
max_hh_size <- 7
crowding_fold_diff <- 1

# Load household state definitions
household_states <- generate_household_state_table(n_min=1, n_max=max_hh_size, crowding=TRUE)
n_states <- nrow(household_states)

# Create a data frame to help make initial conditions: 
ic_joiner <- make_ic_joiner(nat_data, fold_diff=crowding_fold_diff)

# Create IC vector for the community. For now, make it so that 1% of households start off infected. All these infected households will be 2-person households with a single infected member. 
init_nat_C <- household_states %>% 
	left_join(ic_joiner, by=c("x","y","z","hh_size","crowded")) %>% 
	arrange(state_index) %>% 
	replace_na(list(frac=0)) %>% 
	mutate(frac=case_when(x==2 & y==0 & z==0 & crowded==0 ~ frac-0.01, TRUE~frac)) %>% 
	mutate(frac=case_when(x==1 & y==1 & z==0 & crowded==0 ~ 0.01, TRUE~frac)) %>% 
	pull(frac)

# Use a dummy IC for ag workers (everyone in 1-person households and recovered)
init_nat_A <- rep(0, n_states)
init_nat_A[1] <- 1

# Initialize model
mod_twopop_crowding <- household_model_twopop_crowding$new(
  n_states = n_states,
  x = household_states$x,
  y = household_states$y,
  z = household_states$z,
  hh_size = household_states$hh_size,
  crowded = household_states$crowded,
  rec_index = household_states$rec_index,
  inf_index = household_states$inf_index,
  init_C = init_nat_C,
  init_A = init_nat_A,
  gamma = 1/5,
  tau_C = (2/3)*(1/5), 
  tau_A = 0, 
  tau_boost = 1/3,
  beta_C = (6/5)*(1/5),
  beta_A = 0,
  eps = 0,
  pop_C = 1000,
  pop_A = 0
)

# Simulate
times <- seq(0, 100, by = 1)
out_twopop_crowding <- as_tibble(data.frame(mod_twopop_crowding$run(times)))

epidf_hh_twopop_crowding <- out_twopop_crowding %>% 
  pivot_longer(-t, names_to="state_index", values_to="prop_hh") %>% 
  mutate(subpop=substr(state_index,3,3)) %>% 
  mutate(state_index=substr(state_index,5,nchar(state_index)-1)) %>% 
  mutate(state_index=as.numeric(state_index)) %>% 
  left_join(select(household_states, x, y, z, hh_size, state_index, crowded), by="state_index") 

epidf_indiv_twopop_crowding <- epidf_hh_twopop_crowding %>% 
  mutate(S_num=prop_hh*x, I_num = prop_hh*y, R_num=prop_hh*z, den=prop_hh*hh_size) %>% 
  group_by(t, subpop) %>% 
  summarise(S_num=sum(S_num), I_num=sum(I_num), R_num=sum(R_num), den=sum(den)) %>% 
  mutate(S_indiv=S_num/den, I_indiv=I_num/den, R_indiv=R_num/den) %>% 
  select(t, subpop, S_indiv, I_indiv, R_indiv)

fig_indiv_twopop_crowding <- epidf_indiv_twopop_crowding %>% 
  pivot_longer(c("S_indiv", "I_indiv", "R_indiv")) %>% 
  mutate(name=substr(name,1,1)) %>% 
  # filter(name=="I") %>% 
  ggplot(aes(x=t, y=value, col=name, lty=subpop)) + 
    geom_line() + 
    expand_limits(y=0)

fig_rel_inf_crowding <- epidf_indiv_twopop_crowding %>% 
  select(t, subpop, I_indiv) %>% 
  pivot_wider(names_from="subpop", values_from="I_indiv") %>% 
  mutate(rel_inf=A/C) %>% 
  ggplot(aes(x=t, y=rel_inf)) + 
    geom_line() 

# For R0 = 2, we're looking for a final size of 0.7968
print(last(epidf_indiv_twopop_crowding$R_indiv))
# that's pretty close 



# # For community: have all households size 4. Start with 1% of households infected.
# init_C <- household_states %>% 
#   mutate(init=case_when(
#     x==4 & y==0 & z==0 & crowded==0 ~ 0.99/2, 
#     x==3 & y==1 & z==0 & crowded==0 ~ 0.01/2,
#     x==4 & y==0 & z==0 & crowded==1 ~ 0.99/2, 
#     x==3 & y==1 & z==0 & crowded==1 ~ 0.01/2,
#     TRUE~0)) %>% 
#   pull(init)

# # For ag workers: have all households size 8. Start with 1% of households infected.
# init_A <- household_states %>% 
#   mutate(init=case_when(
#     x==8 & y==0 & z==0 & crowded==0 ~ 0.99/2, 
#     x==7 & y==1 & z==0 & crowded==0 ~ 0.01/2,
#     x==8 & y==0 & z==0 & crowded==1 ~ 0.99/2, 
#     x==7 & y==1 & z==0 & crowded==1 ~ 0.01/2,
#     TRUE~0)) %>% 
#   pull(init)






