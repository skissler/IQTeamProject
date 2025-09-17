# //////////////////////////////////////////////////////////////////////////////
# Import
# //////////////////////////////////////////////////////////////////////////////

library(tidyverse)
library(odin)
source('code/utils.R')
source('code/epimodels.R')

# source('code/import_naws.R')
# source('code/import_acs.R')

# //////////////////////////////////////////////////////////////////////////////
# Generate regional summaries for the acs
# //////////////////////////////////////////////////////////////////////////////

acs_data_regional <- acs_data %>% 
	group_by(REGION6, hhSize) %>% 
	mutate(
		prop=prop*population, 
		prop_crowded=prop_crowded*population,
		prop_ag_workers=prop_ag_workers*population) %>% 
	summarise(
		prop=sum(prop), 
		prop_crowded=sum(prop_crowded), 
		prop_ag_workers=sum(prop_ag_workers), 
		population=sum(population)) %>% 
	mutate(
		prop=prop/population,
		prop_crowded=prop_crowded/population,
		prop_ag_workers=prop_ag_workers/population)

# //////////////////////////////////////////////////////////////////////////////
# Run the simulation
# //////////////////////////////////////////////////////////////////////////////

# Define key variables
max_hh_size <- 7
crowding_fold_diff <- 2
adjust_hhvars <- TRUE
init_prev <- 0.001

# Load household state definitions
household_states <- generate_household_state_table(n_min=1, n_max=max_hh_size, crowding=TRUE)
n_states <- nrow(household_states)

epidf_indiv_full <- tibble()

for(region in 1:6){

	# Create the ic joiners: 
	ic_joiner_C <- acs_data_regional %>% 
		filter(REGION6==region) %>% 
		make_ic_joiner(fold_diff=crowding_fold_diff)

	ic_joiner_A <- naws_data %>% 
		filter(REGION6==region) %>% 
		make_ic_joiner(fold_diff=crowding_fold_diff)

	# Adjust the ic joiners to reflect initial infected: 
	ic_joiner_C_inf <- ic_joiner_C %>% 
		mutate(frac=init_prev*frac*hh_size) %>% 
		mutate(y=y+1, x=x-1)
		ic_joiner_C$frac = ic_joiner_C$frac - ic_joiner_C_inf$frac
		ic_joiner_C <- bind_rows(ic_joiner_C, ic_joiner_C_inf)

	ic_joiner_A_inf <- ic_joiner_A %>% 
		mutate(frac=init_prev*frac*hh_size) %>% 
		mutate(y=y+1, x=x-1)
		ic_joiner_A$frac = ic_joiner_A$frac - ic_joiner_A_inf$frac
		ic_joiner_A <- bind_rows(ic_joiner_A, ic_joiner_A_inf)

	# Create the initial conditions: 
	init_C <- household_states %>% 
		left_join(ic_joiner_C, by=c("x","y","z","hh_size","crowded")) %>% 
		arrange(state_index) %>% 
		replace_na(list(frac=0)) %>% 
		pull(frac)

	init_A <- household_states %>% 
		left_join(ic_joiner_A, by=c("x","y","z","hh_size","crowded")) %>% 
		arrange(state_index) %>% 
		replace_na(list(frac=0)) %>% 
		pull(frac)

	pop_cty <- acs_data_regional %>% 
		filter(REGION6==region) %>% 
		pull(population) %>% 
		first()

	prop_ag <- acs_data_regional %>% 
		filter(REGION6==region) %>% 
		pull(prop_ag_workers) %>% 
		first()

	pop_C <- pop_cty*(1-prop_ag)
	pop_A <- pop_cty*prop_ag

	# Initialize model
	mod <- household_model_twopop_crowding$new(
	  n_states = n_states,
	  x = household_states$x,
	  y = household_states$y,
	  z = household_states$z,
	  hh_size = household_states$hh_size,
	  crowded = household_states$crowded,
	  rec_index = household_states$rec_index,
	  inf_index = household_states$inf_index,
	  init_C = init_C,
	  init_A = init_A,
	  gamma = 1/5,
	  tau_C = (1/4)*(1/5), # 20% SAR
	  tau_A = (1/4)*(1/5), 
	  tau_boost = (2/3)*(1/5) - (1/4)*(1/5), # Boosts to a 40% SAR 
	  beta_C = .765*(1/5), 
	  beta_A = .765*(1/5),
	  eps = 0.33, # 0.4 # 0.52
	  pop_C = pop_C,
	  pop_A = pop_A
	)

	# Simulate
	times <- seq(0, 365, by = 1)
	out <- as_tibble(data.frame(mod$run(times)))

	epidf_hh <- format_output_hh(out, household_states)
	epidf_indiv <- format_output_indiv(out, household_states)

	epidf_indiv_full <- bind_rows(epidf_indiv_full, mutate(epidf_indiv,REGION6=region))

}

# init_A_comp <- rep(0, length(init_A))
# init_A_comp[1] <- 1
# mod_comp <- household_model_twopop_crowding$new(
#   n_states = n_states,
#   x = household_states$x,
#   y = household_states$y,
#   z = household_states$z,
#   hh_size = household_states$hh_size,
#   crowded = household_states$crowded,
#   rec_index = household_states$rec_index,
#   inf_index = household_states$inf_index,
#   init_C = init_C,
#   init_A = init_A_comp,
#   gamma = 1/5,
#   tau_C = (1/4)*(1/5), # 20% SAR
#   tau_A = 0, 
#   tau_boost = (2/3)*(1/5) - (1/4)*(1/5), # Boosts to a 40% SAR 
#   beta_C = 1.53*(1/5), 
#   beta_A = 0,
#   eps = 0.4, # 0.4 # 0.52
#   pop_C = pop_C,
#   pop_A = 0
# )
# # Simulate
# times <- seq(0, 100, by = 1)
# out_comp <- as_tibble(data.frame(mod_comp$run(times)))
# epidf_indiv_comp <- format_output_indiv(out_comp, household_states)
# fs <- epidf_indiv_comp %>% pull(R_indiv) %>% last()

fig_indiv_full_I_regionfacet <- epidf_indiv_full %>% 
  pivot_longer(c("S_indiv", "I_indiv", "R_indiv")) %>% 
  mutate(name=substr(name,1,1)) %>% 
  filter(name=="I") %>% 
	ggplot(aes(x = t, y = value, col = subpop)) +
	  geom_line(alpha=1, linewidth=0.8) +
	  geom_hline(yintercept=0.005, lty="dashed", alpha=0.2) + 
	  labs(x = "Time", y = "Proportion", col = "Subpopulation") +
	  theme_minimal() + 
	  facet_wrap(~factor(REGION6), nrow=2)
	  

fig_indiv_full_R_regionfacet <- epidf_indiv_full %>% 
  pivot_longer(c("S_indiv", "I_indiv", "R_indiv")) %>% 
  mutate(name=substr(name,1,1)) %>% 
  filter(name=="R") %>% 
	ggplot(aes(x = t, y = value, col = subpop)) +
	  geom_line(alpha=1, linewidth=0.8) +
	  geom_hline(yintercept=0.005, lty="dashed", alpha=0.2) + 
	  labs(x = "Time", y = "Proportion", col = "Subpopulation") +
	  theme_minimal() + 
	  facet_wrap(~factor(REGION6), nrow=2)


fig_rel_inf <- epidf_indiv_full %>% 
  select(t, subpop, I_indiv, REGION6) %>% 
  pivot_wider(names_from="subpop", values_from="I_indiv") %>% 
  mutate(rel_inf=A/C) %>% 
  ggplot(aes(x=t, y=rel_inf, group=REGION6)) + 
    geom_line(alpha=0.8, linewidth=1) + 
    geom_hline(yintercept=1, lty="dashed", col="lightgrey") + 
    expand_limits(y=0.5) + 
    theme_classic() 