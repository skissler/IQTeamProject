# //////////////////////////////////////////////////////////////////////////////
# Import
# //////////////////////////////////////////////////////////////////////////////

library(tidyverse)
library(odin)
library(future.apply) # for parallelizing lapply
plan(multisession)
source('code/utils.R')
source('code/epimodels.R')

source('code/import_naws.R')
source('code/import_acs.R')

# //////////////////////////////////////////////////////////////////////////////
# Derive dataset for ag and non-ag households 
# //////////////////////////////////////////////////////////////////////////////

# update population sizes/fraction of agricultural workers; check all inputs to make sure they're updating correctly; check that crowding is implemented correctly. 

# Define key variables
# max_hh_size <- pars$max_hh_size
# crowding_fold_diff <- pars$crowding_fold_diff
# adjust_hhvars <- pars$adjust_hhvars
# init_prev <- pars$init_prev

run_county_sim <- function(pars, acs_data, naws_data){
with(as.list(pars), {
# Load household state definitions
household_states <- generate_household_state_table(n_min=1, n_max=max_hh_size, crowding=TRUE)
n_states <- nrow(household_states)

# Pre-split the acs data: 
acs_data_list <- split(acs_data, acs_data$GEOID)

# epidf_indiv_full <- tibble()

# for(geoid in GEOID_vec){
# for(geoid in GEOID_vec[1:500]){
# for(geoid in GEOID_vec[1:5]){
results_list <- future_lapply(GEOID_vec[1:100], function(geoid){

	county_data <- acs_data_list[[geoid]]

	# Which region is our county in? 
	region <- county_lookup %>% 
		filter(GEOID == geoid) %>% 
		pull(REGION6) %>% 
		first()

	# Get the adjustments: 
	hhSize_factor <- county_data %>% 
		arrange(hhSize) %>% 
		pull(hhSize_factor)

	crowded_factor <- county_data %>% 
		pull(crowded_factor) %>% 
		first()

	# Create the ic joiners: 
	ic_joiner_C <- county_data %>% 
		make_ic_joiner(fold_diff=crowding_fold_diff)

	if(adjust_hhvars){
		naws_data_processed <- naws_data %>% 
			filter(REGION6==region) %>% 
			mutate(hhSize_factor=hhSize_factor, crowded_factor=crowded_factor) %>% 
			mutate(prop=prop*hhSize_factor) %>% 
			mutate(prop=prop/sum(prop)) %>% 
			mutate(prop_crowded=prop_crowded*crowded_factor) %>% 
			mutate(prop_crowded=case_when(prop_crowded>1~1, TRUE~prop_crowded)) %>% 
			select(-hhSize_factor, -crowded_factor)
	} else {
		naws_data_processed <- naws_data %>% 
			filter(REGION6==region)
	}

	ic_joiner_A <- naws_data_processed %>% 
		make_ic_joiner(fold_diff=crowding_fold_diff)

	# Adjust the ic joiners to reflect initial infected: 
	ic_joiner_A_inf <- ic_joiner_A %>% 
  	mutate(frac=init_prev*frac*1*hh_size) %>% 
  	mutate(y=y+1, x=x-1)
  ic_joiner_A$frac = ic_joiner_A$frac - ic_joiner_A_inf$frac
  ic_joiner_A <- bind_rows(ic_joiner_A, ic_joiner_A_inf)

  ic_joiner_C_inf <- ic_joiner_C %>% 
  	mutate(frac=init_prev*frac*1*hh_size) %>% 
  	mutate(y=y+1, x=x-1)
  ic_joiner_C$frac = ic_joiner_C$frac - ic_joiner_C_inf$frac
  ic_joiner_C <- bind_rows(ic_joiner_C, ic_joiner_C_inf)

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

	pop_cty <- county_data %>% 
		pull(population) %>% 
		first()

	prop_ag <- county_data %>% 
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
	  gamma = gamma,
	  tau_C = tau_C, # 20% SAR
	  tau_A = tau_A, 
	  tau_boost = tau_boost, # Boosts to a 40% SAR 
	  beta_C = beta_C, 
	  beta_A = beta_A,
	  eps = eps, # 0.4 # 0.52
	  pop_C = pop_C,
	  pop_A = pop_A
	)

	# Simulate
	times <- seq(0, 100, by = 1)
	out <- as_tibble(data.frame(mod$run(times)))

	epidf_hh <- format_output_hh(out, household_states)
	epidf_indiv <- format_output_indiv(out, household_states)

	# epidf_indiv_full <- bind_rows(epidf_indiv_full, mutate(epidf_indiv,GEOID=geoid,REGION6=region))

	counter <- which(GEOID_vec==geoid)
	if(counter %% 20 == 0){
		print(counter)	
	}

	return(mutate(epidf_indiv, GEOID=geoid, REGION6=region))
})

epidf_indiv_full <- bind_rows(results_list)
return(epidf_indiv_full)
})
}

epidf_indiv_full <- run_county_sim(pars, acs_data, naws_data)

# write_csv(epidf_indiv_full, file="output/epidf_indiv_full.csv")