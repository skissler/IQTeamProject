# //////////////////////////////////////////////////////////////////////////////
# Simulate County-Level Epidemics
# //////////////////////////////////////////////////////////////////////////////
# Runs the household-structured epidemic model for each US county, using
# county-specific household size distributions and crowding rates.
#
# Requires: pars object (from parameters.R or direct definition)
# Outputs: county_sim.csv in output directory
# //////////////////////////////////////////////////////////////////////////////

# Load dependencies (skip if already loaded via run_analysis.R)
if (!exists("paths")) {
  source('code/setup.R')
}

# Set up parallel processing
if (sim_settings$use_parallel) {
  future::plan(future::multisession)
}

# Import data (skip if already loaded)
if (!exists("naws_data")) {
  source('code/import_naws.R')
}
if (!exists("acs_data")) {
  source('code/import_acs.R')
}

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

# //////////////////////////////////////////////////////////////////////////////
# Pre-compute lookups to avoid repeated filtering inside the loop
# //////////////////////////////////////////////////////////////////////////////

# Pre-filter NAWS data by region (6 times instead of ~3000)
naws_by_region <- lapply(1:6, function(r) {
  naws_data %>% filter(REGION6 == r)
})

# Region lookup: named vector for O(1) access
county_regions <- setNames(county_lookup$REGION6, county_lookup$GEOID)

# Pre-compute scalar values per county
county_scalars <- acs_data %>%
  group_by(GEOID) %>%
  summarise(
    population = first(population),
    prop_ag = first(prop_ag_workers),
    crowded_factor = first(crowded_factor),
    crowded_diff = first(crowded_diff),
    .groups = "drop"
  )
scalars_list <- split(county_scalars, county_scalars$GEOID)

# Pre-compute hhSize vectors per county (ordered by hhSize)
hhsize_data <- acs_data %>%
  arrange(GEOID, hhSize) %>%
  group_by(GEOID) %>%
  summarise(
    hhSize_factor = list(hhSize_factor),
    hhSize_diff = list(hhSize_diff),
    .groups = "drop"
  )
hhsize_list <- setNames(
  Map(list, hhsize_data$hhSize_factor, hhsize_data$hhSize_diff),
  hhsize_data$GEOID
)

# Pre-split the acs data for ic_joiner creation:
acs_data_list <- split(acs_data, acs_data$GEOID)

# //////////////////////////////////////////////////////////////////////////////

results_list <- future_lapply(GEOID_vec, function(geoid){

	county_data <- acs_data_list[[geoid]]

	# Which region is our county in? (O(1) lookup)
	region <- county_regions[[geoid]]

	# Get scalar values (O(1) lookup)
	scalars <- scalars_list[[geoid]]
	pop_cty <- scalars$population
	prop_ag <- scalars$prop_ag
	crowded_factor <- scalars$crowded_factor
	crowded_diff <- scalars$crowded_diff

	# Get hhSize vectors (O(1) lookup)
	hhSize_factor <- hhsize_list[[geoid]][[1]]
	hhSize_diff <- hhsize_list[[geoid]][[2]]

	# Create the ic joiners:
	ic_joiner_C <- county_data %>%
		make_ic_joiner(fold_diff=crowding_fold_diff)

	# Adjust NAWS data based on adjust_hhvars setting:
	#   "none"           - Use regional NAWS data directly
	#   "multiplicative" - Multiply by (county/regional_mean) ratio
	#   "additive"       - Add (county - regional_mean) difference
	# Start with pre-filtered regional data
	naws_regional <- naws_by_region[[region]]

	if (adjust_hhvars == "multiplicative") {
		naws_data_processed <- naws_regional %>%
			mutate(hhSize_factor=hhSize_factor, crowded_factor=crowded_factor) %>%
			mutate(prop=prop*hhSize_factor) %>%
			mutate(prop=prop/sum(prop)) %>%
			mutate(prop_crowded=prop_crowded*crowded_factor) %>%
			mutate(prop_crowded=case_when(prop_crowded>1~1, prop_crowded<0~0, TRUE~prop_crowded)) %>%
			select(-hhSize_factor, -crowded_factor)
	} else if (adjust_hhvars == "additive") {
		naws_data_processed <- naws_regional %>%
			mutate(hhSize_diff=hhSize_diff, crowded_diff=crowded_diff) %>%
			mutate(prop=prop + hhSize_diff) %>%
			mutate(prop=case_when(prop<0~0, TRUE~prop)) %>%  # Clamp to >= 0
			mutate(prop=prop/sum(prop)) %>%                   # Renormalize
			mutate(prop_crowded=prop_crowded + crowded_diff) %>%
			mutate(prop_crowded=case_when(prop_crowded>1~1, prop_crowded<0~0, TRUE~prop_crowded)) %>%
			select(-hhSize_diff, -crowded_diff)
	} else {
		# "none" or any other value: use regional NAWS data directly
		naws_data_processed <- naws_regional
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

	# pop_cty and prop_ag already extracted from scalars_list above
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
	  tau = tau,             # Derived from sar_uncrowded
	  tau_boost = tau_boost, # Boosts from sar_uncrowded to sar_crowded
	  beta = beta,
	  eps = eps,
	  pop_C = pop_C,
	  pop_A = pop_A
	)

	# Simulate
	times <- seq(0, sim_settings$t_max, by = sim_settings$t_step)
	out <- as_tibble(data.frame(mod$run(times)))

	epidf_hh <- format_output_hh(out, household_states)
	epidf_indiv <- format_output_indiv(out, household_states)

	# epidf_indiv_full <- bind_rows(epidf_indiv_full, mutate(epidf_indiv,GEOID=geoid,REGION6=region))

	return(mutate(epidf_indiv, GEOID=geoid, REGION6=region))
}, future.seed = TRUE)

epidf_indiv_full <- bind_rows(results_list)
return(epidf_indiv_full)
})
}

epidf_indiv_full <- run_county_sim(pars, acs_data, naws_data)

write_csv(epidf_indiv_full, file = paths$county_output)