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
# Derive dataset for ag and non-ag households 
# //////////////////////////////////////////////////////////////////////////////

# update population sizes/fraction of agricultural workers; check all inputs to make sure they're updating correctly; check that crowding is implemented correctly. 

# Define key variables
max_hh_size <- 7
crowding_fold_diff <- 2
adjust_hhvars <- TRUE

# Load household state definitions
household_states <- generate_household_state_table(n_min=1, n_max=max_hh_size, crowding=TRUE)
n_states <- nrow(household_states)

epidf_indiv_full <- tibble()

for(geoid in GEOID_vec){
# for(geoid in GEOID_vec[1:500]){

	# Which region is our county in? 
	region <- county_lookup %>% 
		filter(GEOID == geoid) %>% 
		pull(REGION6) %>% 
		first()

	# Get the adjustments: 
	hhSize_factor <- acs_data %>% 
		filter(GEOID==geoid) %>% 
		arrange(hhSize) %>% 
		pull(hhSize_factor)

	crowded_factor <- acs_data %>% 
		filter(GEOID==geoid) %>% 
		pull(crowded_factor) %>% 
		first()

	# Create the ic joiners: 
	ic_joiner_C <- acs_data %>% 
		filter(GEOID==geoid) %>% 
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

	# Create the initial conditions: 
	init_C <- household_states %>% 
		left_join(ic_joiner_C, by=c("x","y","z","hh_size","crowded")) %>% 
		arrange(state_index) %>% 
		replace_na(list(frac=0)) %>% 
		mutate(frac=case_when(x==2 & y==0 & z==0 & crowded==0 ~ frac-0.01, TRUE~frac)) %>% 
		mutate(frac=case_when(x==1 & y==1 & z==0 & crowded==0 ~ 0.01, TRUE~frac)) %>% 
		pull(frac)

	init_A <- household_states %>% 
		left_join(ic_joiner_A, by=c("x","y","z","hh_size","crowded")) %>% 
		arrange(state_index) %>% 
		replace_na(list(frac=0)) %>% 
		mutate(frac=case_when(x==2 & y==0 & z==0 & crowded==0 ~ frac-0.01, TRUE~frac)) %>% 
		mutate(frac=case_when(x==1 & y==1 & z==0 & crowded==0 ~ 0.01, TRUE~frac)) %>% 
		pull(frac)

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
	  tau_boost = (2/3) - (1/4), # Boosts to a 40% SAR 
	  beta_C = 1.52*(1/5), 
	  beta_A = 1.52*(1/5),
	  eps = 0.4, # 0.52
	  pop_C = 476877,
	  pop_A = 11559
	)

	# Simulate
	times <- seq(0, 100, by = 1)
	out <- as_tibble(data.frame(mod$run(times)))

	epidf_hh <- format_output_hh(out, household_states)
	epidf_indiv <- format_output_indiv(out, household_states)

	epidf_indiv_full <- bind_rows(epidf_indiv_full, mutate(epidf_indiv,GEOID=geoid))

	counter <- which(GEOID_vec==geoid)
	if(counter %% 20 == 0){
		print(counter)	
	}

}

epidf_indiv_overallmean <- epidf_indiv_full %>% 
	group_by(t, subpop) %>% 
	summarise(S_indiv=mean(S_indiv), I_indiv=mean(I_indiv), R_indiv=mean(R_indiv))

epidf_indiv_regionalmean <- epidf_indiv_full %>% 
	left_join(st_drop_geometry(select(county_lookup, GEOID, REGION6)), by="GEOID") %>% 
	group_by(t, subpop, REGION6) %>% 
	summarise(S_indiv=mean(S_indiv), I_indiv=mean(I_indiv), R_indiv=mean(R_indiv))



fig_indiv_full <- epidf_indiv_full %>% 
  pivot_longer(c("S_indiv", "I_indiv", "R_indiv")) %>% 
  mutate(name=substr(name,1,1)) %>% 
  # filter(name=="I") %>% 
	ggplot(aes(x = t, y = value, color = name, linetype = subpop, group = interaction(GEOID, name, subpop))) +
	  geom_line(alpha=0.1) +
	  labs(x = "Time", y = "Proportion", color = "Compartment", linetype = "Subpopulation") +
	  theme_minimal()

fig_indiv_full_I <- epidf_indiv_full %>% 
  pivot_longer(c("S_indiv", "I_indiv", "R_indiv")) %>% 
  mutate(name=substr(name,1,1)) %>% 
  filter(name=="I") %>% 
	ggplot(aes(x = t, y = value, col = subpop, group = interaction(GEOID, name, subpop))) +
	  geom_line(alpha=0.2) +
	  labs(x = "Time", y = "Proportion", col = "Subpopulation") +
	  theme_minimal()

fig_indiv_full_I_regioncolor <- epidf_indiv_full %>% 
  pivot_longer(c("S_indiv", "I_indiv", "R_indiv")) %>% 
  left_join(st_drop_geometry(select(county_lookup, GEOID, REGION6)), by="GEOID") %>% 
  mutate(name=substr(name,1,1)) %>% 
  filter(name=="I") %>% 
	ggplot(aes(x = t, y = value, lty = subpop, col=factor(REGION6), group = interaction(GEOID, name, subpop, REGION6))) +
	  geom_line(alpha=0.2) +
	  labs(x = "Time", y = "Proportion", col = "Subpopulation") +
	  theme_minimal() + 
	  scale_color_manual(values=c("white","white","white","white","blue","white")) 

# geoid_subset <- GEOID_vec
geoid_subset <- acs_data %>% 
	filter(prop_ag_workers > 0.05) %>% 
	# filter(prop_rural > 0.5) %>% 
	pull(GEOID) %>% 
	unique()

fig_indiv_full_I_regionfacet <- epidf_indiv_full %>% 
  filter(GEOID %in% geoid_subset) %>% 
  pivot_longer(c("S_indiv", "I_indiv", "R_indiv")) %>% 
  left_join(st_drop_geometry(select(county_lookup, GEOID, REGION6)), by="GEOID") %>% 
  mutate(name=substr(name,1,1)) %>% 
  filter(name=="I") %>% 
	ggplot(aes(x = t, y = value, col = subpop, group = interaction(GEOID, name, subpop))) +
	  geom_line(alpha=0.2) +
	  labs(x = "Time", y = "Proportion", col = "Subpopulation") +
	  theme_minimal() + 
	  facet_wrap(~factor(REGION6), nrow=2)

epidf_indiv_overallmean %>% 
  pivot_longer(c("S_indiv", "I_indiv", "R_indiv")) %>% 
  mutate(name=substr(name,1,1)) %>% 
  filter(name=="I") %>% 
	ggplot(aes(x = t, y = value, col = subpop, group = interaction(name, subpop))) +
	  geom_line(alpha=0.2) +
	  labs(x = "Time", y = "Proportion", col = "Subpopulation") +
	  theme_minimal()

epidf_indiv_regionalmean %>% 
  pivot_longer(c("S_indiv", "I_indiv", "R_indiv")) %>% 
  mutate(name=substr(name,1,1)) %>% 
  filter(name=="I") %>% 
	ggplot(aes(x = t, y = value, col = factor(REGION6), lty=subpop, group = interaction(REGION6, name, subpop))) +
	  geom_line(alpha=1) +
	  scale_color_manual(values=c("red","blue","green","orange","black","magenta")) + 
	  labs(x = "Time", y = "Proportion", col = "Subpopulation") +
	  theme_minimal()


  # ggplot(aes(x=t, y=value, col=name, lty=subpop, group=factor(GEOID))) + 
  #   geom_line() + 
  #   expand_limits(y=0)


fig_rel_inf <- epidf_indiv_full %>% 
  select(t, subpop, I_indiv, GEOID) %>% 
  pivot_wider(names_from="subpop", values_from="I_indiv") %>% 
  mutate(rel_inf=A/C) %>% 
  ggplot(aes(x=t, y=rel_inf, group=GEOID)) + 
    geom_line(alpha=0.2) 


# temp2 <- temp %>% 
# 	left_join(naws_data, by=c("REGION6","hhSize"))

# temp %>% 
# 	adjust_crowding(fold_diff=2, indexcols=c("GEOID")) %>% 
# 	print(width=Inf)

# ic_joiner_C <- county_lookup %>% 
# 	st_drop_geometry() %>% 
# 	# filter(GEOID==first(GEOID)) %>% 
# 	filter(GEOID=="06097") %>% 
# 	make_ic_joiner(fold_diff=1)

# ic_joiner_A <- naws_data %>% 
# 	# filter(REGION6==2) %>% 
# 	filter(REGION6==6) %>% 
# 	make_ic_joiner(fold_diff=1)

# # Define key variables
# max_hh_size <- 7
# crowding_fold_diff <- 1

# # Load household state definitions
# household_states <- generate_household_state_table(n_min=1, n_max=max_hh_size, crowding=TRUE)
# n_states <- nrow(household_states)

# init_C <- household_states %>% 
# 	left_join(ic_joiner_C, by=c("x","y","z","hh_size","crowded")) %>% 
# 	arrange(state_index) %>% 
# 	replace_na(list(frac=0)) %>% 
# 	mutate(frac=case_when(x==2 & y==0 & z==0 & crowded==0 ~ frac-0.01, TRUE~frac)) %>% 
# 	mutate(frac=case_when(x==1 & y==1 & z==0 & crowded==0 ~ 0.01, TRUE~frac)) %>% 
# 	pull(frac)

# init_A <- household_states %>% 
# 	left_join(ic_joiner_A, by=c("x","y","z","hh_size","crowded")) %>% 
# 	arrange(state_index) %>% 
# 	replace_na(list(frac=0)) %>% 
# 	mutate(frac=case_when(x==2 & y==0 & z==0 & crowded==0 ~ frac-0.01, TRUE~frac)) %>% 
# 	mutate(frac=case_when(x==1 & y==1 & z==0 & crowded==0 ~ 0.01, TRUE~frac)) %>% 
# 	pull(frac)

# # Initialize model
# mod <- household_model_twopop_crowding$new(
#   n_states = n_states,
#   x = household_states$x,
#   y = household_states$y,
#   z = household_states$z,
#   hh_size = household_states$hh_size,
#   crowded = household_states$crowded,
#   rec_index = household_states$rec_index,
#   inf_index = household_states$inf_index,
#   init_C = init_C,
#   init_A = init_A,
#   gamma = 1/5,
#   tau_C = (1/4)*(1/5), # 20% SAR
#   tau_A = (1/4)*(1/5), 
#   tau_boost = (2/3) - (1/4), # Boosts to a 40% SAR 
#   beta_C = 1.52*(1/5), 
#   beta_A = 1.52*(1/5),
#   eps = 0.8,
#   pop_C = 476877,
#   pop_A = 11559
# )

# # Simulate
# times <- seq(0, 100, by = 1)
# out <- as_tibble(data.frame(mod$run(times)))

# epidf_hh <- format_output_hh(out, household_states)
# epidf_indiv <- format_output_indiv(out, household_states)

# fig_indiv <- epidf_indiv %>% 
#   pivot_longer(c("S_indiv", "I_indiv", "R_indiv")) %>% 
#   mutate(name=substr(name,1,1)) %>% 
#   # filter(name=="I") %>% 
#   ggplot(aes(x=t, y=value, col=name, lty=subpop)) + 
#     geom_line() + 
#     expand_limits(y=0)

# fig_rel_inf <- epidf_indiv %>% 
#   select(t, subpop, I_indiv) %>% 
#   pivot_wider(names_from="subpop", values_from="I_indiv") %>% 
#   mutate(rel_inf=A/C) %>% 
#   ggplot(aes(x=t, y=rel_inf)) + 
#     geom_line() 

# # For R0 = 2, we're looking for a final size of 0.7968




























