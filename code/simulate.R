# //////////////////////////////////////////////////////////////////////////////
# Import
# //////////////////////////////////////////////////////////////////////////////

library(tidyverse)
library(odin)
source('code/utils.R')
source('code/epimodels.R')

# source('code/import_naws.R')
# source('code/import_acs.R')

stateregion <- read_csv("data/stateregion.csv")
stateabbrev <- read_csv("data/stateabbrev.csv")

# //////////////////////////////////////////////////////////////////////////////
# Derive dataset for ag and non-ag households 
# //////////////////////////////////////////////////////////////////////////////

# Download county geometries (2023 by default)
counties_sf <- counties(cb = TRUE, year = 2022)

# Extract GEOID-to-state mapping
county_lookup <- counties_sf %>%
  select(GEOID, STATEFP, STUSPS, STATE_NAME)  %>% 
  inner_join(stateregion, by="STATE_NAME")

temp <- inner_join(county_lookup, acs_data, by="GEOID")

temp2 <- temp %>% 
	left_join(naws_data, by=c("REGION6","hhSize"))

temp %>% 
	adjust_crowding(fold_diff=2, indexcols=c("GEOID")) %>% 
	print(width=Inf)

ic_joiner_C <- temp %>% 
	st_drop_geometry() %>% 
	# filter(GEOID==first(GEOID)) %>% 
	filter(GEOID=="06097") %>% 
	make_ic_joiner(fold_diff=1)

ic_joiner_A <- naws_data %>% 
	# filter(REGION6==2) %>% 
	filter(REGION6==6) %>% 
	make_ic_joiner(fold_diff=1)

# Define key variables
max_hh_size <- 7
crowding_fold_diff <- 1

# Load household state definitions
household_states <- generate_household_state_table(n_min=1, n_max=max_hh_size, crowding=TRUE)
n_states <- nrow(household_states)

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
  eps = 0.8,
  pop_C = 476877,
  pop_A = 11559
)

# Simulate
times <- seq(0, 100, by = 1)
out <- as_tibble(data.frame(mod$run(times)))

epidf_hh <- format_output_hh(out, household_states)
epidf_indiv <- format_output_indiv(out, household_states)

fig_indiv <- epidf_indiv %>% 
  pivot_longer(c("S_indiv", "I_indiv", "R_indiv")) %>% 
  mutate(name=substr(name,1,1)) %>% 
  # filter(name=="I") %>% 
  ggplot(aes(x=t, y=value, col=name, lty=subpop)) + 
    geom_line() + 
    expand_limits(y=0)

fig_rel_inf <- epidf_indiv %>% 
  select(t, subpop, I_indiv) %>% 
  pivot_wider(names_from="subpop", values_from="I_indiv") %>% 
  mutate(rel_inf=A/C) %>% 
  ggplot(aes(x=t, y=rel_inf)) + 
    geom_line() 

# For R0 = 2, we're looking for a final size of 0.7968




























