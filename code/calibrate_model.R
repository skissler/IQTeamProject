library(tidyverse)
library(odin)
source('code/utils.R')
source('code/epimodels.R')

source('code/import_naws.R')
source('code/import_acs.R')

# Quick viz of relative household sizes: 
fig_hhcomp <- ggplot() + 
	geom_jitter(data=acs_hh, aes(x=HHSize, y=Prop), width=0.05, alpha=0.02) + 
	geom_point(data=naws_hh, aes(x=HHSize, y=Prop, group=REGION_NAME), col="blue") + 
	# geom_line(data=naws_hh, aes(x=HHSize, y=Prop, group=REGION_NAME), col="blue") + 
	expand_limits(y=0) +
	theme_classic() 

fig_agprop <- acs_data %>% 
	group_by(GEOID) %>% 
	summarise(prop_ag_workers=first(prop_ag_workers)) %>% 
	ggplot(aes(x=prop_ag_workers)) + 
		geom_histogram() + 
		theme_classic()

fig_agprop_pretty <- acs_data %>% 
  group_by(GEOID) %>% 
  summarise(prop_ag_workers = first(prop_ag_workers)) %>% 
  ggplot(aes(x = prop_ag_workers)) +
  geom_histogram(fill = "#2C77B8", color = "white", binwidth = 0.01, alpha = 0.8) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Distribution of Agricultural Workforce\nShare by County",
    x = "Percent Employed in Agriculture",
    y = "Number of Counties"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  )

# //////////////////////////////////////////////////////////////////////////////
# Implement adjusted crowding 
# //////////////////////////////////////////////////////////////////////////////

nat_data <- acs_data %>% 
	mutate(
		prop=prop*population, 
		prop_crowded=prop_crowded*population) %>%
	group_by(hhSize) %>% 
	summarise(prop=sum(prop), prop_crowded=sum(prop_crowded), population=sum(population)) %>% 
	mutate(prop=prop/population, prop_crowded=prop_crowded/population)

nat_data %>% 
	adjust_crowding(fold_diff=5)

# //////////////////////////////////////////////////////////////////////////////
# Try a national calibration
# //////////////////////////////////////////////////////////////////////////////

# Load household state definitions
household_states <- generate_household_state_table(n_min=1, n_max=7)
n_states <- nrow(household_states)

# Make another df of household states for crowded households
household_states_crowded <- household_states %>% 
  mutate(state_index=state_index + n_states) %>% 
  mutate(rec_index=case_when(rec_index>0 ~ rec_index + n_states, TRUE~0)) %>%
  mutate(inf_index=case_when(inf_index>0 ~ inf_index + n_states, TRUE~0))

# Combine into a single household states data frame: 
household_states <- bind_rows(
  mutate(household_states, crowded=0),
  mutate(household_states_crowded, crowded=1))
n_states <- nrow(household_states)

temp1 <- nat_data %>% 
	adjust_crowding() %>% 
	mutate(x=hhSize, y=0, z=0, crowded=0, frac=prop*(1-prop_crowded_adj)) %>% 
	select(x, y, z, hh_size=hhSize, crowded, frac)

temp2 <- nat_data %>% 
	adjust_crowding() %>% 
	mutate(x=hhSize, y=0, z=0, crowded=1, frac=prop*prop_crowded_adj) %>% 
	select(x, y, z, hh_size=hhSize, crowded, frac)

ic_joiner <- bind_rows(temp1, temp2)

init_nat_C <- household_states %>% 
	left_join(ic_joiner, by=c("x","y","z","hh_size","crowded")) %>% 
	arrange(state_index) %>% 
	replace_na(list(frac=0)) %>% 
	mutate(frac=case_when(x==2 & y==0 & z==0 & crowded==0 ~ frac-0.01, TRUE~frac)) %>% 
	mutate(frac=case_when(x==1 & y==1 & z==0 & crowded==0 ~ 0.01, TRUE~frac)) %>% 
	pull(frac)

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
  tau_A = (2/3)*(1/5), 
  tau_boost = 1/3,
  beta_C = (6/5)*(1/5), #(6/5)*(1/5),
  beta_A = (6/5)*(1/5), #(6/5)*(1/5),
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






