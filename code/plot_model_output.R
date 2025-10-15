# //////////////////////////////////////////////////////////////////////////////
# Import
# //////////////////////////////////////////////////////////////////////////////

library(tidyverse) 
library(tidycensus)
source('code/utils.R')
source('code/import_naws.R')
source('code/import_acs.R')

epidf_indiv_full <- read_csv("output/epidf_indiv_full.csv")

# Load US states geometry
states <- tigris::states(cb = TRUE, year = 2020) %>%
  filter(!STUSPS %in% c("AK", "HI", "PR", "GU", "VI", "MP", "AS")) %>%
  st_transform(crs = 5070) 

# epidf_indiv_full <- epidf_indiv_full %>% 
# 	left_join(st_drop_geometry(select(county_lookup, GEOID, REGION6)), by="GEOID")

# //////////////////////////////////////////////////////////////////////////////
# Crowding in the ACS vs NAWS? 
# //////////////////////////////////////////////////////////////////////////////

acs_crowding_plotter <- acs_data %>% 
	group_by(GEOID) %>% 
	summarise(prop_crowded = first(prop_crowded))

naws_crowding_plotter <- naws_data %>% 
	group_by(REGION6) %>% 
	summarise(prop_crowded=first(prop_crowded)) 

ggplot() + 
	geom_histogram(data=acs_crowding_plotter, aes(x=prop_crowded), bins=50) + 
	geom_vline(data=naws_crowding_plotter, aes(xintercept=prop_crowded)) + 
	theme_classic() 

naws_data %>% 
	group_by(REGION6) %>% 
	summarise(prop_crowded=first(prop_crowded)) 


acs_data %>% 
	filter(hhSize==1) %>% 
	ggplot(aes(x=prop_ag_workers, fill=factor(REGION6))) + 
		geom_histogram(position="identity") + 
		theme_classic() 

acs_data %>% 
	filter(hhSize==1) %>% 
	group_by(REGION6) %>% 
	mutate(propagwt = prop_ag_workers * population) %>% 
	summarise(propagwt = sum(propagwt), population=sum(population)) %>% 
	mutate(propag = propagwt / population)

# //////////////////////////////////////////////////////////////////////////////
# Plot infections by region
# //////////////////////////////////////////////////////////////////////////////

# temp <- epidf_indiv_full %>% 
# 	select(GEOID, t, subpop, I=I_indiv, REGION6) %>% 
# 	ggplot() +
# 	  geom_line(aes(x=t, y=I, col=subpop, group=interaction(GEOID, subpop)), 
# 	  	alpha=0.1) +
# 	  labs(x="Time", y="Proportion", col="Subpopulation") +
# 	  scale_color_manual(values=c(A="#e41a1c", C="#377eb8")) + 
# 	  theme_minimal() + 
# 	  facet_wrap(~factor(REGION6), nrow=2)

epidf_indiv_Iavg <- epidf_indiv_full %>% 
	group_by(t, subpop, REGION6) %>% 
	summarise(I_avg=mean(I_indiv))


temp <- ggplot() +
	  geom_line(data=epidf_indiv_full, aes(x=t, y=I_indiv, col=subpop, group=interaction(GEOID, subpop)), 
	  	alpha=0.1) +
	  geom_line(data=epidf_indiv_Iavg, aes(x=t, y=I_avg, col=subpop)) + 
	  labs(x="Time", y="Proportion", col="Subpopulation") +
	  scale_color_manual(values=c(A="#e41a1c", C="#377eb8")) + 
	  theme_minimal() + 
	  facet_wrap(~factor(REGION6), nrow=2)


# //////////////////////////////////////////////////////////////////////////////
# How do crowding levels compare? 
# //////////////////////////////////////////////////////////////////////////////


acs_data %>% 
	left_join(naws_data, by=c("REGION6","hhSize")) %>% 
	mutate(prop_crowded.y_adj = prop_crowded.y*crowded_factor) %>% 
	select(GEOID, REGION6, hhSize, prop.x, prop.y, pcx=prop_crowded.x, pcy=prop_crowded.y_adj, crowded_factor) %>% 
	select(GEOID, pcx, pcy) %>% 
	pivot_longer(-GEOID) %>% 
	ggplot(aes(x=value, fill=name)) + 
		geom_histogram(position="identity", col="black", alpha=0.5, bins=50) + 
		# geom_vline(data=naws_crowding_plotter, aes(xintercept=prop_crowded)) + 
		theme_classic() 


acs_data %>% 
	ggplot(aes(x=crowded_factor)) + 
		geom_histogram(bins=50) + 
		theme_classic() + 
		geom_vline(aes(xintercept=1))


# //////////////////////////////////////////////////////////////////////////////
# Where are the places with the biggest differences? 
# //////////////////////////////////////////////////////////////////////////////


fracdiff_df <- epidf_indiv_full %>% 
	group_by(GEOID, subpop) %>% 
	summarise(I_max=max(I_indiv)) %>% 
	pivot_wider(names_from="subpop", values_from="I_max") %>% 
	mutate(fracdiff = A/C) %>% 
	select(GEOID, fracdiff) %>% 
	left_join(county_lookup, by="GEOID") %>% 
	st_as_sf() %>% 
	st_transform(crs=5070)

fig_fracdiff_hist <- fracdiff_df %>% 
	ggplot(aes(x=fracdiff)) + 
		geom_histogram(bins=50) + 
		theme_classic() 

fig_fracdiff_map <- fracdiff_df %>% 
	# mutate(fracdiff=case_when(fracdiff<1.2~NA, TRUE~fracdiff)) %>% 
	ggplot() + 
	  geom_sf(aes(geometry=geometry, fill = fracdiff), color = NA) +  
	  geom_sf(data = states, fill = NA, color = "black", size = 0.3) + 
	  scale_fill_viridis_c(
	    name = "FracDiff",
	    na.value = "lightgrey"
	  ) +
	  labs(
	    title = "County-level Map of FracDiff",
	    subtitle = "Using sf geometries",
	    caption = "Source: Your Data"
	  ) +
	  theme_void() +
	  theme(
	    legend.position = "right",
	    plot.title = element_text(size = 16, face = "bold"),
	    plot.subtitle = element_text(size = 12)
	  )

# how is crowding distributed geographically at baseline? probably in big cities? 

fig_acscrowding_map <- acs_data %>% 
	left_join(select(county_lookup, GEOID), by="GEOID") %>% 
	st_as_sf() %>% 
	st_transform(crs=5070) %>% 
	filter(prop_crowded < 0.15) %>% 
	ggplot() + 
	  geom_sf(aes(geometry=geometry, fill = prop_crowded), color = NA) +  
	  geom_sf(data = states, fill = NA, color = "black", size = 0.3) + 
	  scale_fill_viridis_c(
	    name = "Proportion crowded",
	    na.value = "lightgrey"
	  ) +
	  labs(
	    title = "County-level Map of crowding (ACS)",
	    subtitle = "Using sf geometries",
	    caption = "Source: Your Data"
	  ) +
	  theme_void() +
	  theme(
	    legend.position = "right",
	    plot.title = element_text(size = 16, face = "bold"),
	    plot.subtitle = element_text(size = 12)
	  )


fig_acscrowdingfactor_map <- acs_data %>% 
	left_join(select(county_lookup, GEOID), by="GEOID") %>% 
	st_as_sf() %>% 
	st_transform(crs=5070) %>% 
	filter(crowded_factor < 5) %>% 
	ggplot() + 
	  geom_sf(aes(geometry=geometry, fill = crowded_factor), color = NA) +  
	  geom_sf(data = states, fill = NA, color = "black", size = 0.3) + 
	  scale_fill_viridis_c(
	    name = "Crowding factor",
	    na.value = "lightgrey"
	  ) +
	  labs(
	    title = "County-level Map of crowding (ACS)",
	    subtitle = "Using sf geometries",
	    caption = "Source: Your Data"
	  ) +
	  theme_void() +
	  theme(
	    legend.position = "right",
	    plot.title = element_text(size = 16, face = "bold"),
	    plot.subtitle = element_text(size = 12)
	  )






















epidf_indiv_overallmean <- epidf_indiv_full %>% 
	group_by(t, subpop) %>% 
	summarise(S_indiv=mean(S_indiv), I_indiv=mean(I_indiv), R_indiv=mean(R_indiv))

epidf_indiv_regionalmean <- epidf_indiv_full %>% 
	left_join(st_drop_geometry(select(county_lookup, GEOID, REGION6)), by=c("GEOID","REGION6")) %>% 
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
  # left_join(st_drop_geometry(select(county_lookup, GEOID, REGION6)), by="GEOID") %>% 
  mutate(name=substr(name,1,1)) %>% 
  filter(name=="I") %>% 
	ggplot(aes(x = t, y = value, lty = subpop, col=factor(REGION6), group = interaction(GEOID, name, subpop, REGION6))) +
	  geom_line(alpha=0.2) +
	  labs(x = "Time", y = "Proportion", col = "Subpopulation") +
	  theme_minimal() + 
	  scale_color_manual(values=c("white","white","white","white","blue","white")) 

geoid_subset <- GEOID_vec
# geoid_subset <- acs_data %>% 
# 	filter(prop_ag_workers > 0.05) %>% 
# 	filter(prop_rural > 0.5) %>% 
# 	pull(GEOID) %>% 
# 	unique()

fig_indiv_full_I_regionfacet <- epidf_indiv_full %>% 
  # filter(GEOID %in% geoid_subset) %>% 
  pivot_longer(c("S_indiv", "I_indiv", "R_indiv")) %>% 
  # left_join(st_drop_geometry(select(county_lookup, GEOID, REGION6)), by="GEOID") %>% 
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
    geom_line(alpha=0.1) 


fig_rel_inf_mean <- epidf_indiv_full %>% 
  select(t, subpop, I_indiv, GEOID) %>% 
  pivot_wider(names_from="subpop", values_from="I_indiv") %>% 
  mutate(rel_inf=A/C) %>% 
  group_by(t) %>% 
  summarise(rel_inf=mean(rel_inf)) %>% 
  ggplot(aes(x=t, y=rel_inf)) + 
    geom_line()

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


