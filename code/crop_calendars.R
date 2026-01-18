library(tidyverse)
library(purrr)

# uses input from simulate_regional.R. 

# Steps: 
# - Compute the number of symptomatic people per day from the final R curve
# - Import the labor requirements over time for harvesting for each crop 
# - Compute labor reductions for epidemics starting at different times of year 

# Read in the time series of weekly movements by crop type: 
lettuce <- read_csv("data/movements_lettuce.csv")
strawberries <- read_csv("data/movements_strawberries.csv")
oranges <- read_csv("data/movements_oranges.csv")

movements <- bind_rows(lettuce, strawberries, oranges)

movements <- movements %>% 
	filter(grepl("California", origin)) %>% 
	group_by(begin_date, commodity) %>% 
	summarise(lbs=sum(`1_lb_units`)) %>% 
	mutate(begin_date=mdy(begin_date)) %>% 
	arrange(commodity, begin_date)

fig_movements <- movements %>% 
	filter(begin_date >= mdy("01-01-2018")) %>% 
	filter(begin_date < mdy("01-01-2025")) %>% 
	# filter(begin_date < mdy("01-01-2019")) %>% 
	ggplot(aes(x=begin_date, y=lbs, col=commodity)) + 
		geom_line(linewidth=0.8, alpha=0.8) + 
		theme_classic() + 
		scale_color_manual(values=c("Oranges"="orange", "Strawberries"="magenta", "Lettuce, Iceberg"="blue")) #+ 
		# geom_line(stat="smooth", method="loess", span=0.1, alpha=0.2)

# Let's look at averaging: 
avg_movements <- movements %>% 
	mutate(year=year(begin_date)) %>% 
	arrange(commodity, begin_date) %>% 
	group_by(commodity, year) %>% 
	mutate(week=1:n()) %>% 
	group_by(commodity, week) %>% 
	summarise(lbs=mean(lbs, na.rm=TRUE)) %>% 
	filter(week<=52)

fig_avg_movements <- avg_movements %>% 
	ggplot(aes(x=week, y=lbs, col=commodity)) + 
		geom_line(linewidth=0.8, alpha=0.8) + 
		theme_classic() + 
		scale_color_manual(values=c("Oranges"="orange", "Strawberries"="magenta", "Lettuce, Iceberg"="blue"))


# Read in the default regional output: 
epidf_indiv_full <- read_csv("output/epidf_indiv_full_regional_1.csv")
params <- pars_list[[1]]

# at any given time, how many people are symptomatic? 
symp_temp <- epidf_indiv_full %>% 
	filter(REGION6==6) %>% 
	group_by(subpop) %>% 
	arrange(t) %>% 
	mutate(Inew = lag(S_indiv) - S_indiv) %>% 
	replace_na(list(Inew=0)) %>% 
	mutate(symp_start=t+1, symp_end=t+3) %>% 
	select(subpop, REGION6, Inew, symp_start, symp_end) 

indices <- epidf_indiv_full %>% 
	filter(REGION6==6) %>% 
	select(t, subpop, REGION6)


epidf_REGION6 <- epidf_indiv_full %>% 
	filter(REGION6==6) %>% 
	full_join(symp_temp, by=c("subpop","REGION6"), relationship="many-to-many") %>% 
	mutate(tosum=case_when(t >= symp_start & t <= symp_end ~ Inew, TRUE~0)) %>% 
	group_by(t, subpop) %>% 
	summarise(
		S_indiv = first(S_indiv),
		I_indiv = first(I_indiv),
		R_indiv = first(R_indiv),
		REGION6=first(REGION6), 
		symp=sum(tosum)
		)

fig_symp <- epidf_REGION6 %>% 
	select(t, subpop, I_indiv, symp) %>% 
	pivot_longer(c("I_indiv","symp")) %>% 
	ggplot(aes(x=t, y=value, col=subpop, lty=name)) + 
		geom_line() + 
		theme_classic() 

# epidf_REGION6 %>% 
# 	group_by(subpop) %>% 
# 	mutate(ismax_symp = case_when(symp==max(symp)~1, TRUE~0)) %>% 
# 	mutate(ismax_I = case_when(I_indiv==max(I_indiv)~1, TRUE~0)) %>% 
# 	filter(ismax_symp==1 | ismax_I == 1)


epidf_indiv_full %>% 
	left_join(symp_temp, by=c("t"="symp_start","subpop","REGION6"))

# Calculate proportion of pop symptomatically infected by day: 
epidf_indiv_full %>% 
	filter(REGION6==6) %>% 
	ggplot(aes(x=t/7, y=I_indiv, col=subpop)) + 
		geom_line(linewidth=0.8, alpha=0.8) + 
		theme_classic() 

epidf_indiv_full %>% 
	filter(REGION6==6) %>% 
	ggplot(aes(x=t/7, y=R_indiv, col=subpop)) + 
		geom_line(linewidth=0.8, alpha=0.8) + 
		theme_classic() 


# ==============================================================================
# Overlay and assess impact 
# ==============================================================================

# Find when symptomatic cases peak in the general community, assuming an epidemic start time of 0: 

peaktime <- epidf_REGION6 %>% 
	ungroup() %>% 
	filter(subpop=="C") %>% 
	filter(symp==max(symp)) %>% 
	pull(t) 

wf_df <- epidf_REGION6 %>% 
	mutate(wf=1-symp) %>% 
	filter(subpop=="A") %>% 
	select(t, wf)

avg_movements_daily <- avg_movements %>% 
	split(.$commodity) %>% 
	map(~ split(., .$week)) %>% 
	map(~ map(., ~ cross_join(., tibble(day=1:7)))) %>% 
	map(~ map(., ~ mutate(., day=(week-1)*7+day))) %>% 
	map(~ bind_rows(.)) %>% 
	bind_rows() %>% 
	mutate(lbs = lbs/7)

fig_avg_movements_daily <- avg_movements_daily %>% 
	ggplot(aes(x=day, y=lbs, col=commodity)) + 
		geom_line() + 
		theme_classic() 

labor_shortage_df <- avg_movements_daily %>% 
	left_join(wf_df, by=c("day"="t")) %>% 
	mutate(lbs_adj = lbs*wf)

fig_labor_shortage <- labor_shortage_df %>% 
	select(commodity, day, lbs, lbs_adj) %>% 
	pivot_longer(c("lbs","lbs_adj")) %>% 
	ggplot(aes(x=day, y=value, col=commodity, lty=name)) + 
		geom_line() 

labor_shortage_df %>% 
	group_by(commodity) %>% 
	summarise(lbs_tot = sum(lbs), lbs_adj_tot=sum(lbs_adj)) %>% 
	mutate(pct_loss = (1 - lbs_adj_tot/lbs_tot)*100)


