make_crowding_hists <- function(acs_data, naws_crowding){
fig_crowding_hists <- acs_data %>% 
	ggplot(aes(x=prop_crowded)) + 
		geom_histogram(aes(y=after_stat(density)), bins=50) + 
		geom_vline(data=mutate(naws_crowding, REGION6=REGION), aes(xintercept=prop_crowded)) + 
		facet_wrap(~REGION6) +
		theme_minimal() 
return(fig_crowding_hists)
}

fig_crowding_hists <- make_crowding_hists(acs_data, naws_crowding)
ggsave(fig_crowding_hists, file="figures/crowding_hists.pdf")