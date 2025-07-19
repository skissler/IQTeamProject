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

