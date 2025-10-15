# //////////////////////////////////////////////////////////////////////////////
# Import
# //////////////////////////////////////////////////////////////////////////////

library(tidyverse)
library(scales)
source('code/utils.R')
source('code/epimodels.R')

source('code/import_naws.R')
source('code/import_acs.R')

data_regional <- bind_rows(
	mutate(select(naws_data, REGION6, hhSize, prop), SOURCE="NAWS"),
	mutate(select(acs_data_regional, REGION6, hhSize, prop), SOURCE="ACS") 
)

# write_csv(data_regional, "output/data_regional.csv")

source('code/make_household_pyramid.R')
source('code/make_crowding_hists.R')

