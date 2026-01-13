# uses input from simulate_regional.R. 

# Steps: 
# - Compute the number of symptomatic people per day from the final R curve
# - Import the labor requirements over time for harvesting for each crop 
# - Compute labor reductions for epidemics starting at different times of year 

# Read in the default regional output: 
epidf_indiv_full <- read_csv("output/epidf_indiv_full_regional_1.csv")

# Calculate proportion of pop symptomatically infected by day: 
epidf_indiv_full 

# 