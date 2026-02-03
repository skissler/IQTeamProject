### Data sources and processing

#### Characteristics of the general population

We obtained county-level data on population size, houehold size distribution, proportion of crowded households, and proportion of agricultural workers from the U.S. Census Bureau's 2022 American Community Survey (ACS) 5-year estimates. 
















We obtained county-level data on population size, household size distribution, proportion of crowded households, and proportion of agricultural workers from the U.S. Census Bureau’s 2022 American Community Survey (ACS) 5-year estimates. Following standard definitions, we classified households as crowded when they contained more than one individual per room (excluding bathrooms, kitchens, and hallways). Agricultural workers were defined as individuals employed in “farming, fishing, and forestry occupations” (ACS occupation codes). 

To enable regional-level analysis, we aggregated county-level data using population-weighted averages. Household sizes of size 7 or greater were combined into a single category (7+) in the ACS dataset; we treated them as households of size 7 in our analysis. Households of size 7+ represent 1.4% of all households in the ACS data. 

The ACS reports household size distribution and crowding proportion separately rather than jointly. Our transmission model requires the fraction of households of each size that are crowded. To assign crowding levels by household size, we assumed the probability that a household is crowded increases linearly with household size, since households of size 1 by definition cannot be crowded. Specifically, we set the crowding probability to increase linearly from households of size 2 to households of size 7, constrained so that (1) the overall proportion of crowded households matches the ACS-reported proportion, and (2) households of size 7 are twice as likely to be crowded as households of size 2 (with sensitivity analyses using equal crowding probabilities across household sizes). 

#### Characteristics of agricultural workers 

For agricultural workers specifically, we obtained regional household size distributions and crowding proportions from the 2018-2022 National Agricultural Workers Survey (NAWS), a nationally representative survey of U.S. crop workers conducted by the Department of Labor. The NAWS data are stratified geographically into six regions: East, Southeast, Midwest, Southwest, Northwest, and California. For each region, we calculated the weighted proportion of households of size 1, 2, 3, 4, 5, 6, and 7+ along with the weighted proportion of crowded households (>1 person per room). We applied the same crowding-by-household-size assignment procedure described above to agricultural worker households. 


#### Crop harvest calendars and labor requirements

We obtained crop harvest calendar data for oranges, iceberg lettuce, and strawberries from the United States Department of Agriculture (USDA) Statistics Service and state agricultural extension services [x]. We restricted our analysis of crop impacts to California, which produces approximately 90% of U.S. strawberries, 75% of U.S. iceberg lettuce, and 80% of U.S. oranges. For each crop, we compiled monthly harvest intensity data reflecting the proportion of annual harvest occurring in each calendar month. We then combined these harvest calendars with the epidemic simulation outputs to estimate productivity losses as a function of outbreak timing, assuming that labor shortages during peak harvest periods result in proportional crop losses. 
