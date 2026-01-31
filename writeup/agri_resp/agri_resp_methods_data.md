### Data sources and processing

#### Characteristics of the general population

We obtained county-level data on population size, household size distribution, proportion of crowded households, and proportion of agricultural workers from the U.S. Census Bureau’s 2022 American Community Survey (ACS) 5-year estimates. Following standard definitions, we classified households as crowded when they contained more than one individual per room (excluding bathrooms, kitchens, and hallways). Agricultural workers were defined as individuals employed in “farming, fishing, and forestry occupations” (ACS occupation codes). 

To enable regional-level analysis, we aggregated county-level data using population-weighted averages. Household sizes of size 7 or greater were combined into a single category (7+) in the ACS dataset; we treated them as households of size 7 in our analysis. Households of size 7+ represent xx% of all households in the ACS data. 

The ACS reports household size distribution and crowding proportion separately rather than jointly. Our transmission model requires the fraction of households of each size that are crowded. To assign crowding levels by household size, we used a simple linear relationship: 

[Note here the key idea that we need the proportion of households that are crowded to equal the proportion reported in the ACS. To achieve that, we can allow the probability a household is crowded to increase linearly at rate m from households of size 2 to households of size 7, since households of size 1 by definition can't be crowded. So, we have some target household crowding fraction c* to match, and we want to do that such that pcrowded(7) = x x pcrowded(2), where x = 2 is our base case where size-7 households are twice as likely to be crowded as size-2 households; alternatively, we could consider x = 1, where size-7 households are equally likely to be crowded as size-2 households. We then figure out what m is to meet these constraints, i.e., matching a total proportion of  crowded households c* where size-7 households are x-times as likely to be crowded as size-2 households.] 

#### Characteristics of agricultural workers 

For agricultural workers specifically, we obtained regional household size distributions and crowding proportions from the 2018-2022 National Agricultural Workers Survey (NAWS), a nationally representative survey of U.S. crop workers conducted by the Department of Labor. The NAWS data are stratified geographically into six regions: xx. For each region, we calculated the weighted proportion of households of size 1, 2, 3, 4, 5 , 6, and 7+ along with the weighted proportion of crowded households (>1 person per room). We applied the same crowding-by-household-size assignment procedure described above to agricultural worker households. 


#### Crop harvest calendars and labor requirements

We obtained crop harvest calendar data for oranges, iceberg lettuce, and strawberries from the United States Department of Agriculture (USDA) Statistics Service and state agricultural extension services [x]. We restricted our analysis of crop impacts to the central valley of California, where the majority of U.S. production of all three crops occurs. [this part needs a bit more detail; see Claude but add some] 
