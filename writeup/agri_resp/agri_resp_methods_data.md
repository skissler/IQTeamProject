### Data sources and processing

#### Characteristics of the population

We obtained county-level data on population size, household size distribution (proportion of households of size 1, 2, 3, 4, 5, 6, or 7+), proportion of crowded households (i.e., with more than one individual per room), and proportion of agricultural workers from the U.S. Census Bureau’s 2022 American Community Survey (ACS) 5-year estimates. We obtained regional data on household size distribution and proprtion of crowded households for agricultural workers specifically from the 2018-2022 National Agricultural Workers Survey (NAWS). The NAWS data are stratified geographically into six regions: East, Southeast, Midwest, Southwest, Northwest, and California. To enable region-level analysis, we aggregated the county-level ACS data into the corresponding NAWS regions using population-weighted averages. 

<!--- #### Characteristics of agricultural workers  ---> 

<!--- For agricultural workers specifically, we obtained regional household size distributions and crowding proportions from the 2018-2022 National Agricultural Workers Survey (NAWS), a nationally representative survey of U.S. crop workers conducted by the Department of Labor. The NAWS data are stratified geographically into six regions: East, Southeast, Midwest, Southwest, Northwest, and California. For each region, we calculated the weighted proportion of households of size 1, 2, 3, 4, 5, 6, and 7+ along with the weighted proportion of crowded households (>1 person per room). We applied the same crowding-by-household-size assignment procedure described above to agricultural worker households. ---> 

#### Crop harvest calendars and labor requirements

We obtained crop harvest calendar data for oranges, iceberg lettuce, and strawberries from the United States Department of Agriculture (USDA) Statistics Service and state agricultural extension services [x]. We restricted our analysis of crop impacts to California, which produces approximately 90% of U.S. strawberries, 75% of U.S. iceberg lettuce, and 80% of U.S. oranges. For each crop, we compiled monthly harvest intensity data reflecting the proportion of annual harvest occurring in each calendar month. We then combined these harvest calendars with the epidemic simulation outputs to estimate productivity losses as a function of outbreak timing, assuming that labor shortages during peak harvest periods result in proportional crop losses. 

We used crop movement data as a proxy for harvest timing. These roughly overlap with the reported harvest timings reported in various crop-specific reports. 

UCLA: "Navels are normally harvested from November to June." And: 

for strawberries: 
Table B. Percent Crop Harvested by
        April May Jun July Aug Sep Oct
Fresh % 5     12  25  26   18  12  2

"Lettuce is planted continuously from late December to mid-August along the Central Coast." "Cool season plantings may require up to 100 days to mature, but as the season warms, time to maturity decreases"