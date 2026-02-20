### Data

#### Population characteristics

We obtained county-level data on overall population size, household size distribution (proportion of households of size 1, 2, 3, 4, 5, 6, or 7+), proportion of crowded households (i.e., with more than one individual per room), and proportion of agricultural workers from the U.S. Census Bureau’s 2022 American Community Survey (ACS) 5-year estimates [x](https://www.census.gov/programs-surveys/acs/). For agricultural workers specifically, we obtained regional data on household size distribution and proportion of crowded households from the 2018-2022 National Agricultural Workers Survey (NAWS) [x](https://www.dol.gov/agencies/eta/national-agricultural-workers-survey). The NAWS data are stratified geographically into six regions: East, Southeast, Midwest, Southwest, Northwest, and California. To enable region-level analysis, we aggregated the county-level ACS data into the corresponding NAWS regions using population-weighted averages. Full details on the data extraction are given in the **Supplementary Methods.** 

#### Crop harvest calendars and labor requirements

To approximate daily harvest volumes, we obtained data on specialty crop movements (point-to-point shipments) for strawberries, iceberg lettuce, and oranges from the United States Department of Agriculture's (USDA's) Agricultural Marketing Service [x](https://mymarketnews.ams.usda.gov/public_data). We extracted the total weekly weight of shipments originating in California for each of these crops between 1 Jan 2018 and 1 Jan 2025. In 2024, California produced approximately 90% of U.S. strawberries [x](https://www.nass.usda.gov/Publications/Todays_Reports/reports/ncit0525.pdf), 74% of U.S. iceberg lettuce [x](https://www.nass.usda.gov/Publications/Todays_Reports/reports/vegean25.pdf), and 78% of U.S. oranges [x](https://esmis.nal.usda.gov/sites/default/release-files/j9602060k/vx023d76b/w9507070x/cfrt0825.pdf). We averaged the weekly shipment volumes for each crop across the seven available years to mitigate the impact of inter-annual variation. Then, we interpolated daily shipment volumes by assuming equal shipment volumes across each day of the week. We normalized these shipment volumes by the total mean annual shipment volume, so that the daily values reflected the proportion of the total harvest normally collected on that day. We cross-referenced the resulting production curves with independent reports on each crop's production timing [x](https://coststudyfiles.ucdavis.edu/2024/04/04/2024Strawberry-FULL-FINAL-March2024.pdf) [x](https://coststudyfiles.ucdavis.edu/uploads/pub/2023/08/04/2023-iceberglettuce-full-final.pdf) [x](https://coststudyfiles.ucdavis.edu/uploads/pub/2021/08/12/2021orangessjvsouth.pdf) (**Supplementary Methods, Supplementary Figures S11-S12**). The economic value of strawberries, oranges, and head lettuce produced in California were obtained from USDA National Agricultural Statistics Service reports capturing crop values in 2024 [x](https://www.nass.usda.gov/Publications/Todays_Reports/reports/vegean25.pdf) [x](https://www.nass.usda.gov/Publications/Todays_Reports/reports/cfrt0825.pdf) [x](https://www.nass.usda.gov/Publications/Todays_Reports/reports/ncit0525.pdf)

<!-- 2024 California head lettuce value: $1,245,105,000 (https://www.nass.usda.gov/Publications/Todays_Reports/reports/vegean25.pdf)
2024 California orange value: $852,507,000 (https://www.nass.usda.gov/Publications/Todays_Reports/reports/cfrt0825.pdf)
2024 California strawberry value: $3,456,522,000 (https://www.nass.usda.gov/Publications/Todays_Reports/reports/ncit0525.pdf)
 -->

<!--- UCLA: "Navels are normally harvested from November to June." And: 

for strawberries: 
Table B. Percent Crop Harvested by
        April May Jun July Aug Sep Oct
Fresh % 5     12  25  26   18  12  2

"Lettuce is planted continuously from late December to mid-August along the Central Coast." "Cool season plantings may require up to 100 days to mature, but as the season warms, time to maturity decreases" ---> 
