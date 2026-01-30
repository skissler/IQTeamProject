
## Methods

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

### Disease transmission model

We simulated respiratory disease spread using a deterministic household-structured susceptible-infectious-recovered (SIR) model based on a previously developed framework [x]. The model explicitly accounts for both within-household and between-household transmission, incorporating household size and crowding status as key structural parameters. 

#### Model structure

The model divides the population into compartments based on infection status (susceptible, infectious, or recovered), household size (n = 1, 2, …, 7+), crowding status (crowded or uncrowded), and occupational group (agricultural worker or general population). Within each household type, we tracked the number of households with i infectious individuals out of n total members. more here. 

#### Model parameterization

**Recovery rate:** We fixed the recovery rate at γ = 1/5 day^-1, corresponding to a mean infectious period of 5 days [x]. 

**Within-household transmission:** We parameterized within-household transmission using secondary attack rate (SAR) estimates from household transmission studies. The SAR is the probability that a susceptible household member becomes infected by an infectious index case within the household. [edited SAR calculation here, Claude is wrong]

**Between-household transmission:** With γ and τ fixed, we calibrated the between-household transmission rate β to achieve a specified basic reproduction number R0. We simulated outbreaks at the national level using the national population-weighted average household size distribution from the ACS data. We a given β value, we ran the model to equilibrium and compared the outbreak’s final size with the theoretical prediction from the implicit relationship  [<- check this]. We adjusted β using a bisection search algorithm until the simulated final size was within 0.001 of the theoretical value [implement this to be sure]. For the baseline analysis, we used R0 = 1.2, reflecting the estimated reproduction number for seasonal influenza and the effective reproduction number during many COVID-19 surges when behavioral mitigations were in place. This yielded β = xx and yielded a final attack rate of xx. In sensitivity analyses, we considered R0 values of 1.2, 1.5, 2.0, and 3.0. 

**Population mixing:** Agricultural workers and the general population likely exhibit some amount of assortative mixing, i.e., preferential contact within their own group. We modeled this using a mixing matrix where the force of infection for population k is 

xxxxxx

Where 

xxxxxx

The parameter  controls the degree of assortative mixing:  implies completely assortative mixing (no between-group contacts), while  implies mixing proportional to the groups’ population sizes. For the baseline analysis, we used , reflecting moderate assortativity where agricultural workers have preferential within-group contact but still interact with the general population. Due to the differences in population sizes, this yielded xx% of agricultural worker contacts with other agricultural workers (xx% with the general community), and xx% of general community contacts with the general community (xx% with agricultural workers). 

#### Model simulation

For the regional model, we initialized the population using household size distributions and crowding proportions from ACS (general population) and NAWS (agricultural workers). We introduced infection by setting 0.1% of individuals in each population as infectious. This was implemented by xx. 

We simulated outbreaks over 365 days using the `odin` package in R. For each simulation, we recorded the prevalence over time and calculated summary statistics including peak prevalence, time to peak, and final size for agricultural workers and the general population. 

For county-level simulations, we repeated this process for all counties in the contiguous United States to assess geographic variation in disease impacts between agricultural workers and the general community. 

### Assessing impact on agricultural productivity

To translate disease dynamics into agricultural productivity impacts, we estimated the number of workers unable to perform harvest labor each day due to symptomatic illness. We assumed that symptoms begin one day after infection and last for three days, during which workers cannot perform agricultural labor. To determine how outbreak timing affects agricultural productivity, we considered outbreaks where incidence in the general community peaked on each day of the year. 

We obtained weekly crop movement data for California strawberries, iceberg lettuce, and oranges from the USDA Agricultural Marketing Service. These movement metrics serve as a proxy for harvest timing and volume. We averaged movements across multiple years (2018-2024) to create representative seasonal patterns, then converted daily estimates by dividing weekly volumes equally across days. 

For each crop and outbreak timing scenario, we calculated the total seasonal productivity loss as 

xxxx

where xx is the average harvest volume for day t and xx is the amount of available labor accounting for illness. 

**[make table of parameters; possibly for supplement]** 