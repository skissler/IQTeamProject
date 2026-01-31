### Disease transmission model

We simulated respiratory disease spread using a deterministic household-structured susceptible-infectious-recovered (SIR) model based on a previously developed framework [x]. The model explicitly accounts for both within-household and between-household transmission, incorporating household size and crowding status as key structural parameters. 

#### Model structure

The model divides the population into compartments based on infection status (susceptible, infectious, or recovered), household size (n = 1, 2, …, 7+), crowding status (crowded or uncrowded), and occupational group (agricultural worker or general population). Within each household type, we tracked the number of households with i infectious individuals out of n total members. more here. 

#### Model parameterization

**Recovery rate:** We fixed the recovery rate at γ = 1/5 day^-1, corresponding to a mean infectious period of 5 days [x]. 

**Within-household transmission:** We parameterized within-household transmission using secondary attack rate (SAR) estimates from household transmission studies. The SAR is the probability that a susceptible household member becomes infected by an infectious index case within the household. [edited SAR calculation here, Claude is wrong]

**Between-household transmission:** With γ and τ fixed, we calibrated the between-household transmission rate β to achieve a specified basic reproduction number R0. We simulated outbreaks at the national level using the national population-weighted average household size distribution from the ACS data. We a given β value, we ran the model to equilibrium and compared the outbreak’s final size with the theoretical prediction from the implicit relationship  [<- check this]. We adjusted β using a bisection search algorithm until the simulated final size was within 0.0005 of the theoretical value. For the baseline analysis, we used R0 = 1.2, reflecting the estimated reproduction number for seasonal influenza and the effective reproduction number during many COVID-19 surges when behavioral mitigations were in place. This yielded β = xx and yielded a final attack rate of xx. In sensitivity analyses, we considered R0 values of 1.2, 1.5, 2.0, and 3.0. 

**Population mixing:** Agricultural workers and the general population likely exhibit some amount of assortative mixing, i.e., preferential contact within their own group. We modeled this using a mixing matrix where the force of infection for population k is 

xxxxxx

Where 

xxxxxx

The parameter  controls the degree of assortative mixing:  implies completely assortative mixing (no between-group contacts), while  implies mixing proportional to the groups’ population sizes. For the baseline analysis, we used , reflecting moderate assortativity where agricultural workers have preferential within-group contact but still interact with the general population. Due to the differences in population sizes, this yielded xx% of agricultural worker contacts with other agricultural workers (xx% with the general community), and xx% of general community contacts with the general community (xx% with agricultural workers). 

#### Model simulation

For the regional model, we initialized the population using household size distributions and crowding proportions from ACS (general population) and NAWS (agricultural workers). We introduced infection by setting 0.1% of individuals in each population as infectious. This was implemented by xx. 

We simulated outbreaks over 365 days using the `odin` package in R. For each simulation, we recorded the prevalence over time and calculated summary statistics including peak prevalence, time to peak, and final size for agricultural workers and the general population. 

For county-level simulations, we repeated this process for all counties in the contiguous United States to assess geographic variation in disease impacts between agricultural workers and the general community. Since information on household size and crowding are not available at the county level from the NAWS dataset, we imputed these data in various ways. For the baseline analysis, we assumed that the proportional difference in crowding rates among agriculturla workers at the county level were the same as for the general population; so that if the mean crowding in the general community for county x was xxx, we adjusted the crowding among agricultural workers to xxx time xxx = xxx. In sensitivity analyses, we (a) instead assumed that the difference was linear, so that we just added rather than multiplied, and (b) assumed that the household crowding rates in each county were the same as those reported in NAWS across all counties (i.e., no difference in crowding across counties for agricultural workers). [Note: we actually want the first sensitivity analysis to be the baseline, because I think what's currently the baseline and what's show in part (b) here should actually be extremes where what's currently (a) is a sort of middle ground.]
