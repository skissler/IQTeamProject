### Disease transmission model

We simulated respiratory disease spread using a deterministic household-structured susceptible-infectious-recovered (SIR) model based on a previously developed framework [x]. The model explicitly accounts for both within-household and between-household transmission, incorporating household size and crowding status as key structural parameters. 

#### Model structure

The model divides the population into compartments based on infection status (susceptible, infectious, or recovered), household size (n = 1, 2, ..., 7+), crowding status (crowded or uncrowded), and occupational group (agricultural worker or general population). Within each household type, we tracked the number of households with i infectious individuals out of n total members. more here. 

The ACS reports household size distribution and crowding proportion separately rather than jointly. Our transmission model requires the fraction of households of each size that are crowded. To assign crowding levels by household size, we assumed the probability that a household is crowded increases linearly with household size, since households of size 1 by definition cannot be crowded. Specifically, we set the crowding probability to increase linearly from households of size 2 to households of size 7, constrained so that (1) the overall proportion of crowded households matches the ACS-reported proportion, and (2) households of size 7 are twice as likely to be crowded as households of size 2 (with sensitivity analyses using equal crowding probabilities across household sizes).

#### Model parameterization

**Recovery rate:** We fixed the recovery rate at $\gamma$ = 1/5 day^-1, corresponding to a mean infectious period of 5 days [x]. 

**Within-household transmission:** We parameterized within-household transmission using secondary attack rate (SAR) estimates from household transmission studies. The SAR is the probability that a susceptible household member becomes infected by an infectious index case within the household. In the household model, the secondary attack rate informs the within-household transmission rate. Specifically, the secondary attack rate is equal to the probability that an infection occurs prior to both recovery and infection from the outside; i.e., 

**Between-household transmission:** With $\gamma$ and $\tau$ fixed, we calibrated the between-household transmission rate β to achieve a specified basic reproduction number $R_0$. We simulated outbreaks at the national level using the national population-weighted average household size distribution from the ACS data. For a given $\beta$ value, we ran the model to equilibrium and compared the outbreak's final size with the theoretical prediction from the implicit relationship $R = 1 - \exp(-R_0 \cdot R)$, where $R$ is the final attack rate. We adjusted β using a bisection search algorithm until the simulated final size was within 0.0005 of the theoretical value. For the baseline analysis, we used $R_0$ = 1.5, reflecting a moderate pandemic influenza scenario and the effective reproduction number during many COVID-19 surges when behavioral mitigations were in place. This yielded a β scalar of 1.049 (i.e., $\beta$ = 1.049 × $\gamma$ = 0.21 day$^{-1}$) and a theoretical final attack rate of approximately 58%. In sensitivity analyses, we considered $R_0$ values of 1.2, 2.0, and 3.0. 

**Population mixing:** Agricultural workers and the general population likely exhibit some amount of assortative mixing, i.e., preferential contact within their own group. We modeled this using a mixing matrix where the force of infection for population $k$ from population $j$ is weighted by mixing coefficient $m_{kj}$. Defining $w_C$ and $w_A$ as the proportions of the total population belonging to the community and agricultural worker groups respectively, we computed the mixing matrix as:

$$m_{CC} = (1-\varepsilon) + \varepsilon w_C, \quad m_{CA} = \varepsilon w_A$$
$$m_{AC} = \varepsilon w_C, \quad m_{AA} = (1-\varepsilon) + \varepsilon w_A$$

The parameter $\varepsilon$ controls the degree of assortative mixing: $\varepsilon = 0$ implies completely assortative mixing (no between-group contacts), while $\varepsilon = 1$ implies random mixing proportional to the groups' population sizes. For the baseline analysis, we used $\varepsilon = 0.33$, reflecting moderate assortativity where agricultural workers have preferential within-group contact but still interact with the general population. 

#### Model simulation

For the regional model, we initialized the population using household size distributions and crowding proportions from ACS (general population) and NAWS (agricultural workers). We introduced infection by setting 0.1% of individuals in each population as infectious, implemented by distributing initial infectious individuals proportionally across household types according to each population's household size distribution. 

We simulated outbreaks over 365 days using the `odin` package in R. For each simulation, we recorded the prevalence over time and calculated summary statistics including peak prevalence, time to peak, and final size for agricultural workers and the general population. 

For county-level simulations, we repeated this process for all counties in the contiguous United States to assess geographic variation in disease impacts between agricultural workers and the general community. Since information on household size and crowding are not available at the county level from the NAWS dataset, we imputed these data using an additive adjustment approach. Specifically, for each county we computed the deviation between county-level ACS crowding rates and the regional mean ACS crowding rate, then added this deviation to the regional NAWS crowding rate for agricultural workers. This approach assumes that county-level variation in crowding among agricultural workers parallels that of the general population. In sensitivity analyses, we also considered (a) a multiplicative adjustment where county-level crowding rates were scaled by the ratio of county to regional mean ACS crowding, and (b) assuming uniform crowding rates across all counties within a region equal to the NAWS regional estimate.
