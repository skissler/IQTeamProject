
<!--

{ cat writeup/agri_resp/agri_resp_title.md; printf '\n\n'; 
cat writeup/agri_resp/agri_resp_abstract.md; printf '\n\n'; 
cat writeup/agri_resp/agri_resp_intro.md; printf '\n\n'; 
cat writeup/agri_resp/agri_resp_methods.md; printf '\n\n'; 
cat writeup/agri_resp/agri_resp_methods_data.md; printf '\n\n'; 
cat writeup/agri_resp/agri_resp_methods_model.md; printf '\n\n'; 
cat writeup/agri_resp/agri_resp_methods_statistics.md; printf '\n\n'; 
cat writeup/agri_resp/agri_resp_results.md; printf '\n\n'; 
cat writeup/agri_resp/agri_resp_discussion.md; printf '\n\n'; 
cat writeup/agri_resp/agri_resp_supplement.md;
} > writeup/agri_resp/agri_resp_full_manuscript.md

-->

# Modeling the impact of respiratory illness outbreaks on the agricultural workforce and food production in the United States



## Abstract

**Background:** Respiratory disease outbreaks pose significant threats to critical infrastructure, including food production systems. Agricultural workers face elevated disease transmission risk due to crowded living conditions and unique occupational exposures, yet the impact of respiratory outbreaks on agricultural productivity remains poorly quantified. 

**Methods:** We developed a household-structured susceptible-infectious-recovered (SIR) transmission model to compare disease dynamics between agricultural workers and the general U.S. population. Using data from the American Community Survey and National Agricultural Workers Survey, we parameterized household size and crowding distributions across six regions. We simulated outbreaks with reproduction numbers ranging from 1.2 to 3.0 and secondary attack rates of 20% in uncrowded households and 40% in crowded households. We assessed productivity losses for three labor-intensive crops (oranges, lettuce, strawberries) with different harvest seasonalities. 

**Results:** Agricultural worker households exhibited substantially higher crowding rates (11-33% vs 2-8% in the general population), despite similar mean household sizes. At a baseline reproduction number of 1.5, disease prevalence peaked 0.5-2.0% higher among agricultural workers compared to the general population, and cumulative attack rates exceeded the general population by 3.5-11.5%. Under baseline assumptions, productivity losses during peak harvest periods reached 114% for strawberries, 90% for lettuce, and 88% for oranges. 

**Conclusions:** Household crowding creates disproportionate disease burden among agricultural workers, potentially leading to substantial harvesting-related productivity losses. These findings highlight the need for targeted outbreak preparedness and mitigation strategies in the agricultural sector to maintain food system resilience. 


## Introduction

Respiratory disease outbreaks can cause economic and infrastructural disruptions that cascade beyond their immediate public health impacts. The food system is particularly vulnerable to outbreaks [x]. The COVID-19 pandemic, for example, profoundly impacted food production, processing, and distribution [x, Jayson Lusk]. Agricultural labor is the critical foundation upon which the rest of the food system rests, yet agricultural workers may be especially vulnerable to respiratory disease. This vulnerability stems from many factors, including an increased rate of comorbidities [x], limited healthcare access [x], frequent migration [x], and household crowding [x]. 

While differences in health outcomes may be straightforward to account for -- the probability of severe illness given infection, for example, may be higher among agricultural workers, so we can just try to measure and account for that difference -- differences in transmission among agricultural workers vs. the general community are harder to anticipate and account for. These could cause substantially different epidemic trajectories among agricultural workers vs. the general population, making it difficult to use standard surveillance to understand what's happening in agricultural communities and to anticipate the timing and extent of the downstream effects. 

Efforts to do outreach to agricultural workers, both for protecting health and surveillance, are varied, including xxx and xxx. Still, we lack a clear understanding of how disease transmission dynamics differ between agricultural workers and the general population. More importantly, we lack predictive tools for anticipating how novel respiratory outbreaks might impact agricultural workers before they occur. Likewise, analytical frameworks for translating these labor impacts into commodity-specific production impacts remain underdeveloped. 

This study addresses these gaps by developing a disease transmission model specifically designed to anticipate the differential impacts of respiratory virus outbreaks on agricultural workers relative to the general U.S. population. Focusing on household size and crowding – two well-established predictors of respiratory disease transmission – we quantify the relative rate of infections and assess potential impacts on harvesting operations. We apply this framework to three economically important, labor-intensive crops with different harvest seasonalities: oranges, lettuce, and strawberries. Our analysis provides both a quantitative assessment of how outbreaks with varying characteristics might impact the agricultural sector and a generalizable framework for future assessments of disease impacts on agricultural production. 



## Methods




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


### Disease transmission model

We simulated respiratory disease spread using a deterministic household-structured susceptible-infectious-recovered (SIR) model based on a previously developed framework [x]. The model explicitly accounts for both within-household and between-household transmission, incorporating household size and crowding status as key structural parameters. 

#### Model structure

The model divides the population into compartments based on infection status (susceptible, infectious, or recovered), household size (n = 1, 2, ..., 7+), crowding status (crowded or uncrowded), and occupational group (agricultural worker or general population). Within each household type, we tracked the number of households with i infectious individuals out of n total members. more here. 

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


### Assessing impact on agricultural productivity

To translate disease dynamics into agricultural productivity impacts, we estimated the number of workers unable to perform harvest labor each day due to symptomatic illness. We assumed that symptoms begin one day after infection and last for three days, during which workers cannot perform agricultural labor. To determine how outbreak timing affects agricultural productivity, we considered outbreaks where incidence in the general community peaked on each day of the year. 

We obtained weekly crop movement data for California strawberries, iceberg lettuce, and oranges from the USDA Agricultural Marketing Service. These movement metrics serve as a proxy for harvest timing and volume. We averaged movements across multiple years (2018-2024) to create representative seasonal patterns, then converted daily estimates by dividing weekly volumes equally across days. 

For each crop and outbreak timing scenario, we calculated the total seasonal productivity loss as 

xxxx

where xx is the average harvest volume for day t and xx is the amount of available labor accounting for illness. 

**[make table of parameters; possibly for supplement]** 



## Results

### Household crowding leads to higher modeled disease prevalence among agricultural workers.

Agricultural worker households are substantially more crowded on average than the general U.S. population (Figure 1). While mean household size was similar between groups (2.7 for agricultural workers vs 2.5 for the general population; Figure 1), the proportion of crowded households ranged from 3 to 9 times higher among agricultural workers than the general population across the six regions. Crowding rates among agricultural workers ranged from 11% (Midwest) to 33% (California), compared to 2-8% in the general population.

Simulations of respiratory disease outbreaks at the regional level revealed consistently higher disease burden among agricultural workers compared to the general population (Figure 2). Under baseline assumptions ($R_0$ = 1.5, SAR = 20%/40% for uncrowded/crowded households, $\varepsilon$ = 0.33), peak prevalence among agricultural workers exceeded that of the general population by 0.5% to 2.0% across regions. Cumulative attack rates ranged from 3.5% to 11.5% higher among agricultural workers, with final attack rates of 60-72% among agricultural workers compared to 56-63% in the general population.

These differences were sensitive to the basic reproduction number. At $R_0$ = 1.2, peak prevalence among agricultural workers exceeded the general population by 0.2-1.0%, and attack rates differed by 4-15%. At higher transmissibility ($R_0$ = 2.0), peak prevalence differences widened to 0.6-2.5%, while attack rate differences narrowed to 2-6% as both populations approached high overall infection levels. At $R_0$ = 3.0, with near-complete infection of both populations (>93% attack rates), differences between groups were minimal (peak prevalence difference 0.5-2.3%, attack rate difference 0.4-1.7%).

Sensitivity to the secondary attack rate in crowded households showed proportional effects on disparities. Increasing SAR in crowded households from 30% to 50% increased the attack rate difference between agricultural workers and the general population by approximately 2-4 percentage points across regions, while reducing SAR to 30% decreased disparities by similar margins.

County-level simulations demonstrated geographic heterogeneity in these infection disparities. Counties with high agricultural employment and elevated crowding rates showed the largest disparities, with some counties experiencing agricultural worker attack rates exceeding their local general population by more than 15 percentage points. These results were somewhat sensitive to how household sizes and crowding rates were assigned to the agricultural worker population at the county level, since these data were only available for agricultural workers at the regional level; under the most conservative assumptions (uniform regional crowding rates), disparities were reduced but remained substantial.

### Respiratory disease outbreaks among agricultural workers can lead to substantial productivity losses.

The simulated outbreaks yielded substantial productivity losses for all three crops we considered, with the impact varying by outbreak timing relative to peak harvest periods. For strawberries, peak productivity losses were 114% with the worst outbreak timing being an epidemic peak on day 141 (approximately late May). For iceberg lettuce, maximum losses were 90% for outbreaks that peaked in late May (day 141). For oranges, peak losses were 88% for outbreaks peaking in late January (day 30).

The greater-than-100% loss for strawberries reflects the labor-intensive nature of strawberry harvest and the narrow harvest window: when an epidemic peak coincides exactly with peak harvest, symptomatic workers missing multiple consecutive harvest days during the brief peak season can result in cumulative losses exceeding the baseline harvest capacity.



## Discussion


## Acknowledgments

## Funding

## Author contributions

## Competing interests

## Data availability

## References

## Supplementary Information

### Supplementary Methods

#### Mathematical model formulation

The household-structured SIR model tracks the distribution of households across disease states. Let $H_k(x,y,z,c)$ denote the number of households in population $k$ (where $k \in \{C, A\}$ for community and agricultural populations) with $x$ susceptible, $y$ infected, and $z$ recovered members, and crowding status $c \in \{0,1\}$. The total household size is $n = x + y + z$.

The dynamics are governed by three types of transitions:

**Recovery transitions:** Infected individuals recover at rate $\gamma$, moving a household from state $(x,y,z,c)$ to state $(x,y-1,z+1,c)$:
$\text{Recovery rate} = \gamma \cdot y \cdot H_k(x,y,z,c)$

**Within-household transmission:** Susceptible individuals are infected by household members at rate $\tau_c = \tau_{\text{base}} + \tau_{\text{boost}} \cdot c$, moving a household from state $(x,y,z,c)$ to state $(x-1,y+1,z,c)$:
$\text{Within-household infection rate} = \tau_c \cdot x \cdot y \cdot H_k(x,y,z,c)$

**Between-household transmission:** Susceptible individuals are infected through community contacts at rate $\lambda_k$, determined by the mixing matrix and overall prevalence in each population:
$\text{Between-household infection rate} = \lambda_k \cdot x \cdot H_k(x,y,z,c)$

The force of infection for population $k$ is:
$\lambda_k = \beta_k \left[ m_{kk} I_k + m_{kj} I_j \right]$

where $I_k$ is the prevalence in population $k$:
$I_k = \frac{\sum_{x,y,z,c} y \cdot H_k(x,y,z,c)}{\sum_{x,y,z,c} n \cdot H_k(x,y,z,c)}$

The mixing matrix elements are:
$m_{kk} = (1-\epsilon) + \epsilon w_k$
$m_{kj} = \epsilon w_j$

where $w_k = \frac{N_k}{N_C + N_A}$ is the population fraction in group $k$.

The complete system of ordinary differential equations is:
$\frac{dH_k(x,y,z,c)}{dt} = \text{inflow} - \text{outflow}$

where inflow includes households transitioning to this state from $(x+1,y-1,z,c)$ (infection), $(x,y+1,z-1,c)$ (recovery), and outflow includes households leaving this state through infection, recovery, or death transitions.

We implemented this system using the `odin` package in R, which provides efficient numerical integration of large ODE systems. The state space includes all possible combinations of $(x,y,z,c)$ for household sizes from 1 to 7, with separate state variables for the community and agricultural populations.

#### Calculation of basic reproduction number

We calibrated the between-household transmission rate $\beta$ to achieve target $R_0$ values by running the model at the national level with aggregated ACS household data and systematically varying $\beta$ until the final attack rate matched theoretical predictions for the desired $R_0$. For an SIR model, the relationship between $R_0$ and final attack rate $R_\infty$ is given implicitly by:
$R_\infty = 1 - e^{-R_0 R_\infty}$

For example, $R_0 = 1.2$ corresponds to $R_\infty \approx 0.26$, and $R_0 = 2.0$ corresponds to $R_\infty \approx 0.80$. We verified that our calibrated $\beta$ values produced final attack rates consistent with these theoretical predictions.

#### Additional parameterization details

**Crowding assignment by household size:** To assign crowding probabilities by household size given only aggregate crowding data, we used a linear relationship:
$p_{\text{crowded}}(n) = \begin{cases} 0 & n = 1 \\ c \cdot (n-1) & n \geq 2 \end{cases}$

where the constant $c$ is chosen such that the weighted average across all household sizes equals the observed aggregate crowding proportion. With a crowding fold difference parameter $d = 2$, households of size 7 are twice as likely to be crowded as households of size 2.

**County-level NAWS adjustment:** For county-level simulations, we adjusted regional NAWS household distributions to match county-specific ACS data. For each county, we calculated adjustment factors for household size distribution and crowding proportion:
$f_{\text{size}}(n) = \frac{p_{\text{ACS}}(n)}{p_{\text{NAWS,region}}(n)}$
$f_{\text{crowd}} = \frac{p_{\text{crowded,ACS}}}{p_{\text{crowded,NAWS,region}}}$

We then applied these factors to the NAWS regional distributions and renormalized to ensure valid probability distributions, allowing us to capture county-level heterogeneity while maintaining the regional NAWS characterization of agricultural worker households.

### Supplementary Figures

**Figure S1.** Household size and crowding distributions by population and region

**Figure S2.** Sensitivity of attack rates to reproduction number (R₀). Attack rates for agricultural workers (red) and general population (blue) across regions for R₀ = 1.2, 1.5, 2.0, and 3.0.

**Figure S3.** Sensitivity to assortative mixing parameter (ε). Attack rate differentials (agricultural workers minus general population) for ε = 0, 0.33, 0.5, and 0.7.

**Figure S4.** Sensitivity to secondary attack rate in crowded households. Attack rates by population for crowded household SAR = 30%, 40%, 50%, and 60%.

**Figure S5.** Sensitivity to crowding fold difference. Impact of varying the relationship between household size and crowding probability (fold difference = 1, 2, 3).

**Figure S6.** Sensitivity to county-level NAWS adjustment approach. Comparison of attack rate differentials under three approaches: (A) Regional NAWS only, (B) Proportional adjustment (baseline), (C) ACS only (conservative).

**Figure S7.** Regional heterogeneity in baseline results. Attack rate differentials by region, with inset showing relationship to regional agricultural worker proportion and crowding levels.

**Figure S8.** Productivity impacts by outbreak timing. Seasonal productivity losses for strawberries, lettuce, and oranges as a function of outbreak peak timing (day of year).

### Supplementary Tables

**Table S1.** Regional household size and crowding data from ACS and NAWS

**Table S2.** Crop calendar and labor intensity data by region  

**Table S3.** Baseline and sensitivity analysis parameter values. Complete specification of all parameter combinations examined.

**Table S4.** Simulation results summary across all scenarios. Peak prevalence, time to peak, and final attack rate for agricultural workers and general population under all parameter combinations.