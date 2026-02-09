
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
} > writeup/agri_resp/agri_resp_full.md

-->

# Modeling the impact of respiratory illness outbreaks on the agricultural workforce and food production in the United States



## Abstract

**Background:** Respiratory disease outbreaks pose significant threats to critical infrastructure, including food production systems. Agricultural workers face elevated disease transmission risk due to crowded living conditions and occupational exposures, yet the impact of respiratory outbreaks on agricultural productivity remains poorly quantified. 

**Methods:** We developed a household-structured susceptible-infectious-recovered (SIR) transmission model to compare disease dynamics between agricultural workers and the general U.S. population. Using data from the American Community Survey and National Agricultural Workers Survey, we parameterized household size and crowding distributions across six regions. We simulated outbreaks with reproduction numbers ranging from 1.2 to 3.0 across various assumptions on household secondary attack rate and assortative mixing. We assessed productivity losses for three labor-intensive crops (oranges, lettuce, strawberries) with different harvest seasonalities. 

**Results:** Agricultural worker households exhibited substantially higher crowding rates (11-33% vs. 2-8% in the general population), despite similar mean household sizes. At a baseline reproduction number of 1.5, disease prevalence peaked 0.5-2.0% higher among agricultural workers compared to the general population, and cumulative attack rates exceeded the general population by 3.5-11.5%. Under baseline assumptions, productivity losses during peak harvest periods reached 114% for strawberries, 90% for lettuce, and 88% for oranges. 

**Conclusions:** Household crowding may lead to disproportionate respiratory disease burden among agricultural workers, potentially generating substantial productivity losses. These findings highlight the need for targeted outbreak preparedness and mitigation strategies in the agricultural sector to maintain food system resilience. 


## Introduction

Respiratory disease outbreaks can cause economic and infrastructural disruptions that cascade beyond their immediate public health impacts. The food system is particularly vulnerable to outbreaks [x]. The COVID-19 pandemic, for example, profoundly impacted food production, processing, and distribution [x, Jayson Lusk]. Agricultural labor is the critical foundation upon which the rest of the food system rests, yet agricultural workers may be especially vulnerable to respiratory disease. This vulnerability stems from many factors, including an increased rate of comorbidities [x], limited healthcare access [x], frequent migration [x], and household crowding [x]. 

While differences in health outcomes may be straightforward to account for -- the probability of severe illness given infection, for example, may be higher among agricultural workers, so we can just try to measure and account for that difference -- differences in transmission among agricultural workers vs. the general community are harder to anticipate and account for. These could cause substantially different epidemic trajectories among agricultural workers vs. the general population, making it difficult to use standard surveillance to understand what's happening in agricultural communities and to anticipate the timing and extent of the downstream effects. 

Efforts to do outreach to agricultural workers, both for protecting health and surveillance, are varied, including xxx and xxx. Still, we lack a clear understanding of how disease transmission dynamics differ between agricultural workers and the general population. More importantly, we lack predictive tools for anticipating how novel respiratory outbreaks might impact agricultural workers before they occur. Likewise, analytical frameworks for translating these labor impacts into commodity-specific production impacts remain underdeveloped. 

This study addresses these gaps by developing a disease transmission model specifically designed to anticipate the differential impacts of respiratory virus outbreaks on agricultural workers relative to the general U.S. population. Focusing on household size and crowding – two well-established predictors of respiratory disease transmission – we quantify the relative rate of infections and assess potential impacts on harvesting operations. We apply this framework to three economically important, labor-intensive crops with different harvest seasonalities: oranges, lettuce, and strawberries. Our analysis provides both a quantitative assessment of how outbreaks with varying characteristics might impact the agricultural sector and a generalizable framework for future assessments of disease impacts on agricultural production. 



## Methods




### Data

#### Population characteristics

We obtained county-level data on overall population size, household size distribution (proportion of households of size 1, 2, 3, 4, 5, 6, or 7+), proportion of crowded households (i.e., with more than one individual per room), and proportion of agricultural workers from the U.S. Census Bureau’s 2022 American Community Survey (ACS) 5-year estimates. We obtained regional data on household size distribution and proprtion of crowded households for agricultural workers from the 2018-2022 National Agricultural Workers Survey (NAWS). The NAWS data are stratified geographically into six regions: East, Southeast, Midwest, Southwest, Northwest, and California. To enable region-level analysis, we aggregated the county-level ACS data into the corresponding NAWS regions using population-weighted averages. Full details on the data extraction are given in the **Supplementary Information.** 

#### Crop harvest calendars and labor requirements

To approximate daily harvest volumes, we obtained data on specialty crop movements (point-to-point shipments) for strawberries, iceberg lettuce, and oranges from the United States Department of Agriculture's (USDA's) Agricultural Marketing Services. We extracted the total weekly weight of shipments originating in California for each of these crops between 1 Jan 2018 and 1 Jan 2025. California produces approximately xx% of U.S. strawberries, xx% of U.S. iceberg lettuce, and xx% of U.S. oranges. We averaged the weekly shipment volumes for each crop across the seven available years to mitigate the impact of inter-anual variation. Then, we interpolated daily shipment volumes by assuming equal shipment volumes across each day of the week. We normalized these shipment volumes by the total mean annual shipment volume, so that the daily values reflected the proportion of the total harvest normally collected on that day. We cross-referenced the resulting production curves with independent reports on each crop's production timing (Supplementary Methods, Supplementary Figure XX). 

<!--- UCLA: "Navels are normally harvested from November to June." And: 

for strawberries: 
Table B. Percent Crop Harvested by
        April May Jun July Aug Sep Oct
Fresh % 5     12  25  26   18  12  2

"Lettuce is planted continuously from late December to mid-August along the Central Coast." "Cool season plantings may require up to 100 days to mature, but as the season warms, time to maturity decreases" ---> 


### Disease transmission model

#### Model structure

We simulated respiratory disease transmission using a deterministic household-structured susceptible-infectious-recovered (SIR) model based on a previously developed framework [x]. We split the population at the household level into "agricultural workers" and "general community", assuming the proportion of households belonging to agricultural workers was equal to the proportion of the working population involved in agricultural work according to the ACS data. 

The model tracks the number of infections over time in the population, explicitly accounting for transmission within and between households of various sizes. The transmission model has three main epidemiological parameters: the between-household transmission rate ($\beta$), the within-household transmission rate ($\tau$), and the recovery rate ($\gamma$). We allowed the within-household transmission rate $\tau$ to differ for uncrowded vs. crowded households. 

We assumed that mixing among agricultural workers (A) and the general community (C) was assortative, governed by parameter $\epsilon$. We modeled this using the mixing matrix 

$$ M = 
\begin{pmatrix} m_{CC} & m_{CA} \\\ m_{AC} & m_{AA} \end{pmatrix} = 
\begin{pmatrix} (1-\epsilon) + \epsilon w_C & \epsilon w_A \\\ \epsilon w_C & (1-\epsilon) + \epsilon w_A \end{pmatrix}
$$

Here, $w_C$ is the fraction of the region's population made up by the general community and $w_A$ is the fraction of the population made up by agricultural workers. This matrix modulates the between-household force of infection $\lambda$ experienced by each population such that 

$$\lambda_C = \beta (m_{CC} I_C + m_{CA} I_A)$$ 
$$\lambda_A = \beta (m_{AC} I_C + m_{AA} I_A)$$

where $\lambda_i$ is the between-household force of infection for members of sub-population $i$, $\beta$ is the between-household transmission constant, and $I_i$ is the proportion of infectious individuals in sub-population $i$; thus, $\epsilon = 0$ implies completely assortative mixing and $\epsilon = 1$ implies mixing proportional to each sub-population's size. 

Besides the impact of household size, household crowding, and assortative mixing, we did not assume any additional differences in transmission rates between the two sub-populations. For full details on the model structure, see the **Supplementary Methods.** 

#### Model parameterization

Following previous methods [x], we began by fixing the recovery rate $\gamma = 1/5$, which corresponds to a mean infectious period of 5 days. Then, given $\gamma$ and the household secondary attack rate (SAR), we derived $\tau$ (**Supplementary Methods**). We set the SAR for uncrowded households at 20%, following estimates for influenza [x]. For crowded households, we set the baseline SAR at 40% [x]. In sensitivity analyses, we considered crowded-household SARs of 0.3, 0.5, and 0.6. Last, given values for $\gamma$ and $\tau$, we numerically identified the value of $\beta$ that would achieve a desired basic reproduction number ($\mathcal{R_0}$) when simulating outbreaks at the national level. Specifically, for a candidate $\beta$ value, we ran the model with a single sub-population to equilibrium; then, we compared the outbreak's final size with the theoretical prediction from the implicit relationship $R(\infty) = 1 - \exp(-\mathcal{R_0} \cdot R(\infty))$, where $R(\infty)$ is the final size of the outbreak. [x] We adjusted $\beta$ using a bisection search algorithm until the simulated final size was within 0.0005 of the theoretical value. For the baseline analysis, we used $\mathcal{R_0}$ = 1.5, reflecting a moderate pandemic influenza scenario and the effective reproduction number during many COVID-19 surges when behavioral mitigations were in place. In sensitivity analyses, we considered $\mathcal{R_0}$ values of 1.2, 2.0, and 3.0. Baseline and sensitivity parameter values are listed in **Supplementary Table XX**. 

For the baseline assortativity, we used $\varepsilon = 0.33$, reflecting moderate assortativity where agricultural workers have preferential within-group contact but still interact with the general population. At this baseline, agricultural workers have approximately 67% of their between-household contacts within their own group and 33% with the general community (range across regions: 67.2%–67.7% within-group). The general community, being much larger, has nearly all contacts (>99%) within their own group. In sensitivity analyses, we considered $\varepsilon \in \{0, 0.5, 0.7\}$, corresponding to agricultural workers having 100%, 50%, and 31% of contacts within their own group, respectively.

We assumed that infections were symptomatic with probability $p_{symp}$ = 0.5 at baseline, and we considered $p_{symp} \in \{0.25, 0.75, 1\}$ in sensitivity analyses. We assumed symptoms began one day after infection and lasted for three days. 

The transmission model requires knowing the fraction of households of each size are crowded, but the ACS and NAWS data report household size distribution and crowding proportion separately. To assign crowding levels by household size, we assumed that the crowding probability increased linearly from households of size 2 to households of size 7+ (since households of size 1 by definition cannot be crowded). We obeyed the constraints that (1) the overall proportion of crowded households must match the ACS- or NAWS-reported proportion, and (2) households of size 7+ are $p$ times as likely to be crowded as households of size 2 (**Supplementary Methods**). We used a baseline of $p = 2$ (i.e. households of size 7+ are twice as likely to be crowded as households of size 2). In sensitivity analyses, we considered $p = 1$ and $p = 3$. 



#### Model implementation

We implemented the transmission model in `R` (version 4.5.0) using `odin` (version 1.2.7). We initialized outbreaks by setting 0.1% of individuals in each sub-population as infectious. We distributed the initial infectious individuals proportionally across household types, equivalent to uniform-randomly choosing the initial infected individuals from each sub-population. We simulated outbreaks over 365 days. 

In addition to the main regional simulations, we also generated county-level simulations to explore differences in transmission between agricultural workers and the general population at a finer geographic scale. Since the NAWS dataset does not report at the county level, we imputed county-level household size distributions and crowding proportions for agricultural workers using three different methods: an "additive" method, where county-level NAWS values were imputed by adding the difference between the county-level and regional ACS values to the regional NAWS value; a "multiplicative" method, where regional NAWS values were scaled by the ratio of county-level to regional ACS values; and a "null" method, where regional NAWS values were used without adjustment. The "null" method yielded no variation in county-level agricultural household characteristics within a region; the "additive" method yielded an intermediate amount of variation; and the "multiplicative" method yielded a high amount of variation (**Supplementary Figure xx**). We treated the "additive" method as a baseline and considered the "null" and "multiplicative" methods in sensitivity analyses. Due to the high uncertainty in these imputation methods, we emphasize that these county-level results have a low level of confidence and are intended to provide a rough estimate of within-region variation around the regional mean. 



### Outcomes and measurements

We compared the difference in household size distribution (mean household size, proportion of households with 4 or more people) and the difference in household crowding rates between agricultural workers and the general community at the region level. For the outbreak simulations, we measured differences in peak prevalence, time to peak, final size, and maximum incidence deviation between agricultural workers and the general community. 

To translate disease dynamics into agricultural productivity impacts, we estimated the number of workers unable to perform harvest labor each day due to symptomatic illness. We assumed that symptomatic individuals could not perform agricultural labor. This allowed us to calculate a daily "workforce strength", consisting of the fraction of agricultural workers still healthy. We multiplied this workforce strength by the daily harvest fraction for each crop to obtain an outbreak-adjusted harvest volume and summed this adjusted volume over the full year to measure the agricultural impact of the outbreak. We did this for outbreaks peaking on each day of the year to assess the impact of outbreak timing on agricultural productivity for each crop. 


## Results

### Household crowding leads to higher modeled disease prevalence among agricultural workers.

Agricultural worker households are substantially more crowded on average than the general U.S. population (Figure 1). While mean household size was similar between groups (2.7 for agricultural workers vs 2.5 for the general population; Figure 1), the proportion of crowded households ranged from 3 to 9 times higher among agricultural workers than the general population across the six regions. Crowding rates among agricultural workers ranged from 11% (Midwest) to 33% (California), compared to 2-8% in the general population.

Simulations of respiratory disease outbreaks at the regional level revealed consistently higher disease burden among agricultural workers compared to the general population (Figure 2). Under baseline assumptions ($R_0$ = 1.5, SAR = 20%/40% for uncrowded/crowded households, $\varepsilon$ = 0.33), peak prevalence among agricultural workers exceeded that of the general population by 0.5% to 2.0% across regions. Cumulative attack rates ranged from 3.5% to 11.5% higher among agricultural workers, with final attack rates of 60-72% among agricultural workers compared to 56-63% in the general population.

These differences were sensitive to the basic reproduction number. At $R_0$ = 1.2, peak prevalence among agricultural workers exceeded the general population by 0.2-1.0%, and attack rates differed by 4-15%. At higher transmissibility ($R_0$ = 2.0), peak prevalence differences widened to 0.6-2.5%, while attack rate differences narrowed to 2-6% as both populations approached high overall infection levels. At $R_0$ = 3.0, with near-complete infection of both populations (>93% attack rates), differences between groups were minimal (peak prevalence difference 0.5-2.3%, attack rate difference 0.4-1.7%).

Sensitivity to the secondary attack rate in crowded households showed proportional effects on disparities. Increasing SAR in crowded households from 30% to 50% increased the attack rate difference between agricultural workers and the general population by approximately 2-4 percentage points across regions, while reducing SAR to 30% decreased disparities by similar margins.

County-level simulations demonstrated geographic heterogeneity in these infection disparities. Counties with high agricultural employment and elevated crowding rates showed the largest disparities, with some counties experiencing agricultural worker attack rates exceeding their local general population by more than 15 percentage points. These results were sensitive to how household sizes and crowding rates were assigned to the agricultural worker population at the county level, since these data were only available for agricultural workers at the regional level; under the most conservative assumptions (uniform regional crowding rates), disparities were reduced but remained substantial.

### Respiratory disease outbreaks among agricultural workers can lead to substantial productivity losses.

The simulated outbreaks yielded substantial productivity losses for all three crops we considered, with the impact varying by outbreak timing relative to peak harvest periods. For strawberries, peak productivity losses were 1.14% with the worst outbreak timing being an epidemic peak on day 141 (approximately late May). For iceberg lettuce, maximum losses were 0.90% for outbreaks that peaked in late May (day 141). For oranges, peak losses were 0.88% for outbreaks peaking in late January (day 30). These translate into losses of roughly xxx, xxx, and xxx USD. 




## Discussion


Using crop movements as a proxy for harvests isn't perfect, and may be worse for crops that can be stored for longer (e.g. oranges). 

We assess region-level impacts; farm-specific impacts may differ substantially (for a given farm, harvests may be more concentrated than reflected here, which creates an all-or-nothing effect on the impact of an outbreak) 




## Acknowledgments

## Funding

## Author contributions

## Competing interests

## Data availability

## References

## Supplementary Information

### Supplementary Methods

#### Data

Agricultural workers were defined as individuals employed in "farming, fishing, and forestry occupations" (ACS occupation codes C24030_004 [males] and C24030_031 [females]) as a proportion of total employed individuals (C24030_001).

County-level household size distributions were obtained from ACS table B11016 (Household Type by Household Size), which reports counts of family and non-family households by size. We combined family and non-family counts for each household size (1 through 7+). County-level household crowding was obtained from ACS table B25014 (Tenure by Occupants per Room), where crowded households were defined as those with more than 1.00 occupants per room, summing across owner- and renter-occupied units and all crowding levels (1.01–1.50, 1.51–2.00, and >2.00 persons per room). Total population was obtained from ACS table B01003.

To enable region-level analysis, we aggregated the county-level ACS data into the corresponding NAWS regions using population-weighted averages. For each variable (household size proportions, crowding proportions, and proportion of agricultural workers), we multiplied each county's value by its population, summed within each region, then divided by the total regional population. This ensures that larger counties contribute proportionally more to the regional estimate.

Household sizes in the NAWS data were derived from the HHFAMGRD variable (number of relatives on the household grid). Households of size 7 or greater were grouped into a single "7+" category for consistency with the ACS data. Crowding status was derived from the CROWDED1 variable. Both household size and crowding data were weighted using the NAWS survey weights (PWTYCRD) and summarized by NAWS region.

To assign crowding probabilities by household size, we used a linear relationship where larger households are progressively more likely to be crowded. For a household of size $n$, the crowding multiplier is:

$$w(n) = \begin{cases} 0 & n = 1 \\\ 1 + (d - 1) \cdot \frac{n - 2}{5} & n \geq 2 \end{cases}$$

where $d$ is the crowding fold difference parameter (the ratio of crowding probability for size-7 households to size-2 households). We treated households of size 7+ as $n = 7$. The crowding probability for each household size within a given geographic area is then:

$$p_{\text{crowded}}(n) = c \cdot w(n)$$

where the constant $\eta$ is chosen so that the weighted average across all household sizes equals the observed aggregate crowding proportion:

$$\eta = \frac{P_{\text{crowded}}}{\sum_n p(n) \cdot w(n)}$$

Here $P_{\text{crowded}}$ is the overall proportion of crowded households and $p(n)$ is the proportion of households of size $n$. With the baseline $d = 2$, households of size 7+ are twice as likely to be crowded as households of size 2, with a linear gradient in between; and the constant $\eta$ ensures that the total fraction of crowded households in the region (
$\sum_n p_{\text{crowded}}(n)$
) matches the proportion of crowded households from the ACS/NAWS data ($P_{\text{crowded}}$). 

The NAWS dataset reports household characteristics for agricultural workers at the regional level only, while the ACS provides county-level data for the general population. While our main analysis was at the regional level, we also performed a county-level analysis to assess geographic variation in outbreak disparities between agriculural workers and the general community. To generate county-level population estimates for agricultural workers, we used county-level ACS variation to adjust the regional NAWS values. The underlying assumption is that county-level variation among agricultural workers follows a similar pattern to county-level variation in the general population; i.e., if a county's general population has larger households than the regional average, agricultural workers in that county likely also have larger households than the regional average for agricultural workers.

For each county $i$ in region $r$, we first computed the population-weighted regional mean of the county-level ACS values:

$$\bar{p}_{\text{ACS},r}(n) = \frac{\sum_{i \in r} p_{\text{ACS},i}(n) \cdot N_i}{\sum_{i \in r} N_i}$$

$$\bar{q}_{\text{ACS},r} = \frac{\sum_{i \in r} q_{\text{ACS},i} \cdot N_i}{\sum_{i \in r} N_i}$$

where $p_{\text{ACS},i}(n)$ is the proportion of households with size $n$ in county $i$ according to ACS data, $q_{\text{ACS},i}$ is the proportion of crowded households in county $i$, and $N_i$ is the population of county $i$.

We then imputed county-level NAWS values using one of three methods (**Figure XX**):

*Additive method.* We shifted regional NAWS values by the difference between county-level and regional mean ACS values:

$$\tilde{p}_{\text{NAWS},i}(n) \propto \max\left(0, \; p_{\text{NAWS},r}(n) + \left[ p_{\text{ACS},i}(n) - \bar{p}_{\text{ACS},r}(n) \right] \right)$$

$$\tilde{q}_{\text{NAWS},i} = q_{\text{NAWS},r} + \left[ q_{\text{ACS},i} - \bar{q}_{\text{ACS},r} \right]$$

Household size proportions were clamped to be non-negative before renormalization, and the crowding proportion was clamped to $[0, 1]$.

*Multiplicative method.* We scaled regional NAWS values by the ratio of county-level to regional mean ACS values:

$$\tilde{p}_{\text{NAWS},i}(n) \propto p_{\text{NAWS},r}(n) \times \frac{p_{\text{ACS},i}(n)}{\bar{p}_{\text{ACS},r}(n)}$$

$$\tilde{q}_{\text{NAWS},i} = q_{\text{NAWS},r} \times \frac{q_{\text{ACS},i}}{\bar{q}_{\text{ACS},r}}$$

The household size distribution was renormalized to sum to 1, and the crowding proportion was clamped to the interval $[0, 1]$.

*Null method.* We used regional NAWS values directly without adjustment:

$$\tilde{p}_{\text{NAWS},i}(n) = p_{\text{NAWS},r}(n)$$

$$\tilde{q}_{\text{NAWS},i} = q_{\text{NAWS},r}$$

This method assumes no county-level variation in agricultural worker household characteristics within a region.

**Crop movement data.** **TO FILL IN: Cross-referencing of crop movement data with UCDavis information, with figure.** 

#### Mathematical model structure

The household-structured SIR model (**Figure XX**) tracks the distribution of households across disease states. Let $H_k(x,y,z,c)$ denote the number of households in population $k$ (where $k \in \{C, A\}$ for community and agricultural populations) with $x$ susceptible, $y$ infected, and $z$ recovered members, and crowding status $c \in \{0,1\}$. The total household size is $n = x + y + z$.

The dynamics are governed by three types of transitions:

- **Recovery transitions:** Infected individuals recover at rate $\gamma$, moving a household from state $(x,y,z,c)$ to state $(x,y-1,z+1,c)$:
$\text{Recovery rate} = \gamma \cdot y \cdot H_k(x,y,z,c)$

- **Within-household transmission:** Susceptible individuals are infected by household members at rate $\tau_c = \tau_{\text{base}} + \tau_{\text{boost}} \cdot c$, moving a household from state $(x,y,z,c)$ to state $(x-1,y+1,z,c)$:
$\text{Within-household infection rate} = \tau_c \cdot x \cdot y \cdot H_k(x,y,z,c)$

- **Between-household transmission:** Susceptible individuals are infected through community contacts at rate $\lambda_k$, determined by the mixing matrix and overall prevalence in each population:
$\text{Between-household infection rate} = \lambda_k \cdot x \cdot H_k(x,y,z,c)$

The force of infection for population $k$ is:
$\lambda_k = \beta \left[ m_{kk} I_k + m_{kj} I_j \right]$

where $I_k$ is the prevalence in population $k$:
$I_k = \frac{\sum_{x,y,z,c} y \cdot H_k(x,y,z,c)}{\sum_{x,y,z,c} n \cdot H_k(x,y,z,c)}$

The mixing matrix elements are:
$m_{kk} = (1-\epsilon) + \epsilon w_k$
$m_{kj} = \epsilon w_j$

where $w_k = \frac{N_k}{N_C + N_A}$ is the population fraction in group $k$.

The complete system of ordinary differential equations is:
$\frac{dH_k(x,y,z,c)}{dt} = \text{inflow} - \text{outflow}$

where inflow includes households transitioning into this state via infection (from state 
$(x+1,y-1,z,c)$
) or recovery (from state 
$(x,y+1,z-1,c)$
), and outflow includes households leaving this state through infection or recovery. 

#### Mathematical model parameterization

We derived the within-household transmission rate, $\tau$, using the recovery rate $\gamma$ and the secondary attack rate (SAR). The SAR is determined by the competing rates of within-household infection ($\tau$) and recovery ($\gamma$). The probability that the susceptible individual is infected before the infectious index case recovers is:

$$\text{SAR} = \frac{\tau}{\tau + \gamma}$$

Solving for $\tau$:

$$\tau = \frac{\text{SAR} \cdot \gamma}{1 - \text{SAR}}$$

For the baseline uncrowded SAR of 20% and $\gamma = 1/5$: 

$$\tau = \frac{(0.2)(0.2)}{1 - 0.2} = 0.05$.

For crowded households, we computed $\tau_{\text{crowded}}$ using the same formula with the crowded SAR, then defined $\tau_{\text{boost}} = \tau_{\text{crowded}} - \tau$. For the baseline crowded SAR of 40%: $\tau_{\text{crowded}} = 0.40 \times 0.2 / 0.60 \approx 0.133$ and thus $\tau_{\text{boost}} \approx 0.083$. 

Next, we calibrated the between-household transmission rate $\beta$ to achieve target $R_0$ values by running the model at the national level with aggregated ACS household data and systematically varying $\beta$ until the final attack rate matched theoretical predictions for the desired $R_0$. For an SIR model, the relationship between $R_0$ and final attack rate $R_\infty$ is given implicitly by:
$R_\infty = 1 - e^{-R_0 R_\infty}$

For example, $R_0 = 1.2$ corresponds to $R_\infty \approx 0.31$, and $R_0 = 2.0$ corresponds to $R_\infty \approx 0.80$. We used a bisection search algorithm to find $\beta$, converging when the simulated final attack rate was within 0.0005 of the theoretical value. Calibration was performed using a single-population simulation at the national level, with national household distributions computed as population-weighted averages of all county-level ACS data. The calibrated $\beta$ values were then used in the full two-population regional simulations. We assumed no difference in $\beta$ between agricultural workers and the general community, so that all of the transmission differences in the model would come from differences in household size and crowding. 

We initialized outbreaks by setting 0.1% of individuals in each sub-population as infectious. The initial infectious individuals were distributed proportionally across household types, weighted by household size: for each household type with $n$ members, a fraction $\text{initprev} \times n$ of those households were moved from the fully susceptible state $(x = n, y = 0, z = 0)$ to the state $(x = n-1, y = 1, z = 0)$. This is equivalent to uniformly randomly selecting 0.1% of individuals to be initially infected, then distributing them across household types according to the population distribution.

#### Symptomatic infection and workforce impact

To translate epidemic dynamics into agricultural workforce availability, we computed the proportion of the agricultural workforce experiencing symptoms on each day. We first calculated the number of new daily infections for each day as $i_t = S(t-1) - S(t)$, where $S(t)$ is the proportion susceptible at time $t$. We assumed symptoms began one day after infection onset and lasted for three days, so that individuals infected on day $t$ were symptomatic on days $t+1$, $t+2$, and $t+3$. The proportion of symptomatic individuals on day $t$ was then:

$$\text{symp}_t_ = p_{\text{symp}} \sum_{d=1}^{3} i_{t-d}$$

where $p_{\text{symp}}$ is the probability that an infection is symptomatic. Daily workforce strength was defined as $\text{wf}(t) = 1 - \text{symp}(t)$. 

To assess the impact of epidemic timing on crop production, we shifted the epidemic curve so that the community symptomatic peak aligned with each day of the calendar year. The outbreak-adjusted harvest volume for each crop on each day was $V_{\text{adj}}(t) = V(t) \times \text{wf}(t)$, and total annual production loss was $(1 - \sum_t V_{\text{adj}}(t) / \sum_t V(t)) \times 100\%$.

### Supplementary Figures

**Figure S1.** Household size and crowding distributions by population and region

**Figure S2.** Sensitivity of attack rates to reproduction number (R₀). Attack rates for agricultural workers (red) and general population (blue) across regions for R₀ = 1.2, 1.5, 2.0, and 3.0.

**Figure S3.** Sensitivity to assortative mixing parameter (ε). Attack rate differentials (agricultural workers minus general population) for ε = 0, 0.33, 0.5, and 0.7.

**Figure S4.** Sensitivity to secondary attack rate in crowded households. Attack rates by population for crowded household SAR = 30%, 40%, 50%, and 60%.

**Figure S5.** Sensitivity to crowding fold difference. Impact of varying the relationship between household size and crowding probability (fold difference = 1, 2, 3).

**Figure S6.** Sensitivity to county-level NAWS adjustment approach. Comparison of attack rate differentials under three approaches: (A) Null method (regional NAWS values used directly), (B) Additive method (baseline; county-level deviations from regional ACS means added to regional NAWS values), (C) Multiplicative method (regional NAWS values scaled by ratio of county-level to regional mean ACS values).

**Figure S7.** Regional heterogeneity in baseline results. Attack rate differentials by region, with inset showing relationship to regional agricultural worker proportion and crowding levels.

**Figure S8.** Productivity impacts by outbreak timing. Seasonal productivity losses for strawberries, lettuce, and oranges as a function of outbreak peak timing (day of year).

### Supplementary Tables

**Table S1.** Regional household size and crowding data from ACS and NAWS

**Table S2.** Crop calendar and labor intensity data by region  

**Table S3.** Baseline and sensitivity analysis parameter values. Complete specification of all parameter combinations examined.

**Table S4.** Simulation results summary across all scenarios. Peak prevalence, time to peak, and final attack rate for agricultural workers and general population under all parameter combinations.