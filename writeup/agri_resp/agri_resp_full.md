
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

<!---
FIGURE LIST: 
Main: 
- [ ] Household size and crowding (histograms, maps) 
- [ ] Disease simulations 
- [ ] Crop impacts 


Supplementary: 
- [ ] Crop movements over time and averaged 
- [ ] Table of baseline and sensitivity parameter values 
- [ ] Three methods for county imputation 
- [x] Model structure 
- [x] Sensitivity to R0
- [x] Sensitivity to eta
- [x] Sensitivity to SAR 
- [x] Sensitivity to crowding fold difference 
- [ ] Sensitivity to county imputation method 

Other to do: 
- [ ] 


--->

## Abstract

**Background:** Respiratory disease outbreaks pose significant threats to critical infrastructure, including food production systems. Agricultural workers face elevated disease transmission risk due to crowded living conditions and occupational exposures, yet the impact of respiratory outbreaks on agricultural productivity remains poorly quantified. 

**Methods:** We developed a household-structured susceptible-infectious-recovered (SIR) transmission model to compare disease dynamics between agricultural workers and the general U.S. population. Using data from the American Community Survey and National Agricultural Workers Survey, we parameterized household size and crowding distributions across six regions. We simulated outbreaks with reproduction numbers ranging from 1.2 to 3.0 across various assumptions on household secondary attack rate and assortative mixing. We assessed productivity losses for three labor-intensive crops (oranges, lettuce, strawberries) with different harvest seasonalities. 

**Results:** Agricultural worker households were substantially larger (mean size 3.3-4.1 vs. 2.4-2.8) and more crowded (11-33% vs. 2-8%) than the general population. At a baseline reproduction number of 1.5, disease prevalence peaked 1.5-3.0 percentage points higher among agricultural workers compared to the general population, and cumulative attack rates exceeded the general population by 9-16 percentage points. Under baseline assumptions, productivity losses during peak harvest periods reached 114% for strawberries, 90% for lettuce, and 88% for oranges.

**Conclusions:** Household crowding may lead to disproportionate respiratory disease burden among agricultural workers, potentially generating substantial productivity losses. These findings highlight the need for targeted outbreak preparedness and mitigation strategies in the agricultural sector to maintain food system resilience. 


## Introduction

Respiratory disease outbreaks can cause economic and infrastructural disruptions that cascade beyond their immediate public health impacts. The food system is particularly vulnerable to outbreaks [x]. The COVID-19 pandemic, for example, profoundly impacted food production, processing, and distribution [x, Jayson Lusk]. Agricultural labor is the critical foundation upon which the rest of the food system rests, yet agricultural workers may be especially vulnerable to respiratory disease. This vulnerability stems from many factors, including an increased rate of comorbidities [x], limited healthcare access [x], frequent migration [x], and household crowding [x]. 

While differences in health outcomes may be straightforward to account for -- the probability of severe illness given infection, for example, may be higher among agricultural workers, so we can just try to measure and account for that difference -- differences in transmission among agricultural workers vs. the general community are harder to anticipate and account for. These could cause substantially different epidemic trajectories among agricultural workers vs. the general population, making it difficult to use standard surveillance to understand what's happening in agricultural communities and to anticipate the timing and extent of the downstream effects. 

Efforts to do outreach to agricultural workers, both for protecting health and surveillance, are varied, including xxx and xxx. Still, we lack a clear understanding of how disease transmission dynamics differ between agricultural workers and the general population. More importantly, we lack predictive tools for anticipating how novel respiratory outbreaks might impact agricultural workers before they occur. Likewise, analytical frameworks for translating these labor impacts into commodity-specific production impacts remain underdeveloped. 

This study addresses these gaps by developing a disease transmission model specifically designed to anticipate the differential impacts of respiratory virus outbreaks on agricultural workers relative to the general U.S. population. Focusing on household size and crowding – two well-established predictors of respiratory disease transmission – we quantify the relative rate of infections and assess potential impacts on harvesting operations. We apply this framework to three economically important, labor-intensive crops with different harvest seasonalities: oranges, lettuce, and strawberries. Our analysis provides both a quantitative assessment of how outbreaks with varying characteristics might impact the agricultural sector and a generalizable framework for future assessments of disease impacts on agricultural production. 



## Methods




### Data

#### Population characteristics

We obtained county-level data on overall population size, household size distribution (proportion of households of size 1, 2, 3, 4, 5, 6, or 7+), proportion of crowded households (i.e., with more than one individual per room), and proportion of agricultural workers from the U.S. Census Bureau’s 2022 American Community Survey (ACS) 5-year estimates. For agricultural workers specifically, we obtained regional data on household size distribution and proprtion of crowded households from the 2018-2022 National Agricultural Workers Survey (NAWS). The NAWS data are stratified geographically into six regions: East, Southeast, Midwest, Southwest, Northwest, and California. To enable region-level analysis, we aggregated the county-level ACS data into the corresponding NAWS regions using population-weighted averages. Full details on the data extraction are given in the **Supplementary Information.** 

#### Crop harvest calendars and labor requirements

To approximate daily harvest volumes, we obtained data on specialty crop movements (point-to-point shipments) for strawberries, iceberg lettuce, and oranges from the United States Department of Agriculture's (USDA's) Agricultural Marketing Services. We extracted the total weekly weight of shipments originating in California for each of these crops between 1 Jan 2018 and 1 Jan 2025. In 2024, California produced approximately [90% of U.S. strawberries](https://www.nass.usda.gov/Publications/Todays_Reports/reports/ncit0525.pdf), [74% of U.S. iceberg lettuce](https://www.nass.usda.gov/Publications/Todays_Reports/reports/vegean25.pdf), and [78% of U.S. oranges](https://esmis.nal.usda.gov/sites/default/release-files/j9602060k/vx023d76b/w9507070x/cfrt0825.pdf). We averaged the weekly shipment volumes for each crop across the seven available years to mitigate the impact of inter-annual variation. Then, we interpolated daily shipment volumes by assuming equal shipment volumes across each day of the week. We normalized these shipment volumes by the total mean annual shipment volume, so that the daily values reflected the proportion of the total harvest normally collected on that day. We cross-referenced the resulting production curves with independent reports on each crop's production timing (Supplementary Methods, Supplementary Figure XX). 

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

We assumed that mixing among agricultural workers (A) and the general community (C) was assortative, governed by an assortativity parameter $\eta$. We modeled this using the mixing matrix

$$ M =
\begin{pmatrix} m_{CC} & m_{CA} \\\ m_{AC} & m_{AA} \end{pmatrix} =
\begin{pmatrix} \eta + (1-\eta) w_C & (1-\eta) w_A \\\ (1-\eta) w_C & \eta + (1-\eta) w_A \end{pmatrix}
$$

Here, $w_C$ is the fraction of the region's population made up by the general community and $w_A$ is the fraction of the population made up by agricultural workers. This matrix modulates the between-household force of infection $\lambda$ experienced by each population such that

$$\lambda_C = \beta (m_{CC} I_C + m_{CA} I_A)$$
$$\lambda_A = \beta (m_{AC} I_C + m_{AA} I_A)$$

where $\lambda_i$ is the between-household force of infection for members of sub-population $i$, $\beta$ is the between-household transmission constant, and $I_i$ is the proportion of infectious individuals in sub-population $i$; thus, $\eta = 1$ implies completely assortative mixing and $\eta = 0$ implies mixing proportional to each sub-population's size.

Besides the impact of household size, household crowding, and assortative mixing, we did not assume any additional differences in transmission rates between the two sub-populations. For full details on the model structure, see the **Supplementary Methods.** 

#### Model parameterization

Following previous methods [x], we began by fixing the recovery rate $\gamma = 1/5$, which corresponds to a mean infectious period of 5 days. Then, given $\gamma$ and the household secondary attack rate (SAR), we derived $\tau$ (**Supplementary Methods**). We set the SAR for uncrowded households at 20%, following estimates for influenza [x]. For crowded households, we set the baseline SAR at 40% [x]. In sensitivity analyses, we considered crowded-household SARs of 0.2, 0.3, 0.5, and 0.6. Last, given values for $\gamma$ and $\tau$, we numerically identified the value of $\beta$ that would achieve a desired basic reproduction number ($\mathcal{R_0}$) when simulating outbreaks at the national level. Specifically, for a candidate $\beta$ value, we ran the model with a single sub-population to equilibrium; then, we compared the outbreak's final size with the theoretical prediction from the implicit relationship $R(\infty) = 1 - \exp(-\mathcal{R_0} \cdot R(\infty))$, where $R(\infty)$ is the final size of the outbreak. [x] We adjusted $\beta$ using a bisection search algorithm until the simulated final size was within 0.0005 of the theoretical value. For the baseline analysis, we used $\mathcal{R_0}$ = 1.5, reflecting a moderate pandemic influenza scenario and the effective reproduction number during many COVID-19 surges when behavioral mitigations were in place. In sensitivity analyses, we considered $\mathcal{R_0}$ values of 1.2, 2.0, and 3.0. Baseline and sensitivity parameter values are listed in **Supplementary Table XX**. 

For the baseline assortativity, we used $\eta = 2/3$. Because agricultural workers constitute a small fraction of the total population ($w_A$ ranges from 0.7% to 2.2% across regions), the proportion of within-group contacts for agricultural workers is approximately equal to $\eta$ ($m_{AA} = \eta + (1-\eta) w_A \approx \eta$, since $w_A$ is small). At the baseline $\eta = 2/3$, agricultural workers have between 66.9% and 67.4% of ther contacts within their own group, depending on the region. The general community, being much larger, has nearly all contacts within their own group regardless of $\eta$ (**Supplementary Table XX**). In sensitivity analyses, we considered $\eta \in \{0, 1/4, 1/3, 1/2, 3/4\}$. At $\eta = 0$ (proportional mixing), agricultural workers have between 0.7% - 2.2% of their contacts within their own group, depending on the region; at $\eta = 3/4$, this rises to between 75.2% and 75.5%.

We assumed symptoms began one day after infection and lasted for three days. For the crop productivity analysis, we assumed half of all infections were symptomatic enough to cause a laborer to miss work ($p_\text{symp} = 0.5$). We also report findings for $p_\text{symp} = 1$ (**Supplementary Figure XX, Supplementary Table XX**). Since production losses scale linearly with $p_{symp}$, results for any other $p_\text{symp}$ can be obtained by scaling the results for $p_{symp} = 1$.

The transmission model requires knowing the fraction of households of each size are crowded, but the ACS and NAWS data report household size distribution and crowding proportion separately. To assign crowding levels by household size, we assumed that the crowding probability increased linearly from households of size 2 to households of size 7+ (since households of size 1 by definition cannot be crowded). We obeyed the constraints that (1) the overall proportion of crowded households must match the ACS- or NAWS-reported proportion, and (2) households of size 7+ are $d$ times as likely to be crowded as households of size 2 (**Supplementary Methods**). We used a baseline of $d = 2$ (i.e. households of size 7+ are twice as likely to be crowded as households of size 2). In sensitivity analyses, we considered $d = 1$ and $d = 3$. 


#### Model implementation

We implemented the transmission model in `R` (version 4.5.0) using `odin` (version 1.2.7). We initialized outbreaks by setting 0.1% of individuals in each sub-population as infectious. We distributed the initial infectious individuals proportionally across household types, equivalent to uniform-randomly choosing the initial infected individuals from each sub-population. We simulated outbreaks over 365 days. 

In addition to the main regional simulations, we also generated county-level simulations to explore differences in transmission between agricultural workers and the general population at a finer geographic scale. Since the NAWS dataset does not report at the county level, we imputed county-level household size distributions and crowding proportions for agricultural workers using three different methods: an "additive" method, where county-level NAWS values were imputed by adding the difference between the county-level and regional ACS values to the regional NAWS value; a "multiplicative" method, where regional NAWS values were scaled by the ratio of county-level to regional ACS values; and a "null" method, where regional NAWS values were used without adjustment (**Supplementary Methods**). The "null" method yielded no variation in county-level agricultural household characteristics within a region; the "additive" method yielded an intermediate amount of variation; and the "multiplicative" method yielded a high amount of variation (**Supplementary Figure xx**). We treated the "additive" method as a baseline and considered the "null" and "multiplicative" methods in sensitivity analyses. Due to the high uncertainty in these imputation methods, we emphasize that these county-level results have a low level of confidence and are intended to provide a rough estimate of within-region variation around the regional mean. 



### Outcomes and measurements

We compared the difference in household size distribution (mean household size, proportion of households with 4 or more people) and the difference in household crowding rates between agricultural workers and the general community at the region level. For the outbreak simulations, we measured differences in peak prevalence, time to peak, final size, and maximum incidence deviation between agricultural workers and the general community. 

To translate disease dynamics into agricultural productivity impacts, we estimated the number of workers unable to perform harvest labor each day due to symptomatic illness. We assumed that symptomatic individuals could not perform agricultural labor. This allowed us to calculate a daily "workforce strength", consisting of the fraction of agricultural workers still healthy. We multiplied this workforce strength by the daily harvest fraction for each crop to obtain an outbreak-adjusted harvest volume and summed this adjusted volume over the full year to measure the agricultural impact of the outbreak. We did this for outbreaks peaking on each day of the year to assess the impact of outbreak timing on agricultural productivity for each crop. 


## Results

### Household crowding leads to higher modeled disease prevalence among agricultural workers.

Agricultural worker households are substantially larger and more crowded on average than the general U.S. population (Figure 1). Mean household size among agricultural workers ranged from 3.3 to 4.1 across regions, compared to 2.4 to 2.8 for the general population. The proportion of crowded households was 3 to 9 times higher among agricultural workers than the general population across the six regions. Crowding rates among agricultural workers ranged from 11% (Midwest) to 33% (California), compared to 2-8% in the general population.

Simulations of respiratory disease outbreaks at the regional level revealed consistently higher disease burden among agricultural workers compared to the general population (Figure 2). Under baseline assumptions ($R_0$ = 1.5, SAR = 20%/40% for uncrowded/crowded households, $\eta$ = 2/3), peak prevalence among agricultural workers exceeded that of the general population by 1.5 to 3.0 percentage points across regions. Cumulative attack rates were 9 to 16 percentage points higher among agricultural workers, with final attack rates of 66-77% among agricultural workers compared to 56-64% in the general population.

These differences were sensitive to the basic reproduction number. At $R_0$ = 1.2, attack rate differences were largest (12-22 percentage points) despite smaller peak prevalence differences (0.7-1.9 percentage points). At higher transmissibility ($R_0$ = 2.0), peak prevalence differences widened to 1.8-3.3 percentage points, while attack rate differences narrowed to 5-8 percentage points as both populations approached high overall infection levels. At $R_0$ = 3.0, with near-complete infection of both populations, attack rate differences were minimal (1.4-2.3 percentage points) while peak prevalence differences remained substantial (1.6-3.0 percentage points).

Sensitivity to the secondary attack rate in crowded households showed proportional effects on disparities. Increasing SAR in crowded households from 30% to 50% increased the attack rate difference between agricultural workers and the general population by approximately 2-4 percentage points across regions, while reducing SAR to 30% decreased disparities by similar margins.

County-level simulations demonstrated geographic heterogeneity in these infection disparities. Counties with high agricultural employment and elevated crowding rates showed the largest disparities, with some counties experiencing agricultural worker attack rates exceeding their local general population by more than 15 percentage points. These results were sensitive to how household sizes and crowding rates were assigned to the agricultural worker population at the county level, since these data were only available for agricultural workers at the regional level; under the most conservative assumptions (uniform regional crowding rates), disparities were reduced but remained substantial.

### Respiratory disease outbreaks among agricultural workers can lead to substantial productivity losses.

The simulated outbreaks yielded substantial productivity losses for all three crops we considered, with the impact varying by outbreak timing relative to peak harvest periods. For strawberries, peak productivity losses were 1.14% with the worst outbreak timing being an epidemic peak on day 141 (approximately late May). For iceberg lettuce, maximum losses were 0.90% for outbreaks that peaked in late May (day 141). For oranges, peak losses were 0.88% for outbreaks peaking in late January (day 30). These translate into peak losses of roughly $39,404,351, $11,205,945, and $7,502,062 USD for strawberries, head lettuce, and oranges, respectively. 

**Figure 1.** Household characteristics.

![Household characteristics](../../figures/hh_inkscape_white.png)

**Figure 2.** Epidemic simulations.

![Epidemic simulations](../../figures/main_epicurves_white.png)

**Figure 3.** Crop impact.

<!-- ![Crop impact](../../figures/crop_impact.png) --> 
<div align="center">
  <img src="../../figures/crop_impact.png" width="50%">
</div>




<!--- 

2024 California head lettuce value: $1,245,105,000 (https://www.nass.usda.gov/Publications/Todays_Reports/reports/vegean25.pdf)
2024 California orange value: $852,507,000
2024 California strawberry value: $3,456,522,000

Losses: 
39,404,351 strawberries
11,205,945 head lettuce 
7,502,062 oranges
---> 




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

#### Data extraction

County-level household size distributions were obtained from American Community Survey (ACS) table B11016 (Household Type by Household Size), which reports counts of family and non-family households by size. We combined family and non-family counts for each household size (1 through 7+). 

County-level household crowding proportions were obtained from ACS table B25014 (Tenure by Occupants per Room), where crowded households were defined as those with more than 1.00 occupants per room. We summed across owner- and renter-occupied units and all occupancy levels over size 1 (1.01–1.50, 1.51–2.00, and >2.00 persons per room). We normalized by the total population size (ACS table B01003).

To calculate the proportion of agricultural workers in a county using the ACS data, we extracted the number of individuals employed in "farming, fishing, and forestry occupations" (ACS occupation codes C24030_004 [males] and C24030_031 [females]) as a proportion of total employed individuals (C24030_001).

#### Data processing: calculating regional values for the general community

To enable region-level analysis, we aggregated the county-level ACS data into the corresponding National Agricultural Workers Survey (NAWS) regions using population-weighted averages. For each variable (household size proportions, crowding proportions, and proportion of agricultural workers), we multiplied each county's value by its population size, summed within each region, then divided by the total regional population size. Specifically, for each county $i$ in region $r$, we computed:

$$\bar{p}_{\text{ACS},r}(n) = \frac{\sum_{i \in r} p_{\text{ACS},i}(n) \cdot N_i}{\sum_{i \in r} N_i}$$

$$\bar{q}_{\text{ACS},r} = \frac{\sum_{i \in r} q_{\text{ACS},i} \cdot N_i}{\sum_{i \in r} N_i}$$

$$\bar{a}_{\text{ACS},r} = \frac{\sum_{i \in r} a_{\text{ACS},i} \cdot N_i}{\sum_{i \in r} N_i}$$

where $p_{\text{ACS},i}(n)$ is the proportion of households with size $n$ in county $i$ according to the ACS data, $q_{\text{ACS},i}$ is the proportion of crowded households in county $i$, $a_{\text{ACS},i}$ is the proportion of agricultural workers in county $i$, and $N_i$ is the population size of county $i$. We re-normalized 

$$\bar{p}_{\text{ACS},r}(n)$$  

to ensure 

$$\sum_n \bar{p}_{\text{ACS},r}(n) = 1$$. 

Household sizes for agricultural workers were derived from the NAWS D52 variable (total number of people sleeping in the housing unit). Households of size 7 or greater were grouped into a single "7+" category for consistency with the ACS data. Crowding status was derived from the CROWDED1 variable. Both household size and crowding data were weighted using the NAWS survey weights (PWTYCRD) and summarized by NAWS region.

#### Data processing: calculating crowding by household size

In both the ACS and NAWS datasets, household size and household crowding are reported separately, but for the disease transmission model, we required the proportion of households of a given size that are crowded. To assign crowding probabilities by household size, we used a linear relationship where larger households are progressively more likely to be crowded. For a household of size $n$, we defined a crowding multiplier:

$$w(n) = \begin{cases} 0 & n = 1 \\\ 1 + (d - 1) \cdot \frac{n - 2}{5} & n \geq 2 \end{cases}$$

where $d$ is a crowding fold-difference parameter (the ratio of crowding probability for size-7 households to size-2 households). For example, when $d = 2$, $w(n) = \\{ 0, 1, 1.2, 1.4, 1.6, 1.8, 2\\}$ for $n \in \\{1, 2, ..., 7\\}$. Note that it is impossible for size-1 households to be crowded. We treated households of size 7+ as $n = 7$. For a given region and sub-population, the probability that a household of size $n$ is crowded is then:

$$p_{\text{crowded}}(n) = \xi \cdot w(n)$$

where the constant $\xi$ is chosen so that the total proportion of crowded households in the region, $\sum_n p(n) p_\text{crowded}(n)$, matches the observed fraction of crowded households, $P_\text{crowded}$ (here, $p(n)$ is the proportion of households that are size $n$). Specifically, 

<!-- $$\xi = \frac{P_{\text{crowded}}}{\sum_n p(n) \cdot w(n)}$$ -->
$$\xi = \frac{P_{\text{crowded}}}{\sum_n p(n) \cdot w(n)}$$

For example, for household size proportions $p(n) = \\{ 0.1, 0.2, 0.3, 0.2, 0.1, 0.05, 0.05\\}$ for $n \in \\{1, 2, ..., n\\}$, and for an overall crowding fraction of $$P_\text{crowded} = 0.2$$, we have 

$$ \xi = \frac{0.2}{(0.1)(0) + (0.2)(1) + (0.3)(1.2) + (0.2)(1.4) + (0.1)(1.6) + (0.05)(1.8) + (0.05)(2)}$$
$$ = 0.168$$

which ensures that 

$$\sum_n p(n) p_\text{crowded}(n) = \sum_n  p(n) \xi w(n) $$
$$ = 0.168 [(0.1)(0) + (0.2)(1) + (0.3)(1.2) + (0.2)(1.4) + (0.1)(1.6) + (0.05)(1.8) + (0.05)(2)]$$
$$ = 0.2 = P_\text{crowded}$$

#### Data processing: imputing county-level household characteristics for agricultural workers

The NAWS dataset reports household characteristics for agricultural workers at the regional level only, while the ACS provides county-level data for the general population. While our main analysis was at the regional level, we also performed a county-level analysis to assess within-region variation in outbreak disparities between agriculural workers and the general community. To generate county-level population estimates for agricultural workers, we used county-level ACS variation to adjust the regional NAWS values. The underlying assumption is that county-level variation among agricultural workers follows a similar pattern to county-level variation in the general population; i.e., if a county's general population has larger households than the regional average, agricultural workers in that county likely also have larger households than the regional average for agricultural workers.

<!-- For each county $i$ in region $r$, we first computed the population-weighted regional mean of the county-level ACS values:

$$\bar{p}_{\text{ACS},r}(n) = \frac{\sum_{i \in r} p_{\text{ACS},i}(n) \cdot N_i}{\sum_{i \in r} N_i}$$

$$\bar{q}_{\text{ACS},r} = \frac{\sum_{i \in r} q_{\text{ACS},i} \cdot N_i}{\sum_{i \in r} N_i}$$

where $p_{\text{ACS},i}(n)$ is the proportion of households with size $n$ in county $i$ according to ACS data, $q_{\text{ACS},i}$ is the proportion of crowded households in county $i$, and $N_i$ is the population size of county $i$. -->

We imputed county-level NAWS values using three methods (**Figure XX**):

*Additive method.* We shifted regional NAWS values by the difference between county-level and regional mean ACS values:

$$\tilde{p}_{\text{NAWS},i}(n) \propto \max\left(0, \; p_{\text{NAWS},r}(n) + \left[ p_{\text{ACS},i}(n) - \bar{p}_{\text{ACS},r}(n) \right] \right)$$

$$\tilde{q}_{\text{NAWS},i} = q_{\text{NAWS},r} + \left[ q_{\text{ACS},i} - \bar{q}_{\text{ACS},r} \right]$$

Household size proportions were clamped to be non-negative before renormalization, and the crowding proportion was clamped to $[0, 1]$ (i.e., values below 0 were set to 0 and values above 1 were set to 1).

*Multiplicative method.* We scaled regional NAWS values by the ratio of county-level to regional mean ACS values:

$$\tilde{p}_{\text{NAWS},i}(n) \propto p_{\text{NAWS},r}(n) \times \frac{p_{\text{ACS},i}(n)}{\bar{p}_{\text{ACS},r}(n)}$$

$$\tilde{q}_{\text{NAWS},i} = q_{\text{NAWS},r} \times \frac{q_{\text{ACS},i}}{\bar{q}_{\text{ACS},r}}$$

The household size distribution was renormalized to sum to 1, and the crowding proportion was clamped to the interval $[0, 1]$.

*Null method.* We used regional NAWS values directly without adjustment:

$$\tilde{p}_{\text{NAWS},i}(n) = p_{\text{NAWS},r}(n)$$

$$\tilde{q}_{\text{NAWS},i} = q_{\text{NAWS},r}$$

This method assumes no county-level variation in agricultural worker household characteristics within a region.

#### Data processing: assessing the validity of crop movements as a proxy for harvest

We used crop movements (point-to-point shipments) reported by the USDA as a proxy for harvest volumes of oranges, iceberg lettuce, and strawberries. Few available data sources capture crop-specific harvest volumes at sub-annual temporal resolution, whereas we needed information on the seasonality of harvesting to assess the potential impact of epidemic timing on crop production. To validate the relationship between crop movements and harvests, we cross-referenced normalized average weekly crop movement volumes against independent harvest information for the same crops from University of California Agriculture and Natural Resources Cooperative Extension reports (**Figure S25**). For strawberries, the report gives the fraction of total annual harvest occurring in each month for the Central Coast Region of California (Santa Cruz, Monterey, and San Benito Counties): 5% in April, 12% in May, 25% in June, 26% in July, 18% in August, 12% in September, and 2% in October. For navel oranges, the report states that fruits in the San Joaquin Valley are "normally harvested from November to June". For iceberg lettuce, the report states that planting in the Central Coast Region occurs "continuously from late December to mid-August" and that plants take up to 100 days to mature for cool-season plantings, with shorter maturation times in the warmer season.

We overlaid this information on the normalized crop movement data (**Figure S25**). For strawberries, we converted the monthly harvest proportions to approximate weekly rates by dividing the monthly harvest proportions by the number of weeks in the month (e.g., by 4.28 for a 30-day month). For oranges, we showed the November-to-June harvest window as a horizontal bar. For lettuce, we displayed both the planting window (late December to mid-August) as a lighter bar and an estimated harvest window (approximately late March to early October, obtained by shifting the planting window forward by 100 days at the cool-season end and 50 days at the warm-season end) as a darker bar. There are differences in alignment between the crop movement data and the University of California reports; for example, the crop movement data place peak strawberry shipments in late April and early May, whereas the University of California report indicates peak harvests in June and July; and iceberg lettuce shipments are shifted somewhat later than the estimated harvest window. These discrepancies may reflect limitations of using crop movements as a proxy for harvests, but may also reflect the fact that the movement data capture shipments from across the entire state of California, while the University of California reports pertain to specific sub-regions. With this caveat, we conclude that the crop movement data provide a reasonable proxy for the seasonal pattern of crop harvests.

#### Mathematical model structure

The household-structured SIR model (**Figure XX**) tracks the distribution of households across disease states. Let $H_k(x,y,z,c)$ denote the number of households in population $k$ (where $k \in \{C, A\}$ for community and agricultural populations) with $x$ susceptible, $y$ infected, and $z$ recovered members, and crowding status $c \in \{0,1\}$. The total household size is $n = x + y + z$.

The dynamics are governed by three types of transitions:

- **Recovery transitions:** Infected individuals recover at rate $\gamma$, moving a household from state $(x,y,z,c)$ to state $(x,y-1,z+1,c)$:
$\text{Recovery rate} = \gamma \cdot y \cdot H_k(x,y,z,c)$

- **Within-household transmission:** Susceptible individuals are infected by household members at rate $\tau_c = \tau_{\text{base}} + \tau_{\text{boost}} \cdot c$, moving a household from state $(x,y,z,c)$ to state $(x-1,y+1,z,c)$:
$\text{Within-household infection rate} = \tau_c \cdot x \cdot y \cdot H_k(x,y,z,c)$

- **Between-household transmission:** Susceptible individuals are infected through community contacts at rate $\lambda_k$, determined by the mixing matrix and overall prevalence in each population:
$\text{Between-household infection rate} = \lambda_k \cdot x \cdot H_k(x,y,z,c)$

The between-household force of infection for population $k$ is:
$\lambda_k = \beta \left[ m_{kk} I_k + m_{kj} I_j \right]$

where $I_k$ is the prevalence in population $k$:
$I_k = \frac{\sum_{x,y,z,c} y \cdot H_k(x,y,z,c)}{\sum_{x,y,z,c} n \cdot H_k(x,y,z,c)}$

The mixing matrix elements are:
$$ M =
\begin{pmatrix} m_{CC} & m_{CA} \\\ m_{AC} & m_{AA} \end{pmatrix} =
\begin{pmatrix} \eta + (1-\eta) w_C & (1-\eta) w_A \\\ (1-\eta) w_C & \eta + (1-\eta) w_A \end{pmatrix}
$$

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

$$\tau = \frac{(0.2)(0.2)}{1 - 0.2} = 0.05$$

For crowded households, we computed $\tau_{\text{crowded}}$ using the same formula with the crowded SAR, then defined $\tau_{\text{boost}} = \tau_{\text{crowded}} - \tau$. For the baseline crowded SAR of 40%: $\tau_{\text{crowded}} = 0.40 \times 0.2 / 0.60 \approx 0.133$ and thus $\tau_{\text{boost}} \approx 0.083$. 

Next, we calibrated the between-household transmission rate $\beta$ to achieve target $R_0$ values by running the model at the national level with aggregated ACS household data and systematically varying $\beta$ until the final attack rate matched theoretical predictions for the desired $R_0$. For an SIR model, the relationship between $R_0$ and final attack rate $R_\infty$ is given implicitly by:
$R_\infty = 1 - e^{-R_0 R_\infty}$

For example, $R_0 = 1.2$ corresponds to $R_\infty \approx 0.31$, and $R_0 = 2.0$ corresponds to $R_\infty \approx 0.80$. We used a bisection search algorithm to find $\beta$, converging when the simulated final attack rate was within 0.0005 of the theoretical value. Calibration was performed using a single-population simulation at the national level, with national household distributions computed as population-weighted averages of all county-level ACS data. The calibrated $\beta$ values were then used in the full two-population regional simulations. We assumed no difference in $\beta$ between agricultural workers and the general community, so that all of the transmission differences in the model would come from differences in household size and crowding. 

We initialized outbreaks by setting 0.1% of individuals in each sub-population as infectious. The initial infectious individuals were distributed proportionally across household types, weighted by household size: for each household type with $n$ members, a fraction $\text{initprev} \times n$ of those households were moved from the fully susceptible state $(x = n, y = 0, z = 0)$ to the state $(x = n-1, y = 1, z = 0)$. This is equivalent to uniformly randomly selecting 0.1% of individuals to be initially infected, then distributing them across household types according to the population distribution.

#### Symptomatic infection and workforce impact

To translate epidemic dynamics into agricultural workforce availability, we computed the proportion of the agricultural workforce experiencing symptoms on each day. We first calculated the number of new daily infections for each day as $i_t = S(t-1) - S(t)$, where $S(t)$ is the proportion susceptible at time $t$. We assumed symptoms began one day after infection onset and lasted for three days, so that individuals infected on day $t$ were symptomatic on days $t+1$, $t+2$, and $t+3$. The proportion of symptomatic individuals on day $t$ was then:

$$\text{symp}_t = p_{\text{symp}} \sum_{d=1}^{3} i_{t-d}$$

where $p_{\text{symp}}$ is the probability that an infection is symptomatic. Daily workforce strength was defined as $\text{wf}(t) = 1 - \text{symp}(t)$. 

To assess the impact of epidemic timing on crop production, we shifted the epidemic curve so that the community symptomatic peak aligned with each day of the calendar year. The outbreak-adjusted harvest volume for each crop on each day was $V_{\text{adj}}(t) = V(t) \times \text{wf}(t)$, and total annual production loss was $(1 - \sum_t V_{\text{adj}}(t) / \sum_t V(t)) \times 100\%$.

### Supplementary Figures

**Figure S1.** Schematic of the household-structured two-population SIR model. Households are stratified by size (1–7+), crowding status (crowded or uncrowded), and population (agricultural workers, A, or general community, C). Within each household, susceptible individuals (S) become infected (I) through within-household transmission at rate $\tau$ (or $\tau + \tau_{\text{boost}}$ in crowded households) and through between-household transmission at rate $\lambda_k$, and recover (R) at rate $\gamma$. Between-household transmission is governed by a mixing matrix parameterized by the assortativity parameter $\eta$.

![Model structure](../../figures/modelstructure.png)

**Figure S2.** Distribution of county-level household crowding proportions under each county-level imputation method for agricultural workers. Each panel corresponds to one of the six NAWS regions. Histograms show the distribution of the imputed proportion of crowded households across counties within each region for (a) the additive method, (b) the multiplicative method, and (c) the null method (no county-level adjustment; all counties within a region receive the same regional NAWS value). Red bars show the ACS county-level distribution for the general population, red dashed vertical lines indicate the regional ACS mean, and blue dashed vertical lines indicate the regional NAWS estimate for agricultural workers.

![Crowding distributions imputed additive](../../figures/crowding_distribution_additive.png)
![Crowding distributions imputed multiplicative](../../figures/crowding_distribution_multiplicative.png)
![Crowding distributions imputed none](../../figures/crowding_distribution_none.png)

**Figure S3.** Distribution of county-level mean household size and proportion of households with 4 or more members under each county-level imputation method for agricultural workers. Each panel corresponds to one of the six NAWS regions. Histograms show the distribution across counties within each region for (a) the additive method, (b) the multiplicative method, and (c) the null method. Red bars show the ACS county-level distribution for the general population, and blue dashed vertical lines indicate the regional NAWS estimate for agricultural workers.

![Mean and four plus household size distributions imputed additive](../../figures/hhsize_distribution_additive.png)
![Mean and four plus household size distributions imputed multiplicative](../../figures/hhsize_distribution_multiplicative.png)
![Mean and four plus household size distributions imputed none](../../figures/hhsize_distribution_none.png)

**Figure S4.** Sensitivity of final attack rate to the basic reproduction number ($R_0$), assortativity ($\eta$), secondary attack rate in crowded households (SAR), and crowding fold difference. Each panel shows one sensitivity dimension, with parameter values on the horizontal axis and final attack rate (proportion of the population ultimately infected) on the vertical axis. Colored lines connect results across parameter values for each of the six NAWS regions (East, Southeast, Midwest, Southwest, Northwest, California), using an Okabe-Ito colorblind-friendly palette. Solid lines with points represent agricultural workers (A); dashed lines with open points represent the general community (C). A horizontal gray dashed reference line indicates the baseline value.

![Sensitivity overview final size](../../figures/sensitivity_overview_attackrate.png)

**Figure S5.** Sensitivity of peak prevalence to the basic reproduction number ($R_0$), assortativity ($\eta$), secondary attack rate in crowded households (SAR), and crowding fold difference. Layout and visual encoding are as in Figure S4, with peak prevalence (maximum proportion infected at any single time point) on the vertical axis.

![Sensitivity overview peak size](../../figures/sensitivity_overview_peaksize.png)

**Figure S6.** Sensitivity of time to peak prevalence to the basic reproduction number ($R_0$), assortativity ($\eta$), secondary attack rate in crowded households (SAR), and crowding fold difference. Layout and visual encoding are as in Figure S4, with time to peak (days from simulation start to maximum prevalence) on the vertical axis.

![Sensitivity overview peak timing](../../figures/sensitivity_overview_peaktiming.png)

**Figure S7.** Sensitivity of the maximum relative infection rate (agricultural workers divided by community) to the basic reproduction number ($R_0$), assortativity ($\eta$), secondary attack rate in crowded households (SAR), and crowding fold difference. Layout and visual encoding are as in Figure S4, with the maximum ratio of agricultural worker to community infection prevalence on the vertical axis. A horizontal gray dashed reference line at 1.0 indicates equal infection rates between the two populations.

![Sensitivity overview max relative infection](../../figures/sensitivity_overview_max_relative_infection.png)

**Figure S8.** Sensitivity of the attack rate ratio (agricultural workers divided by community) to the basic reproduction number ($R_0$), assortativity ($\eta$), secondary attack rate in crowded households (SAR), and crowding fold difference. Layout and visual encoding are as in Figure S4, with the ratio of agricultural worker to community final attack rates on the vertical axis. A horizontal gray dashed reference line at 1.0 indicates equal attack rates.

![Sensitivity overview attack rate ratio](../../figures/sensitivity_overview_attack_rate_ratio.png)

**Figure S9.** Sensitivity of the peak prevalence ratio (agricultural workers divided by community) to the basic reproduction number ($R_0$), assortativity ($\eta$), secondary attack rate in crowded households (SAR), and crowding fold difference. Layout and visual encoding are as in Figure S4, with the ratio of agricultural worker to community peak prevalence on the vertical axis. A horizontal gray dashed reference line at 1.0 indicates equal peak prevalence.

![Sensitivity overview peak prevalence ratio](../../figures/sensitivity_overview_peak_prevalence_ratio.png)

**Figure S10.** Epidemic curves under sensitivity to $R_0$, showing proportion currently infected over time across the six NAWS regions. Each panel corresponds to one region. Within each panel, different colors represent different $R_0$ values (1.2, 1.5, 2.0, 3.0). Solid lines represent agricultural workers (A); dashed lines represent the general community (C).

![Sensitivity R0 epidemic curves](../../figures/sensitivity_r0_curves_all_regions.png)

**Figure S11.** Cumulative infection curves under sensitivity to $R_0$, showing cumulative proportion infected over time across the six NAWS regions. Visual encoding is as in Figure S10. The vertical axis ranges from 0 to 1.

![Sensitivity R0 cumulative](../../figures/sensitivity_r0_cumulative_all_regions.png)

**Figure S12.** Relative infection rate (agricultural workers divided by community) over time under sensitivity to $R_0$ across the six NAWS regions. Each panel corresponds to one region. Different colors represent different $R_0$ values. A horizontal gray dashed line at 1.0 indicates equal infection rates. Values above 1.0 indicate that agricultural workers have higher infection prevalence than the general community.

![Sensitivity R0 relative infection](../../figures/sensitivity_r0_relative_infection_all_regions.png)

**Figure S13.** Epidemic curves under sensitivity to assortativity ($\eta$), showing proportion currently infected over time across the six NAWS regions. Visual encoding is as in Figure S10, with different colors representing different $\eta$ values (0, 0.25, 0.33, 0.50, 0.67, 0.75). Higher $\eta$ implies more within-group mixing. Solid lines: agricultural workers (A); dashed lines: general community (C).

![Sensitivity eps epidemic curves](../../figures/sensitivity_eps_curves_all_regions.png)

**Figure S14.** Cumulative infection curves under sensitivity to assortativity ($\eta$) across the six NAWS regions. Visual encoding is as in Figure S13.

![Sensitivity eps cumulative](../../figures/sensitivity_eps_cumulative_all_regions.png)

**Figure S15.** Relative infection rate (agricultural workers divided by community) over time under sensitivity to assortativity ($\eta$) across the six NAWS regions. Visual encoding is as in Figure S12, with different colors for each $\eta$ value.

![Sensitivity eps relative infection](../../figures/sensitivity_eps_relative_infection_all_regions.png)

**Figure S16.** Epidemic curves under sensitivity to the secondary attack rate (SAR) in crowded households, showing proportion currently infected over time across the six NAWS regions. Visual encoding is as in Figure S10, with different colors representing SAR values (20%, 30%, 40%, 50%, 60%). Solid lines: agricultural workers (A); dashed lines: general community (C).

![Sensitivity SAR epidemic curves](../../figures/sensitivity_sar_curves_all_regions.png)

**Figure S17.** Cumulative infection curves under sensitivity to the secondary attack rate in crowded households across the six NAWS regions. Visual encoding is as in Figure S16.

![Sensitivity SAR cumulative](../../figures/sensitivity_sar_cumulative_all_regions.png)

**Figure S18.** Relative infection rate (agricultural workers divided by community) over time under sensitivity to the secondary attack rate in crowded households across the six NAWS regions. Visual encoding is as in Figure S12, with different colors for each SAR value.

![Sensitivity SAR relative infection](../../figures/sensitivity_sar_relative_infection_all_regions.png)

**Figure S19.** Epidemic curves under sensitivity to the crowding fold difference, showing proportion currently infected over time across the six NAWS regions. Visual encoding is as in Figure S10, with different colors representing fold difference values (1, 2, 3). A fold difference of 1 means no size-dependent crowding gradient; a fold difference of 3 means households of size 7+ are three times as likely to be crowded as households of size 2. Solid lines: agricultural workers (A); dashed lines: general community (C).

![Sensitivity fold epidemic curves](../../figures/sensitivity_fold_curves_all_regions.png)

**Figure S20.** Cumulative infection curves under sensitivity to the crowding fold difference across the six NAWS regions. Visual encoding is as in Figure S19.

![Sensitivity fold cumulative](../../figures/sensitivity_fold_cumulative_all_regions.png)

**Figure S21.** Relative infection rate (agricultural workers divided by community) over time under sensitivity to the crowding fold difference across the six NAWS regions. Visual encoding is as in Figure S12, with different colors for each fold difference value.

![Sensitivity fold relative infection](../../figures/sensitivity_fold_relative_infection_all_regions.png)

**Figure S22.** County-level epidemic dynamics under the multiplicative county-level imputation method for agricultural worker household characteristics. Each of the six panels corresponds to one NAWS region. Thin semi-transparent lines show individual county trajectories; thick lines with black outlines show the population-weighted regional average. Blue lines represent agricultural workers (A); red lines represent the general community (C). Top row: proportion currently infected over time. Middle row: cumulative proportion infected over time. Bottom row: relative infection rate (A/C ratio) over time, with purple lines for the regional average and grey lines for individual counties; a horizontal dashed line at 1.0 indicates equal infection rates.

![Epidemic multiplicative](../../figures/main_overlay_multiplicative.png)

![Cumulative infections multiplicative](../../figures/main_cumulative_multiplicative.png)

![Relative infections multiplicative](../../figures/main_relative_infection_multiplicative.png)

**Figure S23.** County-level epidemic dynamics under the null county-level imputation method (regional NAWS values used directly for all counties within a region, with no county-level adjustment). Visual encoding is as in Figure S22.

![Epidemic none](../../figures/main_overlay_none.png)

![Cumulative infections none](../../figures/main_cumulative_none.png)

![Relative infections none](../../figures/main_relative_infection_none.png)

**Figure S24.** Weekly crop shipments (in million pounds) for iceberg lettuce, oranges, and strawberries originating from California, 2018–2025. Each commodity is shown as a separate colored line: oranges in orange, strawberries in magenta, and iceberg lettuce in blue.

![Crop movements](../../figures/crop_movements_raw.png)

**Figure S25.** Comparison of USDA crop movement data with University of California harvest information for three California crops. Solid lines show normalized average weekly USDA shipment volumes (proportion of total annual volume) for oranges (orange), strawberries (magenta), and iceberg lettuce (blue), averaged across years 2018-2024. The dashed magenta stairstep shows monthly strawberry harvest proportions from the UC report, converted to approximate weekly rates. The orange horizontal bar indicates the reported orange harvest season (November to June). Two blue horizontal bars indicate the lettuce planting season (lighter; December to mid-August) and estimated harvest season (darker; approximately mid-March to early October, derived by shifting the planting window forward by 100 days at the cool-season end and 50 days at the warm-season end to account for maturation time).

![Crop movements validated](../../figures/crop_movements_validated.png)

**Figure S26.** Estimated annual crop production loss (%) as a function of epidemic peak timing, under baseline parameters ($R_0 = 1.5$, $p_{\text{symp}} = 1$). The horizontal axis shows the day of the year on which the community symptomatic peak occurs; the vertical axis shows the resulting percentage loss in total annual production due to workforce illness. Each colored line represents one commodity. Losses are highest when the epidemic peak coincides with peak harvest periods.

![Crop impact by peak day](../../figures/crop_impact_by_peakday.png)

### Supplementary Tables

**Table S1.** Baseline and sensitivity analysis parameter values. The baseline parameter set is shown in bold. Each sensitivity analysis varies one parameter at a time while holding all others at baseline values. The within-household transmission rate ($\tau$) is derived from the uncrowded SAR, the crowded household boost ($\tau_{\text{boost}}$) is derived from the difference between crowded and uncrowded SARs, and the between-household transmission rate ($\beta$) is calibrated to achieve the target $R_0$.

| Parameter set | $R_0$ | $\eta$ | SAR (crowded) | Fold diff. | $\tau$ | $\tau_{\text{boost}}$ | $\beta$ | $\gamma$ |
|:---|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|
| **r0_1.5 (baseline)** | **1.5** | **0.67** | **40%** | **2** | **0.050** | **0.083** | **calibrated** | **0.200** |
| r0_1.2 | 1.2 | 0.67 | 40% | 2 | 0.050 | 0.083 | calibrated | 0.200 |
| r0_2 | 2.0 | 0.67 | 40% | 2 | 0.050 | 0.083 | calibrated | 0.200 |
| r0_3 | 3.0 | 0.67 | 40% | 2 | 0.050 | 0.083 | calibrated | 0.200 |
| eps_0.25 | 1.5 | 0.75 | 40% | 2 | 0.050 | 0.083 | calibrated | 0.200 |
| eps_0.5 | 1.5 | 0.50 | 40% | 2 | 0.050 | 0.083 | calibrated | 0.200 |
| eps_0.6667 | 1.5 | 0.33 | 40% | 2 | 0.050 | 0.083 | calibrated | 0.200 |
| eps_0.75 | 1.5 | 0.25 | 40% | 2 | 0.050 | 0.083 | calibrated | 0.200 |
| eps_1 | 1.5 | 0.00 | 40% | 2 | 0.050 | 0.083 | calibrated | 0.200 |
| sar_0.2 | 1.5 | 0.67 | 20% | 2 | 0.050 | 0.000 | calibrated | 0.200 |
| sar_0.3 | 1.5 | 0.67 | 30% | 2 | 0.050 | 0.036 | calibrated | 0.200 |
| sar_0.5 | 1.5 | 0.67 | 50% | 2 | 0.050 | 0.150 | calibrated | 0.200 |
| sar_0.6 | 1.5 | 0.67 | 60% | 2 | 0.050 | 0.250 | calibrated | 0.200 |
| fold_1 | 1.5 | 0.67 | 40% | 1 | 0.050 | 0.083 | calibrated | 0.200 |
| fold_3 | 1.5 | 0.67 | 40% | 3 | 0.050 | 0.083 | calibrated | 0.200 |

**Table S2.** Mixing matrix elements by region and assortativity parameter ($\eta$). For each region, $w_A$ is the proportion of the population that are agricultural workers (derived from ACS data). The mixing matrix governs between-household contact patterns: $m_{AA}$ is the fraction of agricultural workers' between-household contacts that are with other agricultural workers, $m_{AC}$ is the fraction with the general community, and vice versa for $m_{CC}$ and $m_{CA}$. Because $w_A$ is small (0.7–2.2%), $m_{AA} \approx \eta$ and $m_{CC} \approx 1$ across all values of $\eta$. At $\eta = 0$ (proportional mixing), agricultural workers have only $w_A$ of contacts within their own group; the baseline $\eta = 2/3$ is highlighted in bold.

| $\eta$ | Region | $w_A$ (%) | $m_{AA}$ (%) | $m_{AC}$ (%) | $m_{CC}$ (%) | $m_{CA}$ (%) |
|:---:|:---|:---:|:---:|:---:|:---:|:---:|
| 0 | East | 0.7 | 0.7 | 99.3 | 99.3 | 0.7 |
| 0 | Southeast | 1.0 | 1.0 | 99.0 | 99.0 | 1.0 |
| 0 | Midwest | 1.6 | 1.6 | 98.4 | 98.4 | 1.6 |
| 0 | Southwest | 1.0 | 1.0 | 99.0 | 99.0 | 1.0 |
| 0 | Northwest | 2.1 | 2.1 | 97.9 | 97.9 | 2.1 |
| 0 | California | 2.2 | 2.2 | 97.8 | 97.8 | 2.2 |
| 1/4 | East | 0.7 | 25.5 | 74.5 | 99.5 | 0.5 |
| 1/4 | Southeast | 1.0 | 25.8 | 74.2 | 99.2 | 0.8 |
| 1/4 | Midwest | 1.6 | 26.2 | 73.8 | 98.8 | 1.2 |
| 1/4 | Southwest | 1.0 | 25.7 | 74.3 | 99.3 | 0.7 |
| 1/4 | Northwest | 2.1 | 26.6 | 73.4 | 98.4 | 1.6 |
| 1/4 | California | 2.2 | 26.6 | 73.4 | 98.4 | 1.6 |
| 1/3 | East | 0.7 | 33.8 | 66.2 | 99.5 | 0.5 |
| 1/3 | Southeast | 1.0 | 34.0 | 66.0 | 99.3 | 0.7 |
| 1/3 | Midwest | 1.6 | 34.4 | 65.6 | 99.0 | 1.0 |
| 1/3 | Southwest | 1.0 | 34.0 | 66.0 | 99.4 | 0.6 |
| 1/3 | Northwest | 2.1 | 34.7 | 65.3 | 98.6 | 1.4 |
| 1/3 | California | 2.2 | 34.8 | 65.2 | 98.5 | 1.5 |
| 1/2 | East | 0.7 | 50.4 | 49.6 | 99.6 | 0.4 |
| 1/2 | Southeast | 1.0 | 50.5 | 49.5 | 99.5 | 0.5 |
| 1/2 | Midwest | 1.6 | 50.8 | 49.2 | 99.2 | 0.8 |
| 1/2 | Southwest | 1.0 | 50.5 | 49.5 | 99.5 | 0.5 |
| 1/2 | Northwest | 2.1 | 51.1 | 48.9 | 98.9 | 1.1 |
| 1/2 | California | 2.2 | 51.1 | 48.9 | 98.9 | 1.1 |
| **2/3** | **East** | **0.7** | **66.9** | **33.1** | **99.8** | **0.2** |
| **2/3** | **Southeast** | **1.0** | **67.0** | **33.0** | **99.7** | **0.3** |
| **2/3** | **Midwest** | **1.6** | **67.2** | **32.8** | **99.5** | **0.5** |
| **2/3** | **Southwest** | **1.0** | **67.0** | **33.0** | **99.7** | **0.3** |
| **2/3** | **Northwest** | **2.1** | **67.4** | **32.6** | **99.3** | **0.7** |
| **2/3** | **California** | **2.2** | **67.4** | **32.6** | **99.3** | **0.7** |
| 3/4 | East | 0.7 | 75.2 | 24.8 | 99.8 | 0.2 |
| 3/4 | Southeast | 1.0 | 75.3 | 24.7 | 99.7 | 0.3 |
| 3/4 | Midwest | 1.6 | 75.4 | 24.6 | 99.6 | 0.4 |
| 3/4 | Southwest | 1.0 | 75.2 | 24.8 | 99.8 | 0.2 |
| 3/4 | Northwest | 2.1 | 75.5 | 24.5 | 99.5 | 0.5 |
| 3/4 | California | 2.2 | 75.5 | 24.5 | 99.5 | 0.5 |

**Table S3.** Simulation results: peak prevalence, time to peak, and final attack rate for agricultural workers (A) and the general community (C) across all parameter sets and regions. Results are shown for the baseline additive county-level imputation method.

| Parameter set | Region | Peak prev. (A) | Peak prev. (C) | Time to peak (A) | Time to peak (C) | Attack rate (A) | Attack rate (C) |
|:---|:---|:---:|:---:|:---:|:---:|:---:|:---:|
| **r0_1.5 (baseline)** | East | 8.9% | 6.3% | 45 | 55 | 72.1% | 57.2% |
| | Southeast | 8.9% | 6.2% | 44 | 56 | 72.3% | 56.5% |
| | Midwest | 7.6% | 6.1% | 50 | 56 | 65.9% | 56.4% |
| | Southwest | 8.9% | 7.2% | 46 | 51 | 69.4% | 60.2% |
| | Northwest | 9.7% | 6.9% | 43 | 52 | 73.6% | 58.9% |
| | California | 11.4% | 8.4% | 39 | 47 | 76.7% | 63.5% |
| r0_1.2 | East | 2.9% | 1.4% | 71 | 99 | 49.8% | 29.5% |
| | Southeast | 2.9% | 1.3% | 71 | 100 | 49.8% | 28.4% |
| | Midwest | 2.0% | 1.3% | 87 | 102 | 40.3% | 28.3% |
| | Southwest | 2.9% | 2.0% | 76 | 88 | 47.1% | 34.7% |
| | Northwest | 3.4% | 1.8% | 68 | 90 | 52.6% | 32.7% |
| | California | 4.8% | 2.9% | 59 | 76 | 59.2% | 40.4% |
| r0_2 | East | 18.7% | 15.6% | 28 | 33 | 86.8% | 79.2% |
| | Southeast | 18.7% | 15.5% | 28 | 33 | 87.0% | 78.9% |
| | Midwest | 17.2% | 15.4% | 31 | 33 | 83.8% | 78.8% |
| | Southwest | 18.5% | 16.6% | 29 | 31 | 85.3% | 80.5% |
| | Northwest | 19.4% | 16.2% | 27 | 32 | 87.5% | 79.9% |
| | California | 21.1% | 17.8% | 26 | 29 | 88.9% | 82.0% |
| r0_3 | East | 33.2% | 30.4% | 17 | 19 | 96.1% | 93.9% |
| | Southeast | 33.2% | 30.3% | 17 | 19 | 96.2% | 93.8% |
| | Midwest | 31.9% | 30.3% | 18 | 19 | 95.2% | 93.8% |
| | Southwest | 32.9% | 31.2% | 17 | 18 | 95.6% | 94.2% |
| | Northwest | 33.7% | 30.9% | 17 | 18 | 96.3% | 94.1% |
| | California | 35.1% | 32.1% | 16 | 17 | 96.7% | 94.6% |
| eps_0.25 ($\eta$ = 0.75) | East | 9.6% | 6.3% | 42 | 55 | 72.8% | 57.2% |
| | Southeast | 9.7% | 6.2% | 42 | 56 | 73.0% | 56.5% |
| | Midwest | 7.9% | 6.1% | 49 | 56 | 66.5% | 56.4% |
| | Southwest | 9.2% | 7.2% | 44 | 51 | 69.9% | 60.2% |
| | Northwest | 10.3% | 6.9% | 41 | 52 | 74.2% | 58.8% |
| | California | 12.1% | 8.4% | 37 | 47 | 77.2% | 63.5% |
| eps_0.5 ($\eta$ = 0.50) | East | 8.2% | 6.3% | 49 | 55 | 70.7% | 57.2% |
| | Southeast | 8.2% | 6.2% | 49 | 56 | 70.9% | 56.6% |
| | Midwest | 7.3% | 6.1% | 53 | 56 | 64.9% | 56.4% |
| | Southwest | 8.5% | 7.2% | 48 | 51 | 68.5% | 60.2% |
| | Northwest | 9.0% | 6.9% | 47 | 52 | 72.4% | 58.9% |
| | California | 10.7% | 8.5% | 42 | 47 | 75.8% | 63.5% |
| eps_0.6667 ($\eta$ = 0.33) | East | 8.0% | 6.3% | 51 | 55 | 69.5% | 57.2% |
| | Southeast | 7.9% | 6.2% | 52 | 56 | 69.6% | 56.6% |
| | Midwest | 7.1% | 6.1% | 54 | 56 | 64.1% | 56.4% |
| | Southwest | 8.3% | 7.2% | 49 | 51 | 67.8% | 60.2% |
| | Northwest | 8.7% | 6.9% | 49 | 52 | 71.2% | 59.0% |
| | California | 10.4% | 8.5% | 44 | 47 | 74.9% | 63.6% |
| eps_0.75 ($\eta$ = 0.25) | East | 7.9% | 6.3% | 52 | 55 | 69.0% | 57.2% |
| | Southeast | 7.8% | 6.2% | 52 | 56 | 69.0% | 56.6% |
| | Midwest | 7.0% | 6.1% | 54 | 56 | 63.7% | 56.4% |
| | Southwest | 8.3% | 7.2% | 49 | 51 | 67.5% | 60.2% |
| | Northwest | 8.6% | 6.9% | 50 | 52 | 70.7% | 59.0% |
| | California | 10.3% | 8.5% | 44 | 47 | 74.5% | 63.6% |
| eps_1 ($\eta$ = 0.00) | East | 7.6% | 6.3% | 53 | 55 | 67.5% | 57.2% |
| | Southeast | 7.5% | 6.2% | 54 | 56 | 67.5% | 56.6% |
| | Midwest | 6.9% | 6.1% | 55 | 56 | 62.7% | 56.5% |
| | Southwest | 8.1% | 7.2% | 50 | 51 | 66.6% | 60.2% |
| | Northwest | 8.3% | 6.9% | 51 | 52 | 69.4% | 59.0% |
| | California | 10.1% | 8.5% | 45 | 47 | 73.4% | 63.6% |
| sar_0.2 | East | 7.7% | 6.0% | 51 | 57 | 67.4% | 56.2% |
| | Southeast | 7.6% | 5.9% | 51 | 58 | 67.2% | 55.6% |
| | Midwest | 7.0% | 5.9% | 54 | 57 | 63.2% | 55.7% |
| | Southwest | 7.7% | 6.7% | 51 | 54 | 65.5% | 58.6% |
| | Northwest | 8.0% | 6.5% | 50 | 55 | 67.4% | 57.7% |
| | California | 8.9% | 7.3% | 48 | 51 | 69.6% | 60.8% |
| sar_0.3 | East | 8.4% | 6.2% | 48 | 56 | 70.2% | 56.7% |
| | Southeast | 8.3% | 6.0% | 48 | 57 | 70.2% | 56.1% |
| | Midwest | 7.3% | 6.0% | 52 | 57 | 64.8% | 56.1% |
| | Southwest | 8.4% | 7.0% | 48 | 53 | 67.8% | 59.5% |
| | Northwest | 8.9% | 6.7% | 47 | 54 | 71.0% | 58.4% |
| | California | 10.2% | 7.9% | 43 | 49 | 73.9% | 62.3% |
| sar_0.5 | East | 9.4% | 6.5% | 42 | 54 | 73.3% | 57.5% |
| | Southeast | 9.4% | 6.3% | 41 | 55 | 73.7% | 56.9% |
| | Midwest | 7.8% | 6.2% | 49 | 55 | 66.8% | 56.6% |
| | Southwest | 9.3% | 7.5% | 44 | 50 | 70.6% | 60.7% |
| | Northwest | 10.3% | 7.1% | 40 | 51 | 75.3% | 59.3% |
| | California | 12.4% | 8.9% | 35 | 44 | 78.6% | 64.3% |
| sar_0.6 | East | 9.8% | 6.6% | 39 | 53 | 74.2% | 57.8% |
| | Southeast | 9.8% | 6.4% | 39 | 54 | 74.6% | 57.1% |
| | Midwest | 7.9% | 6.3% | 47 | 55 | 67.4% | 56.8% |
| | Southwest | 9.6% | 7.7% | 42 | 49 | 71.3% | 61.1% |
| | Northwest | 10.9% | 7.2% | 37 | 50 | 76.4% | 59.5% |
| | California | 13.3% | 9.3% | 32 | 42 | 79.8% | 65.0% |
| fold_1 | East | 8.7% | 6.3% | 46 | 55 | 71.5% | 57.0% |
| | Southeast | 8.7% | 6.1% | 46 | 56 | 71.7% | 56.4% |
| | Midwest | 7.5% | 6.1% | 51 | 56 | 65.5% | 56.3% |
| | Southwest | 8.7% | 7.1% | 47 | 52 | 68.9% | 60.0% |
| | Northwest | 9.4% | 6.8% | 44 | 53 | 73.0% | 58.7% |
| | California | 11.0% | 8.3% | 40 | 47 | 76.0% | 63.1% |
| fold_3 | East | 9.1% | 6.4% | 44 | 55 | 72.3% | 57.3% |
| | Southeast | 9.1% | 6.2% | 44 | 55 | 72.6% | 56.6% |
| | Midwest | 7.6% | 6.2% | 50 | 56 | 66.2% | 56.5% |
| | Southwest | 9.0% | 7.3% | 45 | 51 | 69.7% | 60.3% |
| | Northwest | 9.8% | 7.0% | 43 | 52 | 73.9% | 59.0% |
| | California | 11.6% | 8.6% | 38 | 46 | 77.1% | 63.7% |

**Table S4.** Differential metrics between agricultural workers and the general community across all parameter sets and regions. Peak prevalence difference (A minus C), attack rate difference (A minus C), time to peak difference (A minus C, in days; negative values indicate agricultural workers peak earlier), peak prevalence ratio (A/C), attack rate ratio (A/C), and maximum relative infection rate (the highest instantaneous ratio of agricultural worker to community infection prevalence observed during the simulation).

| Parameter set | Region | Peak prev. diff. | Attack rate diff. | Time diff. | Peak prev. ratio | Attack rate ratio | Max rel. infection |
|:---|:---|:---:|:---:|:---:|:---:|:---:|:---:|
| **r0_1.5 (baseline)** | East | 2.6 pp | 14.9 pp | −10 | 1.41 | 1.26 | 2.62 |
| | Southeast | 2.8 pp | 15.8 pp | −12 | 1.45 | 1.28 | 2.78 |
| | Midwest | 1.4 pp | 9.5 pp | −6 | 1.24 | 1.17 | 1.74 |
| | Southwest | 1.6 pp | 9.2 pp | −5 | 1.23 | 1.15 | 1.75 |
| | Northwest | 2.8 pp | 14.7 pp | −9 | 1.40 | 1.25 | 2.52 |
| | California | 3.0 pp | 13.3 pp | −8 | 1.35 | 1.21 | 2.38 |
| r0_1.2 | East | 1.5 pp | 20.3 pp | −28 | 2.05 | 1.69 | 3.41 |
| | Southeast | 1.6 pp | 21.5 pp | −29 | 2.18 | 1.76 | 3.69 |
| | Midwest | 0.7 pp | 12.0 pp | −15 | 1.53 | 1.42 | 2.00 |
| | Southwest | 0.9 pp | 12.5 pp | −12 | 1.46 | 1.36 | 2.00 |
| | Northwest | 1.6 pp | 20.0 pp | −22 | 1.89 | 1.61 | 3.12 |
| | California | 1.9 pp | 18.8 pp | −17 | 1.68 | 1.46 | 2.83 |
| r0_2 | East | 3.1 pp | 7.6 pp | −5 | 1.20 | 1.10 | 2.09 |
| | Southeast | 3.3 pp | 8.0 pp | −5 | 1.21 | 1.10 | 2.19 |
| | Midwest | 1.8 pp | 5.0 pp | −2 | 1.12 | 1.06 | 1.52 |
| | Southwest | 1.9 pp | 4.7 pp | −2 | 1.12 | 1.06 | 1.55 |
| | Northwest | 3.2 pp | 7.6 pp | −5 | 1.20 | 1.10 | 2.07 |
| | California | 3.3 pp | 6.9 pp | −3 | 1.19 | 1.08 | 2.01 |
| r0_3 | East | 2.8 pp | 2.2 pp | −2 | 1.09 | 1.02 | 1.67 |
| | Southeast | 2.9 pp | 2.3 pp | −2 | 1.09 | 1.02 | 1.73 |
| | Midwest | 1.6 pp | 1.4 pp | −1 | 1.05 | 1.02 | 1.34 |
| | Southwest | 1.7 pp | 1.4 pp | −1 | 1.05 | 1.01 | 1.37 |
| | Northwest | 2.8 pp | 2.2 pp | −1 | 1.09 | 1.02 | 1.68 |
| | California | 3.0 pp | 2.0 pp | −1 | 1.09 | 1.02 | 1.67 |
| eps_0.25 ($\eta$ = 0.75) | East | 3.3 pp | 15.6 pp | −13 | 1.52 | 1.27 | 3.14 |
| | Southeast | 3.5 pp | 16.5 pp | −14 | 1.57 | 1.29 | 3.37 |
| | Midwest | 1.8 pp | 10.1 pp | −7 | 1.29 | 1.18 | 1.95 |
| | Southwest | 2.0 pp | 9.7 pp | −7 | 1.27 | 1.16 | 1.96 |
| | Northwest | 3.4 pp | 15.4 pp | −11 | 1.50 | 1.26 | 3.00 |
| | California | 3.6 pp | 13.8 pp | −10 | 1.43 | 1.22 | 2.78 |
| eps_0.5 ($\eta$ = 0.50) | East | 1.9 pp | 13.5 pp | −6 | 1.30 | 1.24 | 1.99 |
| | Southeast | 2.0 pp | 14.3 pp | −7 | 1.32 | 1.25 | 2.08 |
| | Midwest | 1.1 pp | 8.5 pp | −3 | 1.18 | 1.15 | 1.48 |
| | Southwest | 1.3 pp | 8.4 pp | −3 | 1.18 | 1.14 | 1.50 |
| | Northwest | 2.1 pp | 13.4 pp | −5 | 1.30 | 1.23 | 1.94 |
| | California | 2.3 pp | 12.3 pp | −5 | 1.27 | 1.19 | 1.88 |
| eps_0.6667 ($\eta$ = 0.33) | East | 1.6 pp | 12.3 pp | −4 | 1.26 | 1.22 | 1.67 |
| | Southeast | 1.7 pp | 13.0 pp | −4 | 1.28 | 1.23 | 1.73 |
| | Midwest | 1.0 pp | 7.6 pp | −2 | 1.16 | 1.14 | 1.35 |
| | Southwest | 1.1 pp | 7.6 pp | −2 | 1.15 | 1.13 | 1.36 |
| | Northwest | 1.8 pp | 12.3 pp | −3 | 1.26 | 1.21 | 1.65 |
| | California | 1.9 pp | 11.4 pp | −3 | 1.23 | 1.18 | 1.61 |
| eps_0.75 ($\eta$ = 0.25) | East | 1.5 pp | 11.8 pp | −3 | 1.24 | 1.21 | 1.58 |
| | Southeast | 1.6 pp | 12.5 pp | −4 | 1.26 | 1.22 | 1.62 |
| | Midwest | 0.9 pp | 7.2 pp | −2 | 1.15 | 1.13 | 1.31 |
| | Southwest | 1.0 pp | 7.3 pp | −2 | 1.14 | 1.12 | 1.32 |
| | Northwest | 1.7 pp | 11.8 pp | −2 | 1.24 | 1.20 | 1.55 |
| | California | 1.8 pp | 11.0 pp | −3 | 1.22 | 1.17 | 1.53 |
| eps_1 ($\eta$ = 0.00) | East | 1.3 pp | 10.3 pp | −2 | 1.21 | 1.18 | 1.40 |
| | Southeast | 1.4 pp | 10.9 pp | −2 | 1.22 | 1.19 | 1.42 |
| | Midwest | 0.8 pp | 6.3 pp | −1 | 1.12 | 1.11 | 1.22 |
| | Southwest | 0.9 pp | 6.4 pp | −1 | 1.12 | 1.11 | 1.23 |
| | Northwest | 1.4 pp | 10.4 pp | −1 | 1.21 | 1.18 | 1.39 |
| | California | 1.6 pp | 9.8 pp | −2 | 1.19 | 1.15 | 1.37 |
| sar_0.2 | East | 1.7 pp | 11.3 pp | −6 | 1.29 | 1.20 | 1.82 |
| | Southeast | 1.7 pp | 11.6 pp | −7 | 1.30 | 1.21 | 1.85 |
| | Midwest | 1.1 pp | 7.5 pp | −3 | 1.18 | 1.13 | 1.45 |
| | Southwest | 1.1 pp | 6.9 pp | −3 | 1.16 | 1.12 | 1.41 |
| | Northwest | 1.5 pp | 9.7 pp | −5 | 1.23 | 1.17 | 1.60 |
| | California | 1.5 pp | 8.8 pp | −3 | 1.21 | 1.15 | 1.55 |
| sar_0.3 | East | 2.2 pp | 13.4 pp | −8 | 1.36 | 1.24 | 2.19 |
| | Southeast | 2.3 pp | 14.1 pp | −9 | 1.38 | 1.25 | 2.28 |
| | Midwest | 1.3 pp | 8.7 pp | −5 | 1.21 | 1.15 | 1.59 |
| | Southwest | 1.4 pp | 8.3 pp | −5 | 1.20 | 1.14 | 1.58 |
| | Northwest | 2.2 pp | 12.7 pp | −7 | 1.33 | 1.22 | 2.02 |
| | California | 2.3 pp | 11.6 pp | −6 | 1.29 | 1.19 | 1.94 |
| sar_0.5 | East | 2.9 pp | 15.8 pp | −12 | 1.45 | 1.27 | 3.07 |
| | Southeast | 3.1 pp | 16.8 pp | −14 | 1.50 | 1.30 | 3.32 |
| | Midwest | 1.6 pp | 10.1 pp | −6 | 1.25 | 1.18 | 1.88 |
| | Southwest | 1.8 pp | 9.9 pp | −6 | 1.24 | 1.16 | 1.93 |
| | Northwest | 3.3 pp | 16.0 pp | −11 | 1.46 | 1.27 | 3.08 |
| | California | 3.5 pp | 14.3 pp | −9 | 1.40 | 1.22 | 2.84 |
| sar_0.6 | East | 3.2 pp | 16.4 pp | −14 | 1.48 | 1.28 | 3.52 |
| | Southeast | 3.4 pp | 17.5 pp | −15 | 1.54 | 1.31 | 3.87 |
| | Midwest | 1.6 pp | 10.6 pp | −8 | 1.26 | 1.19 | 2.02 |
| | Southwest | 1.9 pp | 10.2 pp | −7 | 1.25 | 1.17 | 2.10 |
| | Northwest | 3.7 pp | 16.9 pp | −13 | 1.51 | 1.28 | 3.68 |
| | California | 4.0 pp | 14.8 pp | −10 | 1.43 | 1.23 | 3.31 |
| fold_1 | East | 2.5 pp | 14.5 pp | −9 | 1.39 | 1.25 | 2.46 |
| | Southeast | 2.6 pp | 15.3 pp | −10 | 1.43 | 1.27 | 2.60 |
| | Midwest | 1.4 pp | 9.2 pp | −5 | 1.23 | 1.16 | 1.68 |
| | Southwest | 1.5 pp | 9.0 pp | −5 | 1.22 | 1.15 | 1.69 |
| | Northwest | 2.6 pp | 14.3 pp | −9 | 1.38 | 1.24 | 2.38 |
| | California | 2.8 pp | 12.9 pp | −7 | 1.34 | 1.21 | 2.27 |
| fold_3 | East | 2.7 pp | 15.1 pp | −11 | 1.42 | 1.26 | 2.70 |
| | Southeast | 2.9 pp | 16.0 pp | −11 | 1.46 | 1.28 | 2.87 |
| | Midwest | 1.5 pp | 9.7 pp | −6 | 1.24 | 1.17 | 1.77 |
| | Southwest | 1.7 pp | 9.4 pp | −6 | 1.23 | 1.16 | 1.78 |
| | Northwest | 2.8 pp | 14.9 pp | −9 | 1.41 | 1.25 | 2.59 |
| | California | 3.1 pp | 13.4 pp | −8 | 1.36 | 1.21 | 2.42 |