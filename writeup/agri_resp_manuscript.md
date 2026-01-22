# Modeling the impact of respiratory illness outbreaks on the agricultural workforce and food production in the United States

## Abstract

**Background:** Respiratory disease outbreaks pose significant threats to critical infrastructure, including food production systems. Agricultural workers face elevated disease transmission risk due to crowded living conditions and unique occupational exposures, yet the impact of respiratory outbreaks on agricultural productivity remains poorly quantified.

**Methods:** We developed a household-structured susceptible-infectious-recovered (SIR) transmission model to compare disease dynamics between agricultural workers and the general U.S. population. Using data from the American Community Survey and National Agricultural Workers Survey, we parameterized household size and crowding distributions across regions. We simulated outbreaks with reproduction numbers ranging from 1.1 to 3.0 and secondary attack rates of 15-30% in uncrowded households and 30-60% in crowded households. We assessed productivity losses for three labor-intensive crops (oranges, lettuce, strawberries) with different harvest seasonalities.

**Results:** Agricultural worker households exhibited substantially higher crowding rates (X% vs Y% in the general population) despite similar mean household sizes. At a baseline reproduction number of 1.2, disease prevalence peaked Z% higher among agricultural workers compared to the general population, with cumulative attack rates exceeding the general population by W percentage points. Under baseline assumptions, productivity losses during peak harvest periods reached up to [X]% for strawberries, [Y]% for lettuce, and [Z]% for oranges, with regional variation reflecting both disease dynamics and crop seasonality.

**Conclusions:** Household crowding creates disproportionate disease burden among agricultural workers, leading to substantial harvest-season productivity losses. These findings highlight the need for targeted outbreak preparedness and mitigation strategies in the agricultural sector to maintain food system resilience.

## Introduction

Respiratory disease outbreaks represent major disruptions to global trade, economic activity, and critical societal functions. The COVID-19 pandemic starkly illustrated how infectious disease can cascade through interconnected systems, affecting not only public health but also supply chains, labor availability, and food security. Agriculture, as a fundamental pillar of societal functioning, has proven particularly vulnerable to disease-related disruptions. Recent outbreaks—including dengue fever in agricultural regions and COVID-19 across food production systems—have demonstrated the sector's susceptibility to infectious disease impacts, particularly during labor-intensive operations like harvesting.

Agricultural workers constitute a unique population with distinct epidemiological characteristics. Studies consistently document higher incidence rates of various infectious diseases among agricultural workers compared to the general population, attributed to factors including limited healthcare access, high occupational exposure risks, and frequent migration patterns. Living conditions, particularly household crowding, further amplify transmission risk. The seasonal and migratory nature of agricultural labor means that disease impacts may vary substantially across regions and crop cycles, creating temporal vulnerabilities in food production.

Despite documented impacts of disease on agricultural systems, critical knowledge gaps persist. We lack predictive frameworks for anticipating how novel respiratory outbreaks might differentially affect agricultural workers before outbreaks occur—information essential for preparedness planning and targeted mitigation strategies. Existing surveillance data are often insufficient to characterize agricultural worker populations at the resolution needed for disease modeling. Furthermore, analytical frameworks for translating epidemiological dynamics into agricultural productivity impacts remain underdeveloped.

This study addresses these gaps by developing a disease transmission model specifically designed to anticipate differential impacts of respiratory virus outbreaks on agricultural workers relative to the general U.S. population. Focusing on household size and crowding—two well-established predictors of respiratory disease transmission—we quantify the relative rate of infections and assess potential impacts on harvesting operations. We apply this framework to three economically important, labor-intensive crops with different harvest seasonalities: oranges, lettuce, and strawberries. Our analysis provides both a quantitative assessment of how outbreaks with varying characteristics might impact the agricultural sector and a generalizable framework for future assessments of disease impacts on agricultural production.

## Methods

### Data sources and processing

#### Population and household characteristics

We obtained county-level data on population size, household size distribution, proportion of crowded households, and proportion of agricultural workers from the U.S. Census Bureau's 2022 American Community Survey (ACS) 5-year estimates. Following standard definitions, we classified households as crowded when they contained more than one individual per room (excluding bathrooms, kitchens, and hallways). Agricultural workers were defined as individuals employed in "farming, fishing, and forestry occupations" (ACS occupation codes).

To enable regional-level analysis, we aggregated county-level data using population-weighted averages. Regions were defined based on [USDA agricultural regions / state groupings]. For computational tractability and data availability considerations, we combined households of size 7 or greater into a single category ("7+") and treated them as households of size 7 in our model. These large households represent approximately [X]% of total households in the ACS data.

The ACS reports household size distribution and crowding proportion separately rather than jointly. Our transmission model requires the fraction of households of each size that are crowded. To assign crowding levels by household size, we employed the following relationship:

$$p_{\text{crowded}}(n) = \begin{cases} 0 & n = 1 \\ \frac{c \cdot (n-1)}{N} & n \geq 2 \end{cases}$$

where $p_{\text{crowded}}(n)$ is the probability that a household of size $n$ is crowded, $c$ is a scaling constant, and $N$ is a normalization factor. By definition, single-person households cannot be crowded under the Census definition (>1 person per room). This formulation ensures that the overall fraction of crowded households matches the ACS-reported value while allowing crowding probability to increase linearly with household size. The parameter $c$ effectively represents how much more likely a household of size 7+ is to be crowded compared to a household of size 2.

We calculated $c$ by solving:

$$\sum_{n=1}^{7+} f(n) \cdot p_{\text{crowded}}(n) = P_{\text{crowded,overall}}$$

where $f(n)$ is the fraction of households of size $n$ and $P_{\text{crowded,overall}}$ is the overall crowding proportion from ACS data.

#### Agricultural worker household characteristics

For agricultural workers specifically, we obtained regional household size distributions and crowding proportions from the 2018-2022 National Agricultural Workers Survey (NAWS), a nationally representative survey of U.S. crop workers conducted by the Department of Labor. For each region, we calculated weighted proportions of households of size 1, 2, 3, 4, 5, 6, and 7+, along with the weighted proportion of crowded households (>1 person per room). We applied the same crowding-by-household-size assignment procedure described above to agricultural worker households.

#### Crop harvest calendars and labor requirements

We obtained crop harvest calendar data for oranges, lettuce, and strawberries from the USDA National Agricultural Statistics Service and state agricultural extension services. For each crop and major producing region, we identified peak harvest months and the duration of harvest seasons. We focused analysis on California's Central Valley for detailed case studies, as this region accounts for substantial U.S. production of all three crops and exhibits clear harvest seasonality differences: oranges (winter/spring), lettuce (year-round with spring/fall peaks), and strawberries (spring/summer).

Labor intensity data—the number of worker-hours required per acre during harvest—were obtained from agricultural economics studies and extension publications for each crop.

### Disease transmission model

We simulated respiratory disease spread using a deterministic household-structured susceptible-infectious-recovered (SIR) model based on the framework developed by House and Keeling (2008). The model explicitly accounts for both within-household and between-household transmission, incorporating household size and crowding status as key structural parameters.

#### Model structure

The population is divided into compartments based on infection status (susceptible, infectious, or recovered), household size ($n = 1, 2, ..., 7+$), crowding status (crowded or uncrowded), and occupational group (agricultural worker or general population). Within each household type, we track the number of households with $i$ infectious individuals out of $n$ total members.

Transmission occurs through two routes:

1. **Within-household transmission:** Susceptible individuals become infected through contact with infectious household members at rate $\beta_W$, modified by crowding status
2. **Between-household transmission:** Susceptible individuals become infected through community contacts at rate $\beta_B$, proportional to the overall infectious prevalence

The force of infection on a susceptible individual in a household with $i$ infectious members is:

$$\lambda = \beta_W(c) \cdot i + \beta_B \cdot I_{\text{total}}$$

where $\beta_W(c)$ is the within-household transmission rate (dependent on crowding status $c$), $\beta_B$ is the between-household transmission rate, and $I_{\text{total}}$ is the total prevalence of infectious individuals in the population.

Infectious individuals recover at rate $\gamma = 1/D$, where $D$ is the mean infectious period.

Full mathematical details including the system of differential equations are provided in Supplementary Information.

#### Model parameterization

**Recovery rate:** We fixed the recovery rate at $\gamma = 1/5$ day$^{-1}$, corresponding to a mean infectious period of 5 days, consistent with influenza and mild-to-moderate COVID-19 infections.

**Within-household transmission:** We parameterized within-household transmission using secondary attack rate (SAR) estimates from household transmission studies. The SAR represents the probability that a susceptible household member becomes infected given at least one infectious household member. For a household-structured model, the expected SAR relates to the within-household transmission rate through:

$\text{SAR} = 1 - e^{-\tau / \gamma}$

where $\tau$ is the within-household transmission rate per susceptible-infected contact pair. For the baseline analysis, we assumed SAR = 20% for uncrowded households and SAR = 40% for crowded households, consistent with meta-analyses of influenza household transmission and evidence that crowding approximately doubles transmission risk. This yields $\tau = 0.05$ day$^{-1}$ for uncrowded households and $\tau = 0.133$ day$^{-1}$ for crowded households. We implemented this as $\tau = \tau_{\text{base}} + \tau_{\text{boost}} \times c$, where $c \in \{0,1\}$ indicates crowding status, $\tau_{\text{base}} = 0.05$ day$^{-1}$, and $\tau_{\text{boost}} = 0.083$ day$^{-1}$.

**Between-household transmission:** With $\gamma$ and $\tau$ fixed, we calibrated the between-household transmission rate $\beta$ to achieve a specified basic reproduction number $R_0$. We performed calibration at the national level using population-weighted average household distributions from ACS data. For each candidate $\beta$ value, we ran the model to equilibrium and compared the final attack rate to the theoretical prediction from the implicit relationship $R_\infty = 1 - \exp(-R_0 R_\infty)$. For the baseline analysis, we targeted $R_0 = 1.2$, reflecting estimated reproduction numbers for seasonal influenza and effective reproduction numbers during many COVID-19 surge periods, which required $\beta = 0.153$ day$^{-1}$ and yields a final attack rate of approximately 31%. Sensitivity analyses considered $R_0$ values of 1.2, 1.5, 2.0, and 3.0, corresponding to $\beta$ values of 0.153, 0.210, 0.306, and 0.504 day$^{-1}$ and final attack rates of 31%, 58%, 80%, and 94%, respectively.

**Population mixing:** Agricultural workers and the general population likely exhibit some degree of assortative mixing—preferential contact within their own group. We modeled this using a mixing matrix where the force of infection for population $k$ is:

$\lambda_k = \beta_k \sum_j m_{kj} I_j$

where $I_j$ is the prevalence in population $j$, and the mixing matrix elements are:

$m_{kk} = (1-\epsilon) + \epsilon w_k$
$m_{kj} = \epsilon w_j \text{ for } k \neq j$

with $w_k$ being the population fraction in group $k$, and $\epsilon \in [0,1]$ controlling the degree of assortative mixing. When $\epsilon = 0$, mixing is completely assortative (no between-group contacts); when $\epsilon = 1$, mixing is proportional to population size. For the baseline analysis, we used $\epsilon = 0.33$, representing moderate assortativity where agricultural workers have preferential within-group contact but still interact with the general population.

#### Model initialization and simulation

For regional models, we initialized the population using household size distributions and crowding proportions from ACS (general population) and NAWS (agricultural workers). We introduced infection by setting 0.1% of individuals in each population to the infectious state. This was implemented by identifying a fraction of households containing one infected member, calculated as 0.1% of the total population divided by household size, ensuring that the initial prevalence matched the target value.

We simulated outbreaks over 365 days for regional models and 100 days for county-level models using the `odin` package in R for numerical integration. For each simulation, we recorded the prevalence over time and calculated summary statistics including peak prevalence, time to peak, and final attack rate (cumulative proportion infected) for both agricultural workers and the general population.

For county-level simulations, we repeated this process for all counties in the contiguous United States to assess geographic variation in differential disease impact. We used parallel processing to handle the computational demands of running thousands of county-level simulations.

### Productivity impact assessment

To translate disease dynamics into agricultural productivity impacts, we estimated the number of workers unable to perform harvest labor at each time point due to symptomatic illness. We assumed that symptoms begin one day after infection and last for three days (days 2-4 of the 5-day infectious period), during which workers cannot perform agricultural labor. For each day $t$, we calculated the proportion of agricultural workers experiencing symptoms by summing new infections from days $t-3$ through $t-1$ (accounting for the one-day lag to symptom onset and three-day symptomatic period).

We obtained weekly crop movement data for California strawberries, lettuce, and oranges from the USDA Agricultural Marketing Service, which serves as a proxy for harvest timing and volume. We averaged movements across multiple years (2018-2024) to create representative seasonal patterns, then converted to daily estimates by dividing weekly volumes equally across days.

For each crop and outbreak timing scenario, we calculated productivity losses as follows. Let $L(t)$ denote the labor availability on day $t$ (where $L(t) = 1 - s(t)$ and $s(t)$ is the proportion of agricultural workers symptomatic), and let $H(t)$ denote the harvest volume on day $t$ under normal conditions. The realized harvest on day $t$ is:

$H_{\text{actual}}(t) = L(t) \times H(t)$

We simulated outbreaks with peak timing varying across all 365 days of the year. For each outbreak timing, we aligned the epidemic curve (indexed by days since peak in the general community) with the crop calendar, ensuring circular wrapping so that epidemic impacts extending beyond day 365 affect the beginning of the calendar year. The total seasonal productivity loss for crop $k$ with outbreak peak on day $p$ is:

$\text{Loss}_k(p) = 1 - \frac{\sum_{t=1}^{365} H_{\text{actual},k}(t \mid p)}{\sum_{t=1}^{365} H_k(t)}$

This approach provides a first-order estimate of harvest disruption while acknowledging that actual impacts may be mitigated through labor reallocation or exacerbated by compounding factors such as crop perishability and constraints on re-scheduling harvest operations.

## Results

### Crowding is more frequent among agricultural worker households

Analysis of ACS and NAWS data revealed substantial differences in household crowding between agricultural workers and the general U.S. population (Figure 1). While mean household size was similar between groups (agricultural workers: [X.X] persons; general population: [X.X] persons), the distribution of household sizes differed notably. Households with 4 or more individuals were substantially more common among agricultural workers ([X]% vs [Y]% in the general population), with households of 5-7+ members approximately [Z]-fold more prevalent.

Crowding rates differed even more dramatically. Among agricultural worker households, [X]% were classified as crowded (>1 person per room), compared to [Y]% of general population households—a [Z]-fold difference. This disparity persisted across regions, with agricultural worker crowding rates ranging from [X]% to [Y]% compared to [A]% to [B]% in the general population.

The combination of larger household sizes and higher crowding rates creates conditions highly conducive to respiratory disease transmission within agricultural worker communities.

[**Figure 1.** Household size distribution and crowding rates for agricultural workers and general population by region. Panel A: Distribution of household sizes. Panel B: Proportion of crowded households by region.]

### Household crowding leads to higher disease prevalence among agricultural workers

Simulations of respiratory disease outbreaks at the regional level revealed consistently higher disease burden among agricultural workers compared to the general population (Figure 2). Under baseline assumptions ($R_0 = 1.2$, SAR = 15%/30% for uncrowded/crowded households, $\epsilon = 0.3$), peak prevalence among agricultural workers exceeded that of the general population by [X]% to [Y]% across regions (mean: [Z]%).

The cumulative attack rate—the total proportion of each population infected over the entire outbreak—showed similar disparities. Agricultural workers experienced attack rates of [X]% to [Y]% across regions, compared to [A]% to [B]% in the general population, representing a [C] to [D] percentage point excess burden.

These differences were sensitive to the basic reproduction number (Figure 3). At $R_0 = 1.1$, the differential between agricultural workers and the general population was modest ([X] percentage points). However, at $R_0 = 2.0$, this differential increased to [Y] percentage points, and at $R_0 = 3.0$, it reached [Z] percentage points. Higher transmission potential amplified the impact of household crowding, as more generations of within-household transmission occurred.

Sensitivity to the secondary attack rate in crowded households showed a similar pattern (Supplementary Figure S1). Increasing the SAR in crowded households from 30% to 60% resulted in [X] to [Y] additional percentage points of attack rate among agricultural workers, with minimal change in the general population.

County-level simulations demonstrated geographic heterogeneity in these differentials (Figure 4), reflecting variation in both agricultural worker population proportions and crowding rates. Counties with high agricultural worker populations and elevated crowding showed the largest disparities, with some counties experiencing agricultural worker attack rates [X]+ percentage points above their local general population.

[**Figure 2.** Disease dynamics in agricultural workers versus general population. Panel A: Prevalence over time for a representative region. Panel B: Peak prevalence by region. Panel C: Final attack rate by region.]

[**Figure 3.** Sensitivity of attack rate differential (agricultural workers minus general population) to basic reproduction number. Lines represent different regions.]

[**Figure 4.** County-level map of differential attack rate (agricultural workers minus general population) under baseline assumptions.]

### Respiratory disease outbreaks among agricultural workers can lead to substantial productivity losses

Translating disease dynamics into harvest productivity impacts revealed substantial potential losses, with magnitude depending on outbreak timing relative to harvest seasons (Figure 5). Under baseline assumptions, we estimated the following productivity losses during peak harvest periods:

**Strawberries (spring/summer harvest):** Peak-season productivity losses ranged from [X]% to [Y]% across California counties, with losses concentrated in May through July. An outbreak peaking in late spring would coincide with maximum harvest labor demand, potentially reducing harvest by [Z]% during critical weeks.

**Lettuce (year-round with spring/fall peaks):** The extended harvest season distributed risk across the year. Spring outbreaks produced losses of [X]% to [Y]% during March-May, while fall outbreaks generated [A]% to [B]% losses during October-November. The year-round production partially buffered annual losses but did not eliminate substantial seasonal impacts.

**Oranges (winter/spring harvest):** Outbreaks timed to the January-April harvest window produced losses of [X]% to [Y]%. Later-season outbreaks (outbreak peak in February-March) aligned more closely with peak harvest labor demand, amplifying impacts.

Aggregating across the three crops and assuming outbreak timing uniformly distributed across the year, expected annual productivity losses were [X]% for strawberries, [Y]% for lettuce, and [Z]% for oranges in the Central Valley region.

These estimates are conservative in several respects: they do not account for labor reallocation difficulties when multiple crops are harvested simultaneously, they assume no additional productivity loss from workers who have recovered but may experience lingering effects, and they do not consider cascading effects on processing, distribution, or market prices.

[**Figure 5.** Estimated harvest productivity losses by month for three crops in California's Central Valley. Lines represent losses under different outbreak start dates; shaded regions indicate harvest seasons.]

### Sensitivity analyses

We conducted sensitivity analyses to assess how key model assumptions affected our results:

**Reproduction number (R₀):** We varied R₀ from 1.2 (baseline) to 1.5, 2.0, and 3.0 by adjusting the between-household transmission rate β. Higher R₀ values increase both absolute disease burden and the differential between agricultural workers and the general population.

**Assortativity (ε):** We examined mixing patterns ranging from complete assortativity (ε = 0, agricultural workers only contact other agricultural workers) to baseline moderate assortativity (ε = 0.33) to stronger proportional mixing (ε = 0.5 and 0.7). Higher ε values reduce the concentration of infection within the agricultural worker population by increasing their contact with the lower-risk general population.

**Secondary attack rate in crowded households:** We varied the SAR in crowded households from 30% to 60% (baseline 40%) to encompass the range observed across different respiratory pathogens and household studies. This directly affects the within-household transmission rate τ_crowded.

**Crowding fold difference:** We tested how the relationship between household size and crowding probability affects results by varying the fold difference parameter from 1 (all household sizes ≥2 equally likely to be crowded) to 3 (size-7 households 3× more likely to be crowded than size-2 households), with baseline at 2.

**County-level NAWS adjustment approaches:** For county-level simulations, we tested multiple approaches to assign household characteristics to agricultural workers:

1. **Regional NAWS only (baseline):** Use regional NAWS data without county-level adjustment
2. **Proportional adjustment:** Adjust NAWS distributions proportionally to county-level deviations from regional ACS means
3. **No adjustment (ACS only):** Use county-level ACS data for both populations (conservative, as it minimizes the agricultural worker-general population differential)

These approaches address uncertainty in how agricultural worker household characteristics vary at the county level given limited data availability.

## Discussion

This study provides the first quantitative framework for anticipating how respiratory disease outbreaks may differentially impact agricultural workers and, consequently, agricultural productivity in the United States. Our findings demonstrate that household crowding—substantially more prevalent among agricultural workers than the general population—creates conditions for heightened disease transmission, leading to disproportionate disease burden and meaningful harvest-season productivity losses.

### Principal findings and interpretation

Three main findings emerge from our analysis. First, agricultural worker households exhibit markedly higher crowding rates ([X]% vs [Y]% in the general population) despite similar mean household sizes. This structural difference in living conditions translates directly into epidemiological vulnerability. Second, respiratory disease outbreaks produce consistently higher attack rates among agricultural workers, with the differential increasing as outbreak severity (measured by $R_0$) increases. Under moderate transmission conditions ($R_0 = 1.2$), agricultural workers experienced attack rates [Z] percentage points higher than the general population; under more severe conditions ($R_0 = 3.0$), this differential exceeded [W] percentage points. Third, these epidemiological impacts translate into tangible agricultural productivity losses, with harvest-season losses of [X-Y]% for labor-intensive crops when outbreak timing aligns with peak harvest periods.

These findings align with observed patterns during the COVID-19 pandemic, when agricultural workers experienced disproportionate infection rates and agricultural operations faced significant labor shortages. Our model provides a predictive framework for anticipating such impacts in future outbreaks, enabling proactive preparedness planning.

The sensitivity of our results to the basic reproduction number highlights an important dynamic: interventions that reduce overall transmission potential (e.g., vaccination, prophylaxis, early case detection) provide disproportionate benefits to agricultural workers by preventing the amplification of risk created by crowded living conditions. This suggests that agricultural workers should be prioritized in outbreak response strategies not only for equity reasons but also for food system resilience.

### Public health and policy implications

Our findings carry several implications for outbreak preparedness and response. First, agricultural workers should be recognized as a high-priority population for targeted interventions during respiratory disease outbreaks. This includes prioritized access to vaccines, prophylaxis, testing, and treatment. Such prioritization is justified both on equity grounds (addressing disproportionate risk) and on food security grounds (maintaining critical agricultural operations).

Second, interventions targeting household transmission may be particularly effective in agricultural worker populations. Strategies such as providing isolation facilities for infectious individuals, reducing household crowding through temporary housing assistance, or implementing household-level prophylaxis could substantially reduce disease burden. During the COVID-19 pandemic, some agricultural operations successfully implemented such measures, though systematic data on effectiveness remain limited.

Third, harvest season timing should inform outbreak response planning. Surveillance systems should monitor disease activity in agricultural regions, with enhanced vigilance during pre-harvest periods for critical crops. Preparedness plans should identify alternative labor sources or harvest schedule adjustments that could mitigate productivity losses.

Fourth, our framework provides a tool for scenario planning. Public health agencies and agricultural stakeholders can use similar models to assess vulnerabilities in their specific regions and crops, identifying where proactive interventions would provide greatest benefit.

### Limitations and future directions

Several limitations should be noted when interpreting our results. First, our model does not incorporate disease transmission seasonality, which could affect both outbreak dynamics and the alignment of outbreak timing with harvest seasons. Influenza and other respiratory viruses exhibit strong seasonal patterns; incorporating seasonality would refine productivity impact estimates.

Second, we lack county-level data on agricultural worker household characteristics, necessitating the use of regional averages. Substantial within-region heterogeneity likely exists, meaning some counties may face greater vulnerabilities than our estimates suggest.

Third, our mixing parameter $\epsilon$ is not empirically estimated but rather reflects assumed assortativity levels. Contact pattern studies among agricultural workers would enable more precise parameterization.

Fourth, a critical limitation is that NAWS excludes H-2A visa workers, who comprise a substantial and rapidly growing segment of the U.S. agricultural workforce. In fiscal year 2022, approximately 370,000 H-2A jobs were certified, representing roughly 11% of full-time equivalent jobs in U.S. crop agriculture nationally. The H-2A population is particularly important for our three case study crops: berries (including strawberries) were the most common job type filled by H-2A workers in recent years, and estimates suggest 20-50% of California strawberry harvest labor may be H-2A workers. 

H-2A workers receive mandatory employer-provided housing that is subject to federal regulations, but available evidence suggests crowding levels are likely at least as high as—and potentially higher than—those captured in NAWS data. While OSHA and DOL regulations specify minimum space requirements (50 square feet per worker for sleeping areas, 100 square feet if cooking and sleeping occur in the same room), these standards still permit substantial crowding by household transmission standards. Reporting from major strawberry-producing regions documents H-2A housing with four workers per motel room as standard practice and compliant with regulations. Some H-2A housing involves converted motels, retirement homes, and other facilities with workers housed in dormitory-style arrangements that may facilitate transmission differently than the family households characterized in NAWS. Additionally, H-2A workers tend to be younger (average age in their 20s versus 40s for other agricultural workers), predominantly male, and more likely to be housed in non-family group living situations, all of which could affect disease transmission dynamics.

Given that H-2A housing regulations permit and commonly implement four or more workers per room, and that H-2A workers comprise 20-50% of labor in key crops like strawberries, our analysis likely underestimates both the total disease burden among agricultural workers and the differential impact relative to the general population. Our estimates should therefore be considered conservative lower bounds, particularly for crops with high H-2A labor utilization.

Housing inspection data for H-2A workers theoretically exists in state and federal records, as all H-2A housing must be inspected and certified before occupancy. However, these data are not publicly accessible in aggregated form suitable for research. While DOL releases quarterly disclosure data on H-2A certifications including employer information, job characteristics, and worksite locations, specific housing characteristics from inspection reports (such as number of workers per room, total occupancy, or square footage per worker) are not included in public datasets. Future work should pursue these data through Freedom of Information Act requests to DOL or State Workforce Agencies, direct partnerships with state labor departments, or targeted surveys of H-2A employers and housing facilities. Incorporating H-2A housing conditions into disease transmission models would substantially improve estimates of outbreak impacts on agricultural productivity.

Fifth, we have not incorporated other factors that may affect transmission risk among agricultural workers, including occupational exposures, baseline respiratory health disparities (NAWS documents elevated rates of respiratory conditions among agricultural workers), differential comorbidity prevalence, and differences in healthcare access that may affect disease progression and duration of infectiousness.

Fifth, our productivity impact assessment focuses on three labor-intensive crops, but impacts will be felt more broadly across the agricultural sector. Additionally, respiratory disease impacts on the food system extend well beyond harvests to include food processing (as dramatically illustrated by COVID-19 outbreaks in meat processing facilities), distribution, and demand-side dynamics such as consumer stockpiling or avoidance of specific products.

Sixth, our analysis considers only human disease. The broader agricultural disease landscape includes animal and plant diseases, which we have not addressed. We also have not considered zoonotic scenarios where both humans and livestock might be affected by the same pathogen.

Seventh, in our crop-level analysis, we do not account for how labor may be stretched across multiple crops harvested simultaneously. During periods of multi-crop harvest overlap, baseline labor availability may already be constrained, potentially amplifying the impact of disease-related labor losses.

Finally, our use of a reproduction number parameterization warrants discussion. At regional and county scales, we may expect apparently lower $R_0$ values than at finer spatial scales, as we are effectively aggregating many local epidemic curves that occur over extended timespans. Much of the summed incidence curve represents zeros from areas not yet or no longer experiencing active transmission. This spatial aggregation effect should be considered when interpreting our reproduction number sensitivity analyses.

Future work should address these limitations through several avenues: incorporating empirical contact pattern data for agricultural workers, developing coupled models that track multiple crops simultaneously, extending the framework to encompass the full food supply chain from farm to consumer, and validating model predictions against observed outbreak impacts in agricultural regions. Additionally, optimization studies could identify cost-effective intervention strategies that balance public health and agricultural productivity objectives.

### Conclusions

Household crowding creates substantial disparities in respiratory disease burden between agricultural workers and the general population, with meaningful consequences for agricultural productivity. Our modeling framework provides a quantitative basis for anticipating these impacts and planning targeted interventions. As respiratory disease outbreaks continue to emerge and re-emerge, protecting agricultural worker health and maintaining food system resilience should be recognized as interlinked priorities requiring coordinated public health and agricultural policy responses.

## Acknowledgments

[To be added]

## Funding

[To be added]

## Author contributions

[To be added]

## Competing interests

The authors declare no competing interests.

## Data availability

All data sources are publicly available as described in Methods. Analysis code will be made available upon publication at [repository URL].

## References

[To be added - key references would include:
- House & Keeling 2008 (household transmission model)
- COVID-19 agricultural impacts studies
- Influenza secondary attack rate meta-analyses
- Agricultural worker health disparities literature
- NAWS methodological papers
- Food security and disease literature]

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