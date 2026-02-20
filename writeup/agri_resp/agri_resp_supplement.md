
## Acknowledgments

## Funding

## Author contributions

## Competing interests

## Data availability
All data and code associated with this manuscript can be accessed at https://github.com/skissler/IQTeamProject

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

For example, for household size proportions $p(n) = \\{ 0.1, 0.2, 0.3, 0.2, 0.1, 0.05, 0.05\\}$ for $n \in \\{1, 2, ..., 7\\}$, and for an overall crowding fraction of $$P_\text{crowded} = 0.2$$, we have 

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

We imputed county-level NAWS values using three methods (**Figures S2 and S3**):

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

We used crop movements (point-to-point shipments) reported by the USDA as a proxy for harvest volumes of oranges, iceberg lettuce, and strawberries. Few available data sources capture crop-specific harvest volumes at sub-annual temporal resolution, whereas we needed information on the seasonality of harvesting to assess the potential impact of epidemic timing on crop production. To validate the relationship between crop movements and harvests, we cross-referenced normalized average weekly crop movement volumes against independent harvest information for the same crops from University of California Agriculture and Natural Resources Cooperative Extension reports (**Figure S12**). For strawberries, the report gives the fraction of total annual harvest occurring in each month for the Central Coast Region of California (Santa Cruz, Monterey, and San Benito Counties): 5% in April, 12% in May, 25% in June, 26% in July, 18% in August, 12% in September, and 2% in October. For navel oranges, the report states that fruits in the San Joaquin Valley are "normally harvested from November to June". For iceberg lettuce, the report states that planting in the Central Coast Region occurs "continuously from late December to mid-August" and that plants take up to 100 days to mature for cool-season plantings, with shorter maturation times in the warmer season.

We overlaid this information on the normalized crop movement data (**Figure S12**). For strawberries, we converted the monthly harvest proportions to approximate weekly rates by dividing the monthly harvest proportions by the number of weeks in the month (e.g., by 4.28 for a 30-day month). For oranges, we showed the November-to-June harvest window as a horizontal bar. For lettuce, we displayed both the planting window (late December to mid-August) as a lighter bar and an estimated harvest window (approximately late March to early October, obtained by shifting the planting window forward by 100 days at the cool-season end and 50 days at the warm-season end) as a darker bar. There are differences in alignment between the crop movement data and the University of California reports; for example, the crop movement data place peak strawberry shipments in late April and early May, whereas the University of California report indicates peak harvests in June and July; and iceberg lettuce shipments are shifted somewhat later than the estimated harvest window. These discrepancies may reflect limitations of using crop movements as a proxy for harvests, but may also reflect the fact that the movement data capture shipments from across the entire state of California, while the University of California reports pertain to specific sub-regions. With this caveat, we conclude that the crop movement data provide a reasonable proxy for the seasonal pattern of crop harvests.

#### Mathematical model structure

The household-structured SIR model (**Figure S1**) tracks the distribution of households across disease states. Let $H_k(x,y,z,c)$ denote the number of households in population $k$ (where $k \in \{C, A\}$ for community and agricultural populations) with $x$ susceptible, $y$ infected, and $z$ recovered members, and crowding status $c \in \{0,1\}$. The total household size is $n = x + y + z$.

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

Next, we calibrated the between-household transmission rate $\beta$ to achieve target $R_0$ values by running the model at the national level with aggregated ACS household data and systematically varying $\beta$ until the final size matched theoretical predictions for the desired $R_0$. For an SIR model, the relationship between $R_0$ and final size, $R_\infty$, is given implicitly by:
$R_\infty = 1 - e^{-R_0 R_\infty}$. 
For example, $R_0 = 1.5$ corresponds to $R_\infty \approx 0.58$, and $R_0 = 3.0$ corresponds to $R_\infty \approx 0.94$. We used a bisection search algorithm to find $\beta$, converging when the simulated final size was within 0.0005 of the theoretical value. Calibration was performed using a single-population simulation at the national level, with national household distributions computed as population-weighted averages of all county-level ACS data. The calibrated $\beta$ values were then used in the regional and county-level simulations. We assumed no difference in $\beta$ between agricultural workers and the general community, so that all of the transmission differences in the model would come from differences in household size and crowding. 

We initialized outbreaks by setting 0.1% of individuals in each sub-population as infectious. To distribute these initial infections across household types, we moved a fraction $0.001 \times n$ of households of size $n$ from the fully susceptible state $(x = n, y = 0, z = 0)$ to the single-infection state $(x = n-1, y = 1, z = 0)$. Because this fraction scales with household size $n$, exactly 0.1% of individuals are initially infected regardless of household size, approximating uniform random seeding of infections across the population.

#### Symptomatic infection and workforce impact

To translate epidemic dynamics into agricultural workforce availability, we computed the proportion of the agricultural workforce experiencing symptoms on each day. We first calculated the number of new daily infections for each day as $i_t = S(t-1) - S(t)$, where $S(t)$ is the proportion susceptible at time $t$. We assumed symptoms began one day after infection onset and lasted for three days, so that individuals infected on day $t$ were symptomatic on days $t+1$, $t+2$, and $t+3$. The proportion of symptomatic individuals on day $t$ was then:

$$\text{symp}_t = p_{\text{symp}} \sum_{d=1}^{3} i_{t-d}$$

where $p_{\text{symp}}$ is the probability that an infection is symptomatic. Daily workforce strength was defined as $\text{wf}(t) = 1 - \text{symp}_t$. 

To assess the impact of epidemic timing on crop production, we shifted the epidemic curve so that the community symptomatic peak aligned with each day of the calendar year. For each epidemic curve, we calculated the outbreak-adjusted harvest volume for each crop on each day as $V_{\text{adj}}(t) = V(t) \times \text{wf}(t)$, where $V(t)$ is the mean harvest volume on day $t$. The total production loss for that epidemic curve, expressed as a percentage, was $(1 - \sum_t V_{\text{adj}}(t) / \sum_t V(t)) \times 100\%$. We repeated this calculation for epidemics peaking on each day of the year. 

### Supplementary Figures

**Figure S1. Schematic of the disease transmission model for a household of size 3.** An uncrowded agricultural household of size 3 that begins with all members (discs) susceptible (black) is represented as $H_A(3,0,0,0)$ (top-most household). New infections (downward movements; red discs) can occur at rate $\beta I x + \tau x y$; but since $y = 0$, the force of infection is given fully by the between-household force of infection, $\beta I x$. Once an infection occurs within the household, either a new household member can become infected at rate $\beta I x + \tau x y$ (downward movement), or the initially infected individual can recover (left-to-right movement; blue discs) at rate $\gamma y$. 

<div align="center">
  <img src="../../figures/modelstructure.png" width="60%">
</div>
<!--- ![Model structure](../../figures/modelstructure.png) --->

**Figure S2. County-level household crowding distributions for agricultural workers and the general community under three imputation methods** Histograms depict the proportion of households crowded in counties within each of the six NAWS regions, for agricultural workers (blue) and the general community (red). County-level crowding rates for the general community are taken from the ACS. County-level crowding rates for agricultural workers are imputed using three different methods: (A) an additive adjustment, where county-level crowding rates for agricultural workers are shifted by the difference between county-level and regional mean ACS values; (B) a multiplicative adjustment, where county-level crowding rates for agricultural workers are shifted by the ratio between county-level and regional mean ACS values; and (C) no adjustment, where crowding rates across counties for agricultural workers are equal to the regional mean. Red dashed vertical lines indicate the regional ACS mean, and blue dashed vertical lines indicate the regional NAWS estimate for agricultural workers.

**(A)** ![Crowding distributions imputed additive](../../figures/crowding_distribution_additive.png)
**(B)** ![Crowding distributions imputed multiplicative](../../figures/crowding_distribution_multiplicative.png)
**(C)** ![Crowding distributions imputed none](../../figures/crowding_distribution_none.png)

**Figure S3. County-level household size distributions for agricultural workers and the general community under three imputation methods.** Histograms depict the mean household size (A, C, E) and the proportion of households with 4 or more occupants (B, D, F) in counties within each of the six NAWS regions, for agricultural workers (blue) and the general community (red). County-level household size distributions are taken from the ACS. County-level household size distributions for agricultural workers are imputed using three different methods: (A) an additive adjustment, where county-level household size distributions for agricultural workers are shifted by the difference between county-level and regional mean ACS values and re-normalized; (B) a multiplicative adjustment, where county-level household size distributions for agricultural workers are shifted by the ratio between county-level and regional mean ACS values and re-normalized; and (C) no adjustment, where household size distributions across counties for agricultural workers are equal to the regional mean. Red dashed vertical lines indicate the regional ACS mean, and blue dashed vertical lines indicate the regional NAWS estimate for agricultural workers.

**(A)** ![Mean and four plus household size distributions imputed additive](../../figures/hhsize_distribution_additive.png)
**(B)** ![Mean and four plus household size distributions imputed multiplicative](../../figures/hhsize_distribution_multiplicative.png)
**(C)** ![Mean and four plus household size distributions imputed none](../../figures/hhsize_distribution_none.png)

**Figure S4. Sensitivity of epidemic summary statistics to key parameters.** Panels depict the impact of the basic reproduction number ($R_0$), assortativity ($\eta$), secondary attack rate (SAR) in crowded households, and fold-difference in crowding rates between households of size 2 and households of size 7+ ($d$) on (A) the ratio of final sizes, (B) the ratio of peak sizes, (C) the time difference between peaks, and (D) the maximum prevalence ratio between agricultural workers and the general community. All parameter values are held at their baseline values (**Supplementary Table S2**) except for the one being varied in the panel. Colors represent the various NAWS regions. Dashed horizontal lines mark the value indicating "no difference". 

**(A)** ![Sensitivity overview attack rate ratio](../../figures/sensitivity_overview_attack_rate_ratio.png)

**(B)** ![Sensitivity overview peak prevalence ratio](../../figures/sensitivity_overview_peak_prevalence_ratio.png)

**(C)** ![Sensitivity overview peak timing](../../figures/sensitivity_overview_peaktiming.png)

**(D)** ![Sensitivity overview max relative infection](../../figures/sensitivity_overview_max_relative_infection.png)

<!-- Sensitivity of final size to the basic reproduction number ($R_0$), assortativity ($\eta$), secondary attack rate in crowded households (SAR), and crowding fold difference. Each panel shows one sensitivity dimension, with parameter values on the horizontal axis and final size (proportion of the population ultimately infected) on the vertical axis. Colored lines connect results across parameter values for each of the six NAWS regions (East, Southeast, Midwest, Southwest, Northwest, California), using an Okabe-Ito colorblind-friendly palette. Solid lines with points represent agricultural workers (A); dashed lines with open points represent the general community (C). A horizontal gray dashed reference line indicates the baseline value. -->

<!-- ![Sensitivity overview final size](../../figures/sensitivity_overview_attackrate.png) -->

<!-- **Figure S5.** Sensitivity of peak prevalence to the basic reproduction number ($R_0$), assortativity ($\eta$), secondary attack rate in crowded households (SAR), and crowding fold difference. Layout and visual encoding are as in Figure S4, with peak prevalence (maximum proportion infected at any single time point) on the vertical axis. -->

<!-- ![Sensitivity overview peak size](../../figures/sensitivity_overview_peaksize.png) -->

<!-- **Figure S6.** Sensitivity of time to peak prevalence to the basic reproduction number ($R_0$), assortativity ($\eta$), secondary attack rate in crowded households (SAR), and crowding fold difference. Layout and visual encoding are as in Figure S4, with time to peak (days from simulation start to maximum prevalence) on the vertical axis. -->


<!-- **Figure S7.** Sensitivity of the maximum relative infection rate (agricultural workers divided by community) to the basic reproduction number ($R_0$), assortativity ($\eta$), secondary attack rate in crowded households (SAR), and crowding fold difference. Layout and visual encoding are as in Figure S4, with the maximum ratio of agricultural worker to community infection prevalence on the vertical axis. A horizontal gray dashed reference line at 1.0 indicates equal infection rates between the two populations. -->

<!-- **Figure S8.** Sensitivity of the final size ratio (agricultural workers divided by community) to the basic reproduction number ($R_0$), assortativity ($\eta$), secondary attack rate in crowded households (SAR), and crowding fold difference. Layout and visual encoding are as in Figure S4, with the ratio of agricultural worker to community final sizes on the vertical axis. A horizontal gray dashed reference line at 1.0 indicates equal final sizes. -->



<!-- **Figure S9.** Sensitivity of the peak prevalence ratio (agricultural workers divided by community) to the basic reproduction number ($R_0$), assortativity ($\eta$), secondary attack rate in crowded households (SAR), and crowding fold difference. Layout and visual encoding are as in Figure S4, with the ratio of agricultural worker to community peak prevalence on the vertical axis. A horizontal gray dashed reference line at 1.0 indicates equal peak prevalence. -->


<!-- 
**Figure S5.** Epidemic curves under sensitivity to $R_0$, showing proportion currently infected over time across the six NAWS regions. Each panel corresponds to one region. Within each panel, different colors represent different $R_0$ values (1.2, 1.5, 2.0, 3.0). Solid lines represent agricultural workers (A); dashed lines represent the general community (C).

![Sensitivity R0 epidemic curves](../../figures/sensitivity_r0_curves_all_regions.png)
 -->
**Figure S5. Impact of the basic reproduction number ($R_0$) on cumulative infections among agricultural workers and the general community.** Panels depict the simulated cumulative infections over time for agricultural workers (solid lines) and the general community (dashed lines) in each of the six NAWS regions across different $R_0$ values (colors). All other parameter values are held at baseline (**Supplementary Table S2**).

![Sensitivity R0 cumulative](../../figures/sensitivity_r0_cumulative_all_regions.png)

<!-- **Figure S7.** Relative infection rate (agricultural workers divided by community) over time under sensitivity to $R_0$ across the six NAWS regions. Each panel corresponds to one region. Different colors represent different $R_0$ values. A horizontal gray dashed line at 1.0 indicates equal infection rates. Values above 1.0 indicate that agricultural workers have higher infection prevalence than the general community.

![Sensitivity R0 relative infection](../../figures/sensitivity_r0_relative_infection_all_regions.png) -->

<!-- **Figure S8.** Epidemic curves under sensitivity to assortativity ($\eta$), showing proportion currently infected over time across the six NAWS regions. Visual encoding is as in Figure S10, with different colors representing different $\eta$ values (0, 0.25, 0.33, 0.50, 0.67, 0.75). Higher $\eta$ implies more within-group mixing. Solid lines: agricultural workers (A); dashed lines: general community (C). -->

<!-- ![Sensitivity eps epidemic curves](../../figures/sensitivity_eps_curves_all_regions.png) -->

**Figure S6. Impact of assortativity ($\eta$) on cumulative infections among agricultural workers and the general community.** Panels depict the simulated cumulative infections over time for agricultural workers (solid lines) and the general community (dashed lines) in each of the six NAWS regions across different values of the assortativity parameter $\eta$ (colors), where larger $\eta$ corresponds to more within-group mixing. All other parameter values are held at baseline (**Supplementary Table S2**).

![Sensitivity eps cumulative](../../figures/sensitivity_eps_cumulative_all_regions.png)

<!-- **Figure S10.** Relative infection rate (agricultural workers divided by community) over time under sensitivity to assortativity ($\eta$) across the six NAWS regions. Visual encoding is as in Figure S12, with different colors for each $\eta$ value.

![Sensitivity eps relative infection](../../figures/sensitivity_eps_relative_infection_all_regions.png) -->

<!-- **Figure S11.** Epidemic curves under sensitivity to the secondary attack rate (SAR) in crowded households, showing proportion currently infected over time across the six NAWS regions. Visual encoding is as in Figure S10, with different colors representing SAR values (20%, 30%, 40%, 50%, 60%). Solid lines: agricultural workers (A); dashed lines: general community (C).

![Sensitivity SAR epidemic curves](../../figures/sensitivity_sar_curves_all_regions.png) -->

**Figure S7. Impact of the secondary attack rate (SAR) in crowded households on cumulative infections among agricultural workers and the general community.** Panels depict the simulated cumulative infections over time for agricultural workers (solid lines) and the general community (dashed lines) in each of the six NAWS regions across different values of the secondary attack rate (SAR) in crowded households. All other parameter values are held at baseline; note that the SAR in uncrowded households was held fixed at 0.2. (**Supplementary Table S2**).

![Sensitivity SAR cumulative](../../figures/sensitivity_sar_cumulative_all_regions.png)

<!-- **Figure S13.** Relative infection rate (agricultural workers divided by community) over time under sensitivity to the secondary attack rate in crowded households across the six NAWS regions. Visual encoding is as in Figure S12, with different colors for each SAR value.

![Sensitivity SAR relative infection](../../figures/sensitivity_sar_relative_infection_all_regions.png) -->

<!-- **Figure S14.** Epidemic curves under sensitivity to the crowding fold difference, showing proportion currently infected over time across the six NAWS regions. Visual encoding is as in Figure S10, with different colors representing fold difference values (1, 2, 3). A fold difference of 1 means no size-dependent crowding gradient; a fold difference of 3 means households of size 7+ are three times as likely to be crowded as households of size 2. Solid lines: agricultural workers (A); dashed lines: general community (C).

![Sensitivity fold epidemic curves](../../figures/sensitivity_fold_curves_all_regions.png) -->

**Figure S8. Impact of the crowding fold difference parameter $d$ on cumulative infections among agricultural workers and the general community.** Panels depict the simulated cumulative infections over time for agricultural workers (solid lines) and the general community (dashed lines) in each of the six NAWS regions across different values of the crowding fold-difference parameter $d$ (colors), which represents how much more likely a household of size 7+ is to be crowded than a household of size 2. All other parameter values are held at baseline (**Supplementary Table S2**).

![Sensitivity fold cumulative](../../figures/sensitivity_fold_cumulative_all_regions.png)

<!-- **Figure S16.** Relative infection rate (agricultural workers divided by community) over time under sensitivity to the crowding fold difference across the six NAWS regions. Visual encoding is as in Figure S12, with different colors for each fold difference value.

![Sensitivity fold relative infection](../../figures/sensitivity_fold_relative_infection_all_regions.png)
 -->
**Figure S9. Epidemic trajectories under the multiplicative county-level imputation method for agricultural worker household characteristics.** (A) Simulated infection prevalence over time for agricultural workers (blue) and the general community (red) for the six NAWS regions, with simulations at both the region level (thick lines with borders) and county level (thin, semi-transparent lines). (B) Cumulative infections over time for agricultural workers (blue) and the general community (red) for the six NAWS regions, with simulations at both the region level (thick lines with borders) and county level (thin, semi-transparent lines). (C) Prevalence ratio between agricultural workers and the general community for the six NAWS regions, with simulations at both the region level (thick lines with borders) and county level (thin, semi-transparent lines). County-level household attributes for agricultural workers are imputed using the "multiplicative" method, in which regional NAWS household attributes are adjusted by the ratio between the county-level ACS values and the regional ACS mean. 

**(A)** ![Epidemic multiplicative](../../figures/main_overlay_multiplicative.png)

**(B)** ![Cumulative infections multiplicative](../../figures/main_cumulative_multiplicative.png)

**(C)** ![Relative infections multiplicative](../../figures/main_relative_infection_multiplicative.png)

**Figure S10. Epidemic trajectories under the "null" county-level imputation method for agricultural worker household characteristics.** (A) Simulated infection prevalence over time for agricultural workers (blue) and the general community (red) for the six NAWS regions, with simulations at both the region level (thick lines with borders) and county level (thin, semi-transparent lines). (B) Cumulative infections over time for agricultural workers (blue) and the general community (red) for the six NAWS regions, with simulations at both the region level (thick lines with borders) and county level (thin, semi-transparent lines). (C) Prevalence ratio between agricultural workers and the general community for the six NAWS regions, with simulations at both the region level (thick lines with borders) and county level (thin, semi-transparent lines). County-level household attributes for agricultural workers are imputed using the "null" method, in which county-level household attributes for agricultural workers are taken to be equal to the regional NAWS value, with no adjustment. 

**(A)** ![Epidemic none](../../figures/main_overlay_none.png)

**(B)** ![Cumulative infections none](../../figures/main_cumulative_none.png)

**(C)** ![Relative infections none](../../figures/main_relative_infection_none.png)

**Figure S11. Weekly crop movements for three labor-intensive California crops** Weekly point-to-point crop shipments (in million pounds) originating in California for iceberg lettuce (blue), oranges (orange), and strawberries (magenta), from 2018–2024. 

![Crop movements](../../figures/crop_movements_raw.png)

**Figure S12. Average weekly crop movements with known harvesting patterns.** Comparison of normalized average weekly crop movements (proportion of total annual volume from 2018-2024; solid lines) with harvest information from University of California Agriculture and Natural Resources Cooperative Extension reports (dashed line, semi-transparent bars) for iceberg lettuce (blue), oranges (orange), and strawberries (magenta) in California. For strawberries, the University of California report gives explicit monthly harvest proportions, which are re-scaled to approximate weekly harvest volumes (dashed magenta line). For oranges, the reported harvest season runs from November to June (semi-transparent orange bar). For iceberg lettuce, the planting season runs from late December to mid-August (lighter semi-transparent blue bar). We computed an approximate harvest season (darker semi-transparent blue bar) by shifting the planting window forward by 100 days at the cool-season (December) end and by 50 days at the warm-season end (August) to account for reported seasonal differences in maturation time. 

![Crop movements validated](../../figures/crop_movements_validated.png)

**Figure S13. Estimated crop production loss as a function of epidemic peak timing when all infections are symptomatic.** Simulated percent of total harvest volume impacted by outbreak-induced labor shortages for iceberg lettuce (blue), strawberries (magenta), and oranges (orange) under baseline parameter values and symptomatic proportion $p_\text{symp} = 1$. The horizontal axis represents the day of the year on which infection prevalence peaks in the general community (peak infections among agricultural workers occur a few days earlier). Production losses for other symptomatic probabilities can be derived by re-scaling these curves by the desired $p_\text{symp}$. 

![Crop impact by peak day](../../figures/crop_impact_by_peakday.png)

### Supplementary Tables

**Table S1. Household characteristics by region for agricultural workers and the general community.** Mean household size is the population-weighted average across household sizes 1–7+. Crowding proportion is the fraction of households with more than 1 occupant per room. Agricultural worker data are from the National Agricultural Workers Survey (NAWS), and general community data are from the American Community Survey (ACS), aggregated to the regional level using population-weighted averages.

| Region | Mean household size | | Prop. of households size 4+ | | Crowding proportion | |
|:---|:---:|:---:|:---:|:---:|:---:|:---:|
| | Agricultural workers | General community | Agricultural workers | General community | Agricultural workers | General community |
| East | 3.9 | 2.4 | 54.7% | 21.7% | 20.3% | 2.8% |
| Southeast | 3.9 | 2.4 | 52.3% | 20.6% | 22.4% | 2.6% |
| Midwest | 3.3 | 2.4 | 41.1% | 20.7% | 11.2% | 1.9% |
| Southwest | 3.3 | 2.6 | 45.1% | 25.5% | 14.8% | 4.5% |
| Northwest | 3.9 | 2.5 | 58.6% | 23.4% | 27.3% | 3.2% |
| California | 4.1 | 2.8 | 61.7% | 29.3% | 32.8% | 8.3% |

**Table S2. Baseline and sensitivity analysis parameter values for the disease transmission model.** Each sensitivity analysis varies one parameter at a time while holding all others at baseline values (first row). Bold values indicate the parameter(s) being varied in each row.

| $R_0$ | $\eta$ | SAR (crowded) | Fold diff. ($d$) | $\tau$ | $\tau_{\text{boost}}$ | $\beta$ | $\gamma$ |
|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|
| 1.5 | 0.67 | 40% | 2 | 0.050 | 0.083 | 0.2108 | 0.200 |
| **1.2** | 0.67 | 40% | 2 | 0.050 | 0.083 | **0.1546** | 0.200 |
| **2.0** | 0.67 | 40% | 2 | 0.050 | 0.083 | **0.3078** | 0.200 |
| **3.0** | 0.67 | 40% | 2 | 0.050 | 0.083 | **0.5054** | 0.200 |
| 1.5 | **0.75** | 40% | 2 | 0.050 | 0.083 | 0.2108 | 0.200 |
| 1.5 | **0.50** | 40% | 2 | 0.050 | 0.083 | 0.2108 | 0.200 |
| 1.5 | **0.33** | 40% | 2 | 0.050 | 0.083 | 0.2108 | 0.200 |
| 1.5 | **0.25** | 40% | 2 | 0.050 | 0.083 | 0.2108 | 0.200 |
| 1.5 | **0.00** | 40% | 2 | 0.050 | 0.083 | 0.2108 | 0.200 |
| 1.5 | 0.67 | **20%** | 2 | 0.050 | **0.000** | **0.2142** | 0.200 |
| 1.5 | 0.67 | **30%** | 2 | 0.050 | **0.036** | **0.2122** | 0.200 |
| 1.5 | 0.67 | **50%** | 2 | 0.050 | **0.150** | **0.2096** | 0.200 |
| 1.5 | 0.67 | **60%** | 2 | 0.050 | **0.250** | **0.2086** | 0.200 |
| 1.5 | 0.67 | 40% | **1** | 0.050 | 0.083 | **0.2113** | 0.200 |
| 1.5 | 0.67 | 40% | **3** | 0.050 | 0.083 | **0.2103** | 0.200 |

<!-- **Table S3.** Mixing matrix elements by region and assortativity parameter ($\eta$). For each region, $w_A$ is the proportion of the population that are agricultural workers (derived from ACS data). The mixing matrix governs between-household contact patterns: $m_{AA}$ is the fraction of agricultural workers' between-household contacts that are with other agricultural workers, $m_{AC}$ is the fraction with the general community, and vice versa for $m_{CC}$ and $m_{CA}$. Because $w_A$ is small (0.7–2.2%), $m_{AA} \approx \eta$ and $m_{CC} \approx 1$ across all values of $\eta$. At $\eta = 0$ (proportional mixing), agricultural workers have only $w_A$ of contacts within their own group; the baseline $\eta = 2/3$ is highlighted in bold.

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
| 3/4 | California | 2.2 | 75.5 | 24.5 | 99.5 | 0.5 | -->

**Table S3. Summary statistics for simulated epidemics across regions and parameter sets.** Simulated peak prevalence, time to epidemic peak, and final size for agricultural workers (A) and the general community (C).

| Parameter set | Region | Peak prevalence | | Time to peak (days) | | Final size | |
|:---|:---|:---:|:---:|:---:|:---:|:---:|:---:|
| | | Agricultural workers | General community | Agricultural workers | General community | Agricultural workers | General community |
| Baseline | East | 8.9% | 6.3% | 45 | 55 | 72.1% | 57.2% |
| | Southeast | 8.9% | 6.2% | 44 | 56 | 72.3% | 56.5% |
| | Midwest | 7.6% | 6.1% | 50 | 56 | 65.9% | 56.4% |
| | Southwest | 8.9% | 7.2% | 46 | 51 | 69.4% | 60.2% |
| | Northwest | 9.7% | 6.9% | 43 | 52 | 73.6% | 58.9% |
| | California | 11.4% | 8.4% | 39 | 47 | 76.7% | 63.5% |
| $R_0$ = 1.2 | East | 2.9% | 1.4% | 71 | 99 | 49.8% | 29.5% |
| | Southeast | 2.9% | 1.3% | 71 | 100 | 49.8% | 28.4% |
| | Midwest | 2.0% | 1.3% | 87 | 102 | 40.3% | 28.3% |
| | Southwest | 2.9% | 2.0% | 76 | 88 | 47.1% | 34.7% |
| | Northwest | 3.4% | 1.8% | 68 | 90 | 52.6% | 32.7% |
| | California | 4.8% | 2.9% | 59 | 76 | 59.2% | 40.4% |
| $R_0$ = 2 | East | 18.7% | 15.6% | 28 | 33 | 86.8% | 79.2% |
| | Southeast | 18.7% | 15.5% | 28 | 33 | 87.0% | 78.9% |
| | Midwest | 17.2% | 15.4% | 31 | 33 | 83.8% | 78.8% |
| | Southwest | 18.5% | 16.6% | 29 | 31 | 85.3% | 80.5% |
| | Northwest | 19.4% | 16.2% | 27 | 32 | 87.5% | 79.9% |
| | California | 21.1% | 17.8% | 26 | 29 | 88.9% | 82.0% |
| $R_0$ = 3 | East | 33.2% | 30.4% | 17 | 19 | 96.1% | 93.9% |
| | Southeast | 33.2% | 30.3% | 17 | 19 | 96.2% | 93.8% |
| | Midwest | 31.9% | 30.3% | 18 | 19 | 95.2% | 93.8% |
| | Southwest | 32.9% | 31.2% | 17 | 18 | 95.6% | 94.2% |
| | Northwest | 33.7% | 30.9% | 17 | 18 | 96.3% | 94.1% |
| | California | 35.1% | 32.1% | 16 | 17 | 96.7% | 94.6% |
| $\eta$ = 0.75 | East | 9.6% | 6.3% | 42 | 55 | 72.8% | 57.2% |
| | Southeast | 9.7% | 6.2% | 42 | 56 | 73.0% | 56.5% |
| | Midwest | 7.9% | 6.1% | 49 | 56 | 66.5% | 56.4% |
| | Southwest | 9.2% | 7.2% | 44 | 51 | 69.9% | 60.2% |
| | Northwest | 10.3% | 6.9% | 41 | 52 | 74.2% | 58.8% |
| | California | 12.1% | 8.4% | 37 | 47 | 77.2% | 63.5% |
| $\eta$ = 0.5 | East | 8.2% | 6.3% | 49 | 55 | 70.7% | 57.2% |
| | Southeast | 8.2% | 6.2% | 49 | 56 | 70.9% | 56.6% |
| | Midwest | 7.3% | 6.1% | 53 | 56 | 64.9% | 56.4% |
| | Southwest | 8.5% | 7.2% | 48 | 51 | 68.5% | 60.2% |
| | Northwest | 9.0% | 6.9% | 47 | 52 | 72.4% | 58.9% |
| | California | 10.7% | 8.5% | 42 | 47 | 75.8% | 63.5% |
| $\eta$ = 0.33 | East | 8.0% | 6.3% | 51 | 55 | 69.5% | 57.2% |
| | Southeast | 7.9% | 6.2% | 52 | 56 | 69.6% | 56.6% |
| | Midwest | 7.1% | 6.1% | 54 | 56 | 64.1% | 56.4% |
| | Southwest | 8.3% | 7.2% | 49 | 51 | 67.8% | 60.2% |
| | Northwest | 8.7% | 6.9% | 49 | 52 | 71.2% | 59.0% |
| | California | 10.4% | 8.5% | 44 | 47 | 74.9% | 63.6% |
| $\eta$ = 0.25 | East | 7.9% | 6.3% | 52 | 55 | 69.0% | 57.2% |
| | Southeast | 7.8% | 6.2% | 52 | 56 | 69.0% | 56.6% |
| | Midwest | 7.0% | 6.1% | 54 | 56 | 63.7% | 56.4% |
| | Southwest | 8.3% | 7.2% | 49 | 51 | 67.5% | 60.2% |
| | Northwest | 8.6% | 6.9% | 50 | 52 | 70.7% | 59.0% |
| | California | 10.3% | 8.5% | 44 | 47 | 74.5% | 63.6% |
| $\eta$ = 0 | East | 7.6% | 6.3% | 53 | 55 | 67.5% | 57.2% |
| | Southeast | 7.5% | 6.2% | 54 | 56 | 67.5% | 56.6% |
| | Midwest | 6.9% | 6.1% | 55 | 56 | 62.7% | 56.5% |
| | Southwest | 8.1% | 7.2% | 50 | 51 | 66.6% | 60.2% |
| | Northwest | 8.3% | 6.9% | 51 | 52 | 69.4% | 59.0% |
| | California | 10.1% | 8.5% | 45 | 47 | 73.4% | 63.6% |
| Crowded SAR = 20% | East | 8.1% | 6.3% | 50 | 56 | 68.4% | 57.4% |
| | Southeast | 8.0% | 6.2% | 50 | 56 | 68.2% | 56.8% |
| | Midwest | 7.3% | 6.2% | 52 | 56 | 64.2% | 56.9% |
| | Southwest | 8.1% | 7.0% | 50 | 53 | 66.5% | 59.8% |
| | Northwest | 8.3% | 6.8% | 49 | 53 | 68.4% | 58.9% |
| | California | 9.2% | 7.7% | 47 | 50 | 70.5% | 61.8% |
| Crowded SAR = 30% | East | 8.5% | 6.3% | 47 | 55 | 70.5% | 57.2% |
| | Southeast | 8.5% | 6.2% | 47 | 56 | 70.6% | 56.6% |
| | Midwest | 7.4% | 6.2% | 52 | 56 | 65.2% | 56.6% |
| | Southwest | 8.5% | 7.1% | 48 | 52 | 68.2% | 60.0% |
| | Northwest | 9.0% | 6.8% | 46 | 53 | 71.4% | 58.9% |
| | California | 10.4% | 8.1% | 43 | 48 | 74.2% | 62.8% |
| Crowded SAR = 50% | East | 9.3% | 6.3% | 42 | 55 | 73.0% | 57.1% |
| | Southeast | 9.3% | 6.2% | 42 | 55 | 73.4% | 56.4% |
| | Midwest | 7.7% | 6.1% | 49 | 56 | 66.4% | 56.2% |
| | Southwest | 9.1% | 7.3% | 44 | 50 | 70.3% | 60.3% |
| | Northwest | 10.2% | 7.0% | 40 | 52 | 75.0% | 58.8% |
| | California | 12.3% | 8.8% | 36 | 45 | 78.4% | 64.0% |
| Crowded SAR = 60% | East | 9.5% | 6.4% | 40 | 54 | 73.7% | 57.0% |
| | Southeast | 9.6% | 6.2% | 39 | 55 | 74.1% | 56.3% |
| | Midwest | 7.7% | 6.1% | 48 | 56 | 66.8% | 56.1% |
| | Southwest | 9.3% | 7.4% | 42 | 49 | 70.8% | 60.4% |
| | Northwest | 10.6% | 7.0% | 37 | 51 | 76.0% | 58.8% |
| | California | 13.0% | 9.0% | 33 | 43 | 79.4% | 64.4% |
| $d$ = 1 | East | 8.8% | 6.3% | 46 | 55 | 71.6% | 57.2% |
| | Southeast | 8.8% | 6.2% | 46 | 56 | 71.8% | 56.6% |
| | Midwest | 7.5% | 6.1% | 51 | 56 | 65.7% | 56.5% |
| | Southwest | 8.7% | 7.2% | 47 | 52 | 69.1% | 60.1% |
| | Northwest | 9.5% | 6.9% | 44 | 53 | 73.1% | 58.9% |
| | California | 11.1% | 8.3% | 40 | 47 | 76.1% | 63.3% |
| $d$ = 3 | East | 9.0% | 6.3% | 44 | 55 | 72.2% | 57.1% |
| | Southeast | 9.0% | 6.2% | 44 | 56 | 72.5% | 56.4% |
| | Midwest | 7.6% | 6.1% | 50 | 56 | 66.0% | 56.3% |
| | Southwest | 8.9% | 7.3% | 46 | 51 | 69.6% | 60.2% |
| | Northwest | 9.7% | 6.9% | 43 | 52 | 73.8% | 58.8% |
| | California | 11.6% | 8.5% | 38 | 46 | 77.0% | 63.6% |

**Table S4. Differential metrics between agricultural workers and the general community across regions and parameter sets.** Peak prevalence ratio, final size ratio, peak timing difference, and maximum infection prevalence ratio between agricultural workers and the general community.

| Parameter set | Region | Peak prevalence ratio | Final size ratio | Time difference (days) | Max prevalence ratio |
| :--- | :--- | :---: | :---: | :---: | :---: |
| Baseline | East | 1.41 | 1.26 | −10 | 2.62 |
|  | Southeast | 1.45 | 1.28 | −12 | 2.78 |
|  | Midwest | 1.24 | 1.17 | −6 | 1.74 |
|  | Southwest | 1.23 | 1.15 | −5 | 1.75 |
|  | Northwest | 1.40 | 1.25 | −9 | 2.52 |
|  | California | 1.35 | 1.21 | −8 | 2.38 |
| $R_0$ = 1.2 | East | 2.05 | 1.69 | −28 | 3.41 |
|  | Southeast | 2.18 | 1.76 | −29 | 3.69 |
|  | Midwest | 1.53 | 1.42 | −15 | 2.00 |
|  | Southwest | 1.46 | 1.36 | −12 | 2.00 |
|  | Northwest | 1.89 | 1.61 | −22 | 3.12 |
|  | California | 1.68 | 1.46 | −17 | 2.83 |
| $R_0$ = 2 | East | 1.20 | 1.10 | −5 | 2.09 |
|  | Southeast | 1.21 | 1.10 | −5 | 2.19 |
|  | Midwest | 1.12 | 1.06 | −2 | 1.52 |
|  | Southwest | 1.12 | 1.06 | −2 | 1.55 |
|  | Northwest | 1.20 | 1.10 | −5 | 2.07 |
|  | California | 1.19 | 1.08 | −3 | 2.01 |
| $R_0$ = 3 | East | 1.09 | 1.02 | −2 | 1.67 |
|  | Southeast | 1.09 | 1.02 | −2 | 1.73 |
|  | Midwest | 1.05 | 1.02 | −1 | 1.34 |
|  | Southwest | 1.05 | 1.01 | −1 | 1.37 |
|  | Northwest | 1.09 | 1.02 | −1 | 1.68 |
|  | California | 1.09 | 1.02 | −1 | 1.67 |
| $\eta$ = 0.75 | East | 1.52 | 1.27 | −13 | 3.14 |
|  | Southeast | 1.57 | 1.29 | −14 | 3.37 |
|  | Midwest | 1.29 | 1.18 | −7 | 1.95 |
|  | Southwest | 1.27 | 1.16 | −7 | 1.96 |
|  | Northwest | 1.50 | 1.26 | −11 | 3.00 |
|  | California | 1.43 | 1.22 | −10 | 2.78 |
| $\eta$ = 0.5 | East | 1.30 | 1.24 | −6 | 1.99 |
|  | Southeast | 1.32 | 1.25 | −7 | 2.08 |
|  | Midwest | 1.18 | 1.15 | −3 | 1.48 |
|  | Southwest | 1.18 | 1.14 | −3 | 1.50 |
|  | Northwest | 1.30 | 1.23 | −5 | 1.94 |
|  | California | 1.27 | 1.19 | −5 | 1.88 |
| $\eta$ = 0.33 | East | 1.26 | 1.22 | −4 | 1.67 |
|  | Southeast | 1.28 | 1.23 | −4 | 1.73 |
|  | Midwest | 1.16 | 1.14 | −2 | 1.35 |
|  | Southwest | 1.15 | 1.13 | −2 | 1.36 |
|  | Northwest | 1.26 | 1.21 | −3 | 1.65 |
|  | California | 1.23 | 1.18 | −3 | 1.61 |
| $\eta$ = 0.25 | East | 1.24 | 1.21 | −3 | 1.58 |
|  | Southeast | 1.26 | 1.22 | −4 | 1.62 |
|  | Midwest | 1.15 | 1.13 | −2 | 1.31 |
|  | Southwest | 1.14 | 1.12 | −2 | 1.32 |
|  | Northwest | 1.24 | 1.20 | −2 | 1.55 |
|  | California | 1.22 | 1.17 | −3 | 1.53 |
| $\eta$ = 0 | East | 1.21 | 1.18 | −2 | 1.40 |
|  | Southeast | 1.22 | 1.19 | −2 | 1.42 |
|  | Midwest | 1.12 | 1.11 | −1 | 1.22 |
|  | Southwest | 1.12 | 1.11 | −1 | 1.23 |
|  | Northwest | 1.21 | 1.18 | −1 | 1.39 |
|  | California | 1.19 | 1.15 | −2 | 1.37 |
| Crowded SAR = 20% | East | 1.28 | 1.19 | −6 | 1.80 |
|  | Southeast | 1.29 | 1.20 | −6 | 1.83 |
|  | Midwest | 1.17 | 1.13 | −4 | 1.44 |
|  | Southwest | 1.16 | 1.11 | −3 | 1.41 |
|  | Northwest | 1.23 | 1.16 | −4 | 1.59 |
|  | California | 1.20 | 1.14 | −3 | 1.54 |
| Crowded SAR = 30% | East | 1.35 | 1.23 | −8 | 2.18 |
|  | Southeast | 1.38 | 1.25 | −9 | 2.27 |
|  | Midwest | 1.21 | 1.15 | −4 | 1.58 |
|  | Southwest | 1.20 | 1.14 | −4 | 1.57 |
|  | Northwest | 1.32 | 1.21 | −7 | 2.01 |
|  | California | 1.29 | 1.18 | −5 | 1.93 |
| Crowded SAR = 50% | East | 1.46 | 1.28 | −13 | 3.08 |
|  | Southeast | 1.51 | 1.30 | −13 | 3.33 |
|  | Midwest | 1.25 | 1.18 | −7 | 1.89 |
|  | Southwest | 1.24 | 1.16 | −6 | 1.93 |
|  | Northwest | 1.47 | 1.27 | −12 | 3.09 |
|  | California | 1.40 | 1.22 | −9 | 2.85 |
| Crowded SAR = 60% | East | 1.49 | 1.29 | −14 | 3.54 |
|  | Southeast | 1.55 | 1.32 | −16 | 3.90 |
|  | Midwest | 1.27 | 1.19 | −8 | 2.03 |
|  | Southwest | 1.26 | 1.17 | −7 | 2.11 |
|  | Northwest | 1.52 | 1.29 | −14 | 3.69 |
|  | California | 1.44 | 1.23 | −10 | 3.33 |
| $d$ = 1 | East | 1.39 | 1.25 | −9 | 2.46 |
|  | Southeast | 1.42 | 1.27 | −10 | 2.60 |
|  | Midwest | 1.23 | 1.16 | −5 | 1.67 |
|  | Southwest | 1.22 | 1.15 | −5 | 1.69 |
|  | Northwest | 1.38 | 1.24 | −9 | 2.37 |
|  | California | 1.33 | 1.20 | −7 | 2.26 |
| $d$ = 3 | East | 1.42 | 1.26 | −11 | 2.70 |
|  | Southeast | 1.46 | 1.28 | −12 | 2.88 |
|  | Midwest | 1.24 | 1.17 | −6 | 1.77 |
|  | Southwest | 1.23 | 1.16 | −5 | 1.79 |
|  | Northwest | 1.41 | 1.25 | −9 | 2.59 |
|  | California | 1.36 | 1.21 | −8 | 2.42 |

**Table S5. Estimated harvest-related crop production losses due to epidemic-induced workforce illness.** For each crop, we report the worst-case epidemic peak timing (the day of the year on which the community symptomatic peak would cause the largest production loss), the corresponding maximum production loss as a percentage of total annual production, and the estimated dollar value of that loss based on 2024 California crop values. Values assume half of all infections are sufficiently symptomatic to cause missed work ($p_\text{symp} = 0.5$). 

| Crop | 2024 value (USD) | Worst peak day | Max loss (%) | Max loss (USD) |
|:---|---:|:---:|:---:|---:|
| Strawberries | $3,456,522,000 | 147 | 0.62% | $21,511,907 |
| Iceberg lettuce | $1,245,105,000 | 148 | 0.50% | $6,257,962 |
| Oranges | $852,507,000 | 29 | 0.50% | $4,275,115 |