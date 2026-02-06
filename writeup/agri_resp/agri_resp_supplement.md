
## Acknowledgments

## Funding

## Author contributions

## Competing interests

## Data availability

## References

## Supplementary Information

### Supplementary Methods

#### Data 

Agricultural workers were defined as individuals employed in “farming, fishing, and forestry occupations” (ACS occupation codes C24030_004 [males] and C24030_031 [females]). 

To enable region-level analysis, we aggregated the county-level data using population-weighted averages. [methods here]

Cross-referencing of crop movement data with UCDavis information, with figure. 

#### Mathematical model formulation

Deriving $\tau$ and $\tau_{boost}$ from the SAR. 

Mathematical formula giving the crowding probability of a household with size x, given constraints. 

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

**County-level NAWS imputation:** The NAWS dataset reports household characteristics for agricultural workers at the regional level only, while the ACS provides county-level data for the general population. To generate county-level estimates for agricultural workers, we used county-level ACS variation to adjust the regional NAWS values. The underlying assumption is that county-level variation among agricultural workers follows a similar pattern to county-level variation in the general population—i.e., if a county's general population has larger households than the regional average, agricultural workers in that county likely also have larger households than the regional average for agricultural workers.

For each county $i$ in region $r$, we first computed the regional mean of the county-level ACS values:
$$\bar{p}_{\text{ACS},r}(n) = \frac{1}{|r|} \sum_{i \in r} p_{\text{ACS},i}(n)$$
$$\bar{q}_{\text{ACS},r} = \frac{1}{|r|} \sum_{i \in r} q_{\text{ACS},i}$$

where $p_{\text{ACS},i}(n)$ is the proportion of households with size $n$ in county $i$ according to ACS data, and $q_{\text{ACS},i}$ is the proportion of crowded households in county $i$.

We then imputed county-level NAWS values using one of three methods:

*Multiplicative method.* We scaled regional NAWS values by the ratio of county-level to regional mean ACS values:
$$\tilde{p}_{\text{NAWS},i}(n) \propto p_{\text{NAWS},r}(n) \times \frac{p_{\text{ACS},i}(n)}{\bar{p}_{\text{ACS},r}(n)}$$
$$\tilde{q}_{\text{NAWS},i} = q_{\text{NAWS},r} \times \frac{q_{\text{ACS},i}}{\bar{q}_{\text{ACS},r}}$$

The household size distribution was renormalized to sum to 1, and the crowding proportion was clamped to the interval $[0, 1]$.

*Additive method.* We shifted regional NAWS values by the difference between county-level and regional mean ACS values:
$$\tilde{p}_{\text{NAWS},i}(n) \propto \max\left(0, \; p_{\text{NAWS},r}(n) + \left[ p_{\text{ACS},i}(n) - \bar{p}_{\text{ACS},r}(n) \right] \right)$$
$$\tilde{q}_{\text{NAWS},i} = q_{\text{NAWS},r} + \left[ q_{\text{ACS},i} - \bar{q}_{\text{ACS},r} \right]$$

Household size proportions were clamped to be non-negative before renormalization, and the crowding proportion was clamped to $[0, 1]$.

*Null method.* We used regional NAWS values directly without adjustment:
$$\tilde{p}_{\text{NAWS},i}(n) = p_{\text{NAWS},r}(n)$$
$$\tilde{q}_{\text{NAWS},i} = q_{\text{NAWS},r}$$

This method assumes no county-level variation in agricultural worker household characteristics within a region.

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