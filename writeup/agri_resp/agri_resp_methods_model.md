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

For the baseline assortativity, we used $\eta = 2/3$, reflecting moderate assortativity where agricultural workers have preferential within-group contact but still interact with the general population. At this baseline, agricultural workers have approximately 67% of their between-household contacts within their own group and 33% with the general community. The general community, being much larger, has nearly all contacts (>99%) within their own group. In sensitivity analyses, we considered $\eta \in \{0, 1/4, 1/3, 1/2, 3/4\}$, corresponding to agricultural workers having 0%, 25%, 33%, 50%, and 75% of contacts within their own group, respectively.

We assumed symptoms began one day after infection and lasted for three days. For the crop productivity analysis, we computed production losses assuming all infections were symptomatic ($p_{symp} = 1$); because production loss scales linearly with $p_{symp}$, results for any other symptomatic probability can be obtained by multiplying the reported losses by $p_{symp}$.

The transmission model requires knowing the fraction of households of each size are crowded, but the ACS and NAWS data report household size distribution and crowding proportion separately. To assign crowding levels by household size, we assumed that the crowding probability increased linearly from households of size 2 to households of size 7+ (since households of size 1 by definition cannot be crowded). We obeyed the constraints that (1) the overall proportion of crowded households must match the ACS- or NAWS-reported proportion, and (2) households of size 7+ are $p$ times as likely to be crowded as households of size 2 (**Supplementary Methods**). We used a baseline of $p = 2$ (i.e. households of size 7+ are twice as likely to be crowded as households of size 2). In sensitivity analyses, we considered $p = 1$ and $p = 3$. 



#### Model implementation

We implemented the transmission model in `R` (version 4.5.0) using `odin` (version 1.2.7). We initialized outbreaks by setting 0.1% of individuals in each sub-population as infectious. We distributed the initial infectious individuals proportionally across household types, equivalent to uniform-randomly choosing the initial infected individuals from each sub-population. We simulated outbreaks over 365 days. 

In addition to the main regional simulations, we also generated county-level simulations to explore differences in transmission between agricultural workers and the general population at a finer geographic scale. Since the NAWS dataset does not report at the county level, we imputed county-level household size distributions and crowding proportions for agricultural workers using three different methods: an "additive" method, where county-level NAWS values were imputed by adding the difference between the county-level and regional ACS values to the regional NAWS value; a "multiplicative" method, where regional NAWS values were scaled by the ratio of county-level to regional ACS values; and a "null" method, where regional NAWS values were used without adjustment. The "null" method yielded no variation in county-level agricultural household characteristics within a region; the "additive" method yielded an intermediate amount of variation; and the "multiplicative" method yielded a high amount of variation (**Supplementary Figure xx**). We treated the "additive" method as a baseline and considered the "null" and "multiplicative" methods in sensitivity analyses. Due to the high uncertainty in these imputation methods, we emphasize that these county-level results have a low level of confidence and are intended to provide a rough estimate of within-region variation around the regional mean. 

