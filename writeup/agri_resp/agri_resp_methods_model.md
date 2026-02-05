### Disease transmission model

#### Model structure

We simulated respiratory disease transmission using a deterministic household-structured susceptible-infectious-recovered (SIR) model based on a previously developed framework [x]. We split the population at the household level into "agricultural workers" and "general community", assuming the proportion of households belonging to agricultural workers was equal to the proportion of the working population involved in agricultural work according to the ACS data. Besides the impact of household size, household crowding, and assortative mixing, we did not assume any additional differences in transmission rates between these two sub-populations. 

We assumed that mixing among agricultural workers (A) and the general community (C) was assortative, governed by parameter $\epsilon$. We modeled this using the mixing matrix 

$$ M = m_{CC}, m_{CA}, m_{AC}, m_{AA} = $$
$$ (1-\epsilon) + \epsilon w_C, \epsilon w_A$$
$$ \epsilon w_C, (1-\epsilon) + \epsilon w_A$$

Here, $w_C$ is the fraction of the region's population made up by the general community and $w_A$ is the fraction of the population made up by agricultural workers. This matrix modulates the force of infection $\lambda$ experienced by each population such that 

$$\lambda_C = \beta (m_{CC} I_C + m_{CA} I_A)$$ 
$$\lambda_A = \beta (m_{AC} I_C + m_{AA} I_A)$$

where $\lambda_i$ is the force of infection for members of sub-population $i$, $\beta$ is the transmission constant, and $I_i$ is the proportion of infectious individuals in sub-population $i$; thus, $\epsilon = 0$ implies completely assortative mixing and $\epsilon = 1$ implies mixing proportional to each sub-population's size. 

We assumed that the household secondary attack rate (the probability of infection given an infected household member) was higher in crowded households than in uncrowded households. For full details on the model structure, see the **Supplementary Methods.** 

#### Model parameterization

The transmission model has three main epidemiological parameters: the transmission rate for non-houshold contacts ($\beta$), the within-household transmission rate ($\tau$), and the recovery rate ($\gamma$). Following previous methods [x], we began by fixing the recovery rate $\gamma = 1/5$, which corresponds to a mean infectious period of 5 days. Then, given $\gamma$ and the household secondary attack rate (SAR), we derived $\tau$ (**Supplemental Methods**). We set the SAR for uncrowded households at 20%, following estimates for influenza [x]. For crowded households, we set the baseline SAR at 40% [x]. In sensitivity analyses, we considered crowded-household SARs of xx - xx. Last, given values for $\gamma$ and $\tau$, we numerically identified the value of $\beta$ that would achieve a desired basic reproduction number ($\mathcal{R_0}$) when simulating outbreaks at the national level. Specifically, for a candidate $\beta$ value, we ran the model with a single sub-population to equilibrium; then, we compared the outbreak's final size with the theoretical prediction from the implicit relationship $R(\infty) = 1 - \exp(-\mathcal{R_0} \cdot R(\infty))$, where $R(\infty)$ is the final size of the outbreak. [x] We adjusted $\beta$ using a bisection search algorithm until the simulated final size was within 0.0005 of the theoretical value. For the baseline analysis, we used $\mathcal{R_0}$ = 1.5, reflecting a moderate pandemic influenza scenario and the effective reproduction number during many COVID-19 surges when behavioral mitigations were in place. In sensitivity analyses, we considered $\mathcal{R_0}$ values of 1.2, 2.0, and 3.0. Baseline and sensitivity parameter values are listed in **Supplementary Table XX**. 

The transmission model requires knowing the fraction of households of each size are crowded, but the ACS and NAWS data report household size distribution and crowding proportion separately. To assign crowding levels by household size, we assumed that the crowding probability increased linearly from households of size 2 to households of size 7+ (since households of size 1 by definition cannot be crowded). We obeyed the constraints that (1) the overall proportion of crowded households must match the ACS- or NAWS-reported proportion, and (2) households of size 7+ are $p$ times as likely to be crowded as households of size 2 (**Supplementary Methods**). We used a baseline of $p = 2$ (i.e. households of size 7+ are twice as likely to be crowded as households of size 1). In sensitivity analyses, we considered $p = 1$ and $p = 3$. 

For the baseline analysis, we used $\varepsilon = 0.33$, reflecting moderate assortativity where agricultural workers have preferential within-group contact but still interact with the general population. [Something about what fraction of contacts happen within-group vs. outside-of-group for agricultural workers and general community members, with ranges.]

#### Model implementation

We implemented the transmission model in `R` (version xxx) using `odin` (version xxx). We initialized outbreaks by setting 0.1% of individuals in each sub-population as infectious. We distributed the initial infectious individuals proportionally across household types, equivalent to uniform-randomly choosing the initial infected individuals. We simulated outbreaks over 365 days. 

In addition to the main regional simulations, we also generated county-level simulations to explore transmission among agricultural workers at a finer geographic scale. Since the NAWS dataset does not report at the county level, we imputed county-level household size distributions and crowding proportions for agricultural workers using three different methods: and "additive" method, where xxx; a "multiplicative" method, where xxx; and a "null" method, where xxx. Due to the high uncertainty in these imputation methods, we emphasize that these county-level analyses have a low level of confidence. 
