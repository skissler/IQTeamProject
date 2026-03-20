# Simulated Peer Reviews

**Manuscript:** "Modeling the impact of respiratory illness outbreaks on the agricultural workforce and food production in the United States"

**Date:** 2026-03-20

---

## Reviewer 1

### Summary

This manuscript presents a household-structured SIR transmission model that quantifies the differential impact of respiratory disease outbreaks on agricultural workers compared to the general U.S. population. Using data from the ACS and NAWS, the authors demonstrate that differences in household size and crowding alone can produce substantial disparities in disease burden, and they translate these disparities into crop-specific production losses for three labor-intensive California crops. The paper fills an important gap between retrospective COVID-19 analyses and prospective planning tools for agricultural sector preparedness.

### General Assessment

This is a well-conceived and carefully executed study that addresses a timely and policy-relevant question. The strengths of the paper are considerable: the authors have assembled a coherent analytical pipeline from national survey data through epidemic modeling to economic impact estimation, and the sensitivity analyses are thorough. The writing is clear and the limitations are honestly discussed. I recommend publication with minor revisions.

### Major Strengths

1. **Novel framing.** While several papers have documented elevated COVID-19 rates among agricultural workers retrospectively, this study provides a *prospective* and *mechanistic* framework. The explicit linking of household structure (a measurable, upstream determinant) to epidemic dynamics and downstream food production impacts is a valuable contribution. This framing is generalizable beyond influenza to any future respiratory pathogen.

2. **Data integration.** The combination of ACS county-level data with NAWS regional data, including the thoughtful treatment of the mismatch in geographic resolution through three imputation methods, demonstrates methodological care. The authors are appropriately transparent about the limitations of each approach.

3. **Comprehensive sensitivity analysis.** The one-at-a-time sensitivity analysis across six dimensions (R0, assortativity, crowded-household SAR, crowding fold-difference, infectious period, and seeding) provides a clear picture of which parameters drive the results and which do not. The finding that epidemic *sizes* are invariant to the infectious period while *timing* scales proportionally is a clean and interpretable result.

4. **Crop impact analysis.** Translating epidemic dynamics into crop-specific production losses through the workforce availability framework is practical and actionable. The validation of crop movement data against independent UC Cooperative Extension reports adds credibility.

5. **Interactive tool.** The accompanying interactive simulation tool extends the utility of this work beyond the specific scenarios analyzed in the paper.

### Minor Comments

1. **Results on gamma sensitivity are incomplete.** The results paragraph (p. 4) mentions that "long infectious periods (gamma = 10 days) led to greater differences in peak timing" but does not describe the complementary finding for the short infectious period (gamma = 1/3, ~3 days). The data show that timing differences compress to 3-6 days with a 3-day infectious period, completing the picture. Both directions should be mentioned.

2. **Gamma notation.** The phrase "gamma = 10 days" is imprecise; gamma is a rate (1/10 per day), not a duration. Consider "a 10-day infectious period (gamma = 1/10)" for consistency with the notation used elsewhere.

3. **Figure S4 caption.** The caption describes four sensitivity dimensions (R0, eta, SAR, d) but the actual figure contains five panels (the fifth being "Infectious Period"). The caption should be updated.

4. **Table S2 completeness.** Table S2 does not include rows for the gamma sensitivity, despite the fact that varying gamma changes tau, tau_boost, and beta (which is recalibrated). These rows should be added for reproducibility. The seed sensitivity does not change parameter values (only initial conditions), which could be noted with a footnote.

5. **Supplementary figure coverage.** Cumulative infection curves are provided for R0 (S5), eta (S6), SAR (S7), and fold-difference (S8), but not for the gamma or seed dimensions, despite these figures existing in the repository. Consider adding them for completeness, or noting their omission.

6. **Seed sensitivity in the Results.** The finding that seeding location makes no difference is mentioned only parenthetically in the Methods (Model Implementation). This is a useful result for readers designing preparedness models and would benefit from a brief mention in the Results.

7. **County-level analysis.** The county-level results are presented with appropriate caveats about the imputation methods. However, it might help readers to see the county-level results reported with all three imputation methods side by side in a supplementary table, rather than only the "additive" baseline in the main text with the others in supplementary figures.

8. **Minor typographic issues.** The sentence "the simulated epidemics were also largely insensitive to the fold-difference in crowding between the largest and smallest households" (end of the sensitivity results paragraph) is missing a period.

### Recommendation

**Accept with minor revisions.** The manuscript makes a meaningful contribution to agricultural preparedness planning. The requested changes are primarily about completeness and presentation rather than substance.

---

## Reviewer 2

### Summary

The authors develop a household-structured SIR model to argue that differences in household size and crowding between agricultural workers and the general U.S. population lead to disproportionate disease burden during respiratory outbreaks. They estimate modest crop production losses (0.50-0.62%) for three California crops. While the question is relevant, I have significant concerns about the novelty of the approach, the realism of the model, and the practical significance of the findings.

### Major Concerns

#### 1. Limited novelty over existing work

The core finding -- that household crowding elevates respiratory disease risk among agricultural workers -- is already well established empirically. Mora et al. (2021, the authors' reference 15) demonstrated this directly for SARS-CoV-2 among farmworkers in Monterey County, and Lewnard et al. (2021, reference 24) documented elevated prevalence in the same population. The House & Keeling (2009) household-structured model is nearly two decades old. The contribution here is assembling these known pieces (established model + known risk factors + available survey data), but the mechanistic insight is modest: larger, more crowded households transmit more disease. This is the expected result from the model structure itself and does not require simulation to anticipate.

The authors should more clearly articulate what this study reveals that was not already known or strongly suspected. The quantitative precision (e.g., "1.23 to 1.45 times higher peak prevalence") is useful, but only to the extent that the model inputs are trustworthy -- and several key inputs (assortativity, crowded-household SAR) are essentially assumed rather than estimated from data.

#### 2. The deterministic model ignores critical stochastic dynamics

Agricultural workers constitute 0.7-2.2% of the regional population. At this small population fraction, stochastic effects are likely to be substantial, particularly in the early epidemic phase. A deterministic ODE model cannot capture the probability that an outbreak fails to establish in the agricultural worker subpopulation, or the variance in epidemic trajectories conditional on establishment. The authors acknowledge stochastic effects in the Limitations but do not attempt even a simple stochastic sensitivity analysis (e.g., using a Gillespie algorithm or tau-leaping). Given that the main claim is about *differential* dynamics between two populations of very different sizes, this is a significant gap.

Furthermore, the deterministic model seeds 0.1% of both populations simultaneously, guaranteeing synchronized epidemics from the start. In reality, the timing of introduction into the agricultural worker community is uncertain and could substantially affect the relative timing of peaks. The finding that "results were virtually unchanged when seeding the epidemic in just one of the two sub-populations" is an artifact of the deterministic framework: even a single infected individual in the ODE model generates a smooth exponential growth trajectory. In a stochastic model, the number and timing of introductions would matter considerably.

#### 3. The production loss estimates are too small to be policy-relevant

The headline production losses are 0.50-0.62% of annual harvest volume for the three crops considered, translating to $4-21 million USD. While the authors note these are for a "moderate pandemic influenza scenario," these losses are well within the range of normal inter-annual production variability caused by weather, pest pressure, and market fluctuations. For context, California strawberry production dropped approximately 10% in 2022 due to weather alone. A 0.62% loss would be statistically and practically indistinguishable from background variation.

The authors should either: (a) present scenarios with more severe pathogens where losses would be more meaningful, (b) explicitly acknowledge that the estimated losses for the baseline scenario are too small to drive policy on their own, or (c) emphasize that farm-level impacts could be much larger than the state-level averages (they briefly mention this in the Limitations, but it deserves more emphasis).

#### 4. The linear labor-to-production assumption is unjustified

The model assumes that a 1% reduction in available labor produces a 1% reduction in harvest output. This is acknowledged in the Limitations but not explored. In practice, harvest operations have nonlinear dynamics:

- **Threshold effects:** Many harvest operations require minimum crew sizes (e.g., to staff a packing line). Below these thresholds, productivity drops to zero, not proportionally.
- **Substitution effects:** Farms routinely adjust to labor shortfalls by hiring from labor contractors, extending work hours, or prioritizing the most valuable blocks. These compensatory mechanisms would reduce the actual production impact below the linear estimate.
- **Perishability constraints:** For strawberries and lettuce, unharvested fruit spoils within days, meaning labor shortfalls during peak harvest cannot be "made up" later. For oranges, which can remain on the tree for weeks, labor shortfalls are more easily buffered. The authors mention this distinction qualitatively but do not model it.

Without exploring these nonlinearities, even approximately, the production loss estimates are difficult to interpret. A simple sensitivity analysis on the labor-production elasticity (e.g., comparing linear, convex, and concave relationships) would substantially improve the paper.

#### 5. The assortativity parameter is unconstrained and consequential

The baseline assortativity parameter (eta = 2/3) is chosen without empirical basis. The authors state it reflects "moderate preferential mixing within population groups" and that "empirical estimates of occupational assortative mixing are not available for agricultural workers." While the sensitivity analysis explores eta from 0 to 3/4, the results are quite sensitive to this parameter: the peak prevalence ratio ranges from 1.21 (eta = 0) to 1.52 (eta = 0.75) for the East region, and from 1.40 to 3.14 for the max relative infection measure. This 2-3x range across plausible eta values means the model is essentially uncalibrated for this critical parameter.

The baseline choice of eta = 2/3 is also poorly motivated. Agricultural workers share transportation, housing, and recreational spaces predominantly with other agricultural workers, suggesting eta could be quite high. But they also interact with the broader community through shopping, healthcare, and schools (if they have families), suggesting substantial between-group mixing. The authors should provide a more rigorous discussion of what contact tracing or time-use data might inform this parameter, or consider presenting results as ranges across eta rather than defaulting to a single baseline.

#### 6. The model omits behavioral and intervention responses

Real epidemics do not unfold in a behavioral vacuum. During COVID-19, agricultural operations implemented social distancing measures, mask mandates, housing modifications, testing programs, and vaccination campaigns -- all of which modified the trajectory. The model assumes no behavioral change throughout the epidemic, which is unrealistic for any scenario severe enough to produce the losses described. This is a standard limitation of compartmental models, but it is particularly problematic here because the policy question is "what can be done?" and the model cannot address counterfactuals involving interventions.

#### 7. Missing validation against observed COVID-19 outcomes

The authors cite Lusk & Chandra (2021), who estimated a 0.069% reduction in farm labor input from COVID-19, and note their estimates are "an order of magnitude higher." However, this comparison is not developed. Given that COVID-19 data on agricultural worker infection rates, hospitalization, and labor outcomes now exist, the authors should attempt to validate (or at least calibrate) their model against observed COVID-19 outcomes in at least one region. This would substantially strengthen confidence in the model's predictions for future outbreaks.

### Minor Concerns

1. The pairwise SAR formula (tau = SAR * gamma / (1 - SAR)) assumes a single susceptible individual exposed to a single infectious individual. As the authors note, realized household attack rates can exceed this pairwise value due to chains of within-household transmission. However, the distinction between the pairwise SAR used for calibration and the realized household attack rate in the simulation is not made clear in the main text Methods.

2. The paper focuses exclusively on California for the crop impact analysis. While this is justified by California's dominance in fruit and vegetable production, it limits the generalizability of the crop impact findings. Agricultural labor markets and crop calendars differ substantially in other major producing regions (Florida citrus, Pacific Northwest tree fruits, Midwest grain).

3. The "3.2 million" agricultural workers figure (reference 7) likely refers to all hired farmworkers, including those in livestock, nurseries, and other sectors. The NAWS data used here may capture a different subset (primarily crop workers). The authors should clarify which population their model represents.

4. The mixing matrix assumes that agricultural workers and the general community share the same beta (between-household transmission rate). This means all differential transmission comes from household structure. In practice, workplace transmission in agricultural settings (shared transport, field work, communal meals) could independently elevate between-household transmission for agricultural workers, which the model does not capture.

### Recommendation

**Major revision.** The study addresses a relevant question, but the model's inability to capture stochastic dynamics in a small subpopulation, the lack of empirical grounding for key parameters, the very small estimated production losses, and the absence of validation against observed data substantially limit the contribution. I would reconsider after the authors address the stochastic modeling concern, provide more context for the production loss magnitudes, and attempt at least partial validation against COVID-19 data.

---

## Reviewer 3

### Summary

I was asked to provide a focused review of the statistical and computational methods. I examined the mathematical model, the calibration procedure, the parameter derivations, the sensitivity analysis design, and the data processing pipeline.

### Model Specification and Mathematical Correctness

**Household-structured SIR.** The model is a well-established framework (House & Keeling, 2009) applied to a two-population setting. The state space tracks H_k(x, y, z, c) -- the number of households in population k with x susceptible, y infected, z recovered members, and crowding status c. The ODE system is standard and appears correctly specified. The transition rates for recovery (gamma * y), within-household infection (tau_c * x * y), and between-household infection (lambda_k * x) are consistent with the standard formulation.

**Within-household transmission rate derivation.** The derivation of tau from the pairwise SAR uses:

SAR = tau / (tau + gamma), yielding tau = SAR * gamma / (1 - SAR)

This is correct for the probability that a single susceptible is infected by a single infectious individual in a two-person household. The authors correctly note (in the Supplementary Methods) that this is a *pairwise* SAR and that realized attack rates in larger households will exceed this value due to multiple generations of transmission. This is an important clarification that should be more prominent.

**Numerical check on tau and tau_boost:**
- For SAR = 0.20, gamma = 1/5: tau = 0.20 * 0.20 / 0.80 = 0.05. Matches Table S2.
- For SAR = 0.40, gamma = 1/5: tau_crowded = 0.40 * 0.20 / 0.60 = 0.1333; tau_boost = 0.1333 - 0.05 = 0.0833. Matches Table S2.
- For SAR = 0.20, gamma = 1/3: tau = 0.20 * 0.3333 / 0.80 = 0.0833. Consistent.
- For SAR = 0.40, gamma = 1/10: tau_crowded = 0.40 * 0.10 / 0.60 = 0.0667; tau_boost = 0.0667 - 0.025 = 0.0417. Consistent.

**Note:** Table S2 is currently missing the gamma sensitivity rows. Since tau and tau_boost are gamma-dependent, and beta is recalibrated for each gamma value, these rows contain distinct parameter values that readers need for reproducibility. This should be corrected.

**Mixing matrix.** The mixing matrix is correctly specified and satisfies the required symmetry property w_C * m_CA = w_A * m_AC (both equal (1 - eta) * w_A * w_C), ensuring consistent contact rates between populations. The manuscript's discussion of the matrix elements as a function of eta and w_A (including the approximation m_AA ~ eta when w_A is small) is accurate.

### Calibration Procedure

**Beta calibration via final size matching.** The authors calibrate beta by matching the simulated final size to the theoretical SIR final size relationship R_inf = 1 - exp(-R0 * R_inf). This is a standard and sound approach. Key details:

- Calibration uses a bisection search with convergence criterion |simulated - theoretical| < 0.0005.
- Calibration is performed with a single population (pop_A = 0) at the national level, using population-weighted average household distributions.
- Beta is recalibrated for each combination of (R0, SAR_crowded, fold_diff, gamma), reflecting that these parameters change the within-household transmission dynamics and thus the required beta to achieve a target R0.

**Strength:** The calibration approach ensures internal consistency -- the simulated R0 matches the nominal R0 for each parameter combination. The one-population calibration is appropriate because beta represents between-household transmission, which should not differ between populations.

**Concern:** The final size formula R_inf = 1 - exp(-R0 * R_inf) is exact for a simple SIR model but is an approximation for a household-structured model where within-household transmission creates correlations. The authors are effectively defining their R0 operationally (as the R0 of a simple SIR with the same final size), which is a pragmatic choice but not equivalent to the true basic reproduction number of the household model. This should be acknowledged, though it does not invalidate the approach.

**Numerical verification.** I spot-checked several beta values from the calibration output (calibrated_betas.csv) against Table S2:
- R0 = 1.5, baseline: beta = 0.2108 (CSV: 0.210775). Matches after rounding.
- R0 = 1.2: beta = 0.1546 (CSV: 0.154557). Matches.
- R0 = 3.0: beta = 0.5054 (CSV: 0.505402). Matches.
- SAR = 60%: beta = 0.2086 (CSV: 0.208580). Matches.
All checked values are consistent.

### Sensitivity Analysis Design

**One-at-a-time (OAT) design.** The sensitivity analysis varies one parameter dimension at a time while holding all others at baseline. This generates 19 parameter sets across 6 dimensions (4 R0 + 5 eps + 4 SAR + 2 fold + 2 gamma + 2 seed).

**Strengths of OAT:** The design is interpretable and standard for exploratory sensitivity analysis. It clearly isolates the marginal effect of each parameter. The number of runs (19) is manageable and allows complete documentation in supplementary tables.

**Limitation of OAT:** OAT analysis cannot detect interactions between parameters. For example, the effect of assortativity (eta) may be different at R0 = 1.2 vs. R0 = 3.0 -- high assortativity matters more when the epidemic is smaller and the two populations' trajectories have more room to diverge. A factorial design or Latin hypercube sampling across the full parameter space would reveal such interactions, but at a much higher computational cost. The authors should note this limitation.

**Coverage of parameter space:** The sensitivity ranges appear reasonable:
- R0: 1.2 to 3.0 covers mild to severe pandemic scenarios.
- Eta: 0 to 0.75 covers proportional to strongly assortative mixing.
- SAR (crowded): 20% to 60% spans no crowding effect to a large one.
- Fold difference: 1 to 3 is a modest range (see below).
- Gamma: 1/3 to 1/10 covers 3-day to 10-day infectious periods, appropriate for influenza-like illnesses.
- Seed: C only, both, A only -- comprehensive for this binary dimension.

**Missing sensitivity dimension:** The initial prevalence (0.1%) and symptomatic fraction (p_symp = 0.5) are held fixed throughout. Since p_symp scales the production loss linearly, the authors correctly note that results for any p_symp can be obtained by rescaling. However, the initial prevalence could affect the relative timing of epidemics in the two populations, particularly in a stochastic setting. A brief justification for 0.1% as representative would be useful.

### Data Processing and Statistical Methods

**Population-weighted regional aggregation.** The aggregation of county-level ACS data to NAWS regions using population weights is standard and appropriate. The normalization of household size proportions after aggregation is necessary and correctly implemented.

**County-level imputation.** The three imputation methods (additive, multiplicative, null) for assigning county-level agricultural worker household characteristics are well-motivated as a sensitivity check. The additive method preserves absolute differences; the multiplicative method preserves relative differences; the null method provides a conservative lower bound on within-region variation. Clamping negative proportions to zero and re-normalizing is appropriate.

**However,** none of the three methods accounts for the possibility that the *relationship* between general-population and agricultural-worker household characteristics may vary across counties. For example, in counties with large agricultural operations, the agricultural workers might have household characteristics that diverge more from the general population than in counties where agriculture is a smaller sector. This is a structural limitation of using the ACS as a proxy for within-region variation in NAWS characteristics.

**Crowding-by-household-size assignment.** The linear interpolation w(n) = 1 + (d-1)(n-2)/5 for n >= 2 is a simple and transparent assumption. The constraint that the weighted mean matches the observed crowding proportion is correctly implemented. The results are reassuringly insensitive to d (the fold-difference), suggesting this assumption does not drive the findings. However, the functional form is purely assumed -- empirical data on crowding by household size (even aggregate national data) would strengthen this.

**Crop movement data processing.** The use of weekly point-to-point crop shipments as a harvest proxy is pragmatic. Averaging across 7 years mitigates inter-annual variability. The linear interpolation to daily values (equal within each week) introduces a minor smoothing artifact but is unlikely to affect results meaningfully given that epidemic dynamics operate on multi-week timescales.

### Numerical Outputs

I verified a sample of values from sensitivity_summary.csv and sensitivity_differential.csv against the manuscript's Tables S3 and S4. All checked values (approximately 30 spot-checks across both tables) match after appropriate rounding. The rounding convention is consistent: percentages to one decimal place, ratios to two decimal places, time in integer days.

**Gamma sensitivity results verify the claim** that peak prevalences and final sizes are invariant to gamma while timing scales proportionally:
- Baseline timing: peak A at day 39-50, peak C at day 47-56.
- Gamma = 1/3: peak A at day 23-30, peak C at day 28-34 (~0.6x baseline, consistent with 3/5 ratio).
- Gamma = 1/10: peak A at day 78-101, peak C at day 93-112 (~2x baseline, consistent with 10/5 ratio).

Attack rates are unchanged to the reported precision (e.g., baseline California A = 76.7%, gamma = 1/3 California A = 76.7%, gamma = 1/10 California A = 76.7%). This is mathematically expected: the final size depends on R0 (which is held constant via recalibration of beta) and within-household SAR (which is held constant via the SAR formula, since tau * (1/gamma) = SAR / (1-SAR) is preserved when tau and gamma change proportionally). The code correctly implements this by recalibrating beta for each gamma value.

**Seed sensitivity results** are identical to baseline for all metrics, confirming that initial seeding location does not affect deterministic outcomes at 0.1% initial prevalence. This is expected in a deterministic model where the subpopulations are coupled and the initial perturbation is small relative to population size.

### Code Quality Assessment

Based on review of config.R, parameters.R, calibrate_model.R, and sensitivity_analysis.R:

- The codebase is well-organized with a clear separation of configuration, calibration, simulation, and analysis.
- The parameter set factory (create_parset in parameters.R) ensures consistent parameter derivation across all sensitivity runs.
- The calibration grid uses a one-at-a-time filter to avoid unnecessary calibration runs while ensuring all needed parameter combinations are covered.
- Float comparison for gamma uses an epsilon tolerance (abs(gamma - target) < 1e-10), avoiding floating-point equality issues.
- The use of renv for dependency management ensures reproducibility.

### Summary of Recommendations

1. **Add gamma sensitivity rows to Table S2.** These rows involve distinct values of tau, tau_boost, beta, and gamma that readers need for reproducibility.

2. **Acknowledge the operational definition of R0.** The calibration matches the simple SIR final size formula, which is an approximation for the household-structured model. The effective R0 of the household model is not identical to that of a simple SIR with the same final size.

3. **Note the OAT limitation.** The one-at-a-time design cannot detect parameter interactions. A brief statement acknowledging this and noting that the most policy-relevant parameter pairs (e.g., R0 x eta, R0 x SAR) could be explored in future work would be appropriate.

4. **Strengthen the gamma sensitivity discussion.** The mathematical reason that attack rates are preserved under gamma variation (tau/gamma ratio is preserved, and beta is recalibrated to maintain R0) is elegant and worth noting explicitly, as it provides insight into the model's structure.

5. **Consider the implications of deterministic modeling for the seed sensitivity.** The null result for seeding is expected in a deterministic framework and may not hold in a stochastic model where the timing and probability of establishment in the smaller agricultural worker subpopulation depends on the seeding protocol.

### Overall Assessment

The quantitative methods are sound and carefully implemented. The calibration procedure is internally consistent, the sensitivity analysis is comprehensive within the OAT framework, and the numerical outputs are verified. The main quantitative limitations are the lack of interaction effects in the sensitivity design and the deterministic treatment of a small subpopulation where stochastic effects may be non-negligible. Neither of these invalidates the results, but both should be discussed.
