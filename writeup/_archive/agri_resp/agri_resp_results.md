
## Results

### Household size and crowding lead to higher modeled disease prevalence among agricultural workers.

Agricultural worker households are substantially larger and more crowded on average than the general U.S. population (**Figure 1, Supplementary Table S1**). The mean household size among agricultural workers ranged from 3.3 to 4.1 people across regions, compared to 2.4 to 2.8 people for the general population. The proportion of households of size 4 or greater ranged from 41% to 62% among agricultural workers vs. 20% to 30% for the general population. The proportion of crowded households ranged from 11.2%-32.8% for agricultural workers compared to 1.9%-8.3% in the general population. This translated into crowding rates that were 3.3 to 8.6 times higher among agricultural workers than the general population across the six regions. 

Simulations of respiratory disease outbreaks at the regional level revealed consistently higher disease burden among agricultural workers than in the general population (**Figure 2, Supplementary Tables S3-S4**). Under baseline assumptions ($R_0$ = 1.5, SAR = 20%/40% for uncrowded/crowded households, $\eta$ = 2/3), peak prevalence among agricultural workers was 1.23 to 1.45 times that of the general population across regions. Final sizes were 1.15 to 1.28 times higher among agricultural workers, with final sizes of 66–77% among agricultural workers compared to 56–64% in the general population. Outbreaks peaked between 5 and 12 days earlier in agricultural workers across regions. At the point of maximum prevalence difference, the prevalence in agricultural workers was 1.74 to 2.78 times higher in agricultural workers vs. the general community. 

These differences were sensitive to the basic reproduction number (**Supplementary Figures S4-S5, Supplementary Tables S3-S4**). At $R_0$ = 1.2, final size ratios were largest (1.36–1.76 times) despite the overall lower disease burden, and peak prevalence ratios ranged from 1.46 to 2.18 times. The final size at $R_0 = 1.2$ was 40–59% for agricultural workers and 28–40% for the general community. At higher transmissibility ($R_0$ = 2.0), peak prevalence ratios narrowed to 1.12–1.21 times, and final size ratios to 1.06–1.10 times as both populations approached high overall infection levels. At $R_0$ = 3.0, with near-complete infection of both populations, final size ratios were 1.01–1.02 times higher in agricultural workers, while peak prevalence ratios were 1.05–1.09 times higher.

Increasing SAR in crowded households generally led to greater differences in final size, peak prevalence, and peak timing. Increasing assortativity (more within-group mixing; $\eta \rightarrow 1$) generally had a similar effect. The simulated epidemics were largely insensitive to the fold-difference in crowding between the largest and smallest households ($d$) (**Supplementary Figures S4–S8, Supplementary Tables S3-S4**)

County-level simulations demonstrated geographic heterogeneity in these infection disparities. Under the baseline parameter values, the median [20th, 80th percentile] county-level peak prevalence ratio ranged from 1.25 [1.22, 1.28] in the Midwest to 1.46 [1.37, 1.56] in the Northwest. Similarly, the median [20th, 80th percentile] county-level final size ratio ranged from 1.17 [1.14, 1.20] in the Southwest to 1.29 [1.25, 1.33] in the East. These results were sensitive to how household sizes and crowding rates were assigned to the agricultural worker population at the county level, with the "multiplicative" method generally yielding more variation in county-level simulations and the "null" method yielding less (**Supplementary Figures S9–S10**).

### Respiratory disease outbreaks among agricultural workers can lead to substantial productivity losses.

The simulated outbreaks yielded substantial productivity losses for all three crops we considered, with the impact varying by outbreak timing relative to peak harvest periods (**Figure 3, Supplementary Figure S13, Supplementary Table S5**). For strawberries, peak productivity losses were 0.62% with the worst outbreak timing being an epidemic peak on day 147 (approximately late May). For iceberg lettuce, maximum losses were 0.50% for outbreaks that peaked in late May (day 148). For oranges, peak losses were 0.50% for outbreaks peaking in late January (day 29). These translate into peak losses of roughly $21,511,907, $6,257,962, and $4,275,115 USD for strawberries, head lettuce, and oranges, respectively.

### Main figures

**Figure 1. Household characteristics by region for agricultural workers and the general community.** (A) Proportion of households of size 1 – 7+ for agricultural workers (blue) and the general community (red). Mean household sizes for each region and sub-population are depicted as circles. (B) Proportion of households that are crowded for agricultural workers (blue) and the general community (red). Histograms for the general community represent county-level differences in household crowding within each region. For agricultural workers, household crowding is available only at the region level, so these are depicted as single bars. Mean household crowding proportions for each region and sub-population are depicted as circles. Data for agricultural workers are extracted from the National Agricultural Workers Survey (NAWS) and data for the general community are extracted from the American Community Survey (ACS). 

![Household characteristics](../../figures/hh_inkscape.png)

**Figure 2. Simulated epidemic trajectories by region for agricultural workers and the general community.** Infection prevalence (A), cumulative infections (B), and ratio of agricultural worker to general community infection prevalence (C) for agricultural workers (blue) and the general community (red) in the six NAWS regions. Region-level simulations are depicted as thick lines with black outlines. County-level simulations are depicted as thin, partially transparent lines to illustrate within-region variation. 

![Epidemic simulations](../../figures/main_epicurves_2.png)

**Figure 3. Simulated impact of a respiratory virus outbreak on harvesting of strawberries, iceberg lettuce, and oranges in California.** (A) Illustration of the approach for calculating harvest impact. Here, an epidemic peaks in the general community on June 1st, leading to a peak in symptomatic disease among agricultural workers a few days earlier. The mean daily production of strawberries (magenta), iceberg lettuce (blue), and oranges (orange), averaged across 2018-2024, are depicted as solid lines. Dashed lines with shading depict the simulated production impact caused by the loss of labor due to symptomatic disease. The total impact (i.e., the area of the shaded regions) is summed across the year, yielding a single point in plot (B) representing the overall impact of an epidemic peaking on June 1. (B) Simulated production impact on strawberries (magenta), iceberg lettuce (blue), and oranges (orange) for epidemics peaking in the general community on each day of the year. These impacts assume that 50% of infections cause symptoms severe enough to cause a worker to miss work. 

![Crop impact](../../figures/crop_impact.png)

<!-- <div align="center">
  <img src="../../figures/crop_impact.png" width="50%">
</div> -->




<!---

2024 California head lettuce value: $1,245,105,000 (https://www.nass.usda.gov/Publications/Todays_Reports/reports/vegean25.pdf)
2024 California orange value: $852,507,000 (https://www.nass.usda.gov/Publications/Todays_Reports/reports/cfrt0825.pdf)
2024 California strawberry value: $3,456,522,000 (https://www.nass.usda.gov/Publications/Todays_Reports/reports/ncit0525.pdf)

Losses (p_symp = 0.5, using 2018-2024 averaged movements):
$21,511,907 strawberries (0.62%, peak day 147)
$6,257,962 head lettuce (0.50%, peak day 148)
$4,275,115 oranges (0.50%, peak day 29)
---> 

