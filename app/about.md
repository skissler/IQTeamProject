<style>
table { border-collapse: collapse; width: 100%; }
th, td { padding: 8px 16px; text-align: left; }
</style>

## About This Tool

This interactive application accompanies the manuscript "Modeling the impact of respiratory disease outbreaks on the United States agricultural workforce" by Bardsley, de Pablo, Keppler Canada, Ormaza Zulueta, Mehrabi, and Kissler. It simulates respiratory disease outbreaks using a household-structured susceptible-infectious-recovered (SIR) model, comparing disease dynamics between **agricultural workers** and the **general population** across six U.S. regions defined by the National Agricultural Workers Survey (NAWS).

### Motivation

Respiratory disease outbreaks can cause societal and economic disruptions that cascade beyond their direct public health burden. Agricultural workers face elevated transmission risk due to higher rates of household crowding and larger household sizes, which amplify within-household transmission of respiratory pathogens. During the COVID-19 pandemic, household crowding was identified as a primary risk factor for SARS-CoV-2 infection among farmworkers. This tool allows users to explore how differences in household structure, vaccination coverage, and comorbidity prevalence between agricultural workers and the general population translate into differential disease burden and downstream impacts on crop production.

### Model Structure

The model implements a deterministic household-structured SIR framework based on House & Keeling (2008):

1. **Household-level tracking**: The model tracks the distribution of households by composition (number susceptible, infected, and recovered), explicitly accounting for transmission within and between households of various sizes.

2. **Two transmission routes**:
   - *Within-household*: Infected members transmit to susceptible household members at rate &tau;, derived from the household secondary attack rate (SAR).
   - *Between-household*: Community-level transmission at rate &beta;, calibrated to achieve a target basic reproduction number (R&#8320;).

3. **Crowding effect**: Crowded households (>1 person per room) experience elevated within-household transmission. The crowding fold parameter controls how much more likely large households are to be crowded relative to small households.

4. **Two populations with assortative mixing**: Agricultural workers (A) and the general community (C) mix according to an assortativity parameter &eta;, where &eta; = 1 implies fully assortative mixing (groups interact only within themselves) and &eta; = 0 implies proportional mixing.

5. **Vaccination (leaky vaccine model)**: Each subpopulation's force of infection is multiplied by a susceptibility factor (1 &minus; efficacy &times; coverage). Vaccinated individuals remain susceptible but with a proportionally reduced infection rate. Agricultural workers and the general community can have different coverage levels.

6. **Symptomatic fraction (comorbidity model)**: Not everyone who is infected develops symptoms. The proportion symptomatic is derived from obesity prevalence and the odds ratio (OR) of obesity for symptomatic illness. A non-obese baseline symptomatic probability p&#8320; is back-solved so that the general community (40% obesity prevalence) yields an overall symptomatic fraction of 50%. The agricultural worker symptomatic fraction is then computed as the obesity-weighted mixture: p&#8347; = obs&#7424; &times; p&#8321; + (1 &minus; obs&#7424;) &times; p&#8320;, where p&#8321; = OR &times; p&#8320; / (1 + (OR &minus; 1) &times; p&#8320;).

### Parameters

#### Transmission

| Parameter | Description |
|-----------|-------------|
| **R&#8320;** | Basic reproduction number (1.5, 2.0, or 3.0). Baseline R&#8320; = 2.0 reflects a severe pandemic influenza scenario. |
| **Infectious period (1/&gamma;)** | Duration of infectiousness in days (3, 5, or 10). Baseline is 5 days, consistent with influenza. |
| **Assortativity (&eta;)** | Degree of preferential within-group mixing (0 to 1). Baseline &eta; = 2/3 reflects moderate within-group preference. |
| **SAR (crowded)** | Secondary attack rate in crowded households (20%&ndash;60%). Baseline is 40%, approximately twice the uncrowded rate of 20%. |
| **Crowding fold** | How much more likely the largest households are to be crowded vs. the smallest (1, 2, or 3). Baseline is 2. |

The between-household transmission rate &beta; is pre-calibrated for each combination of R&#8320;, SAR, crowding fold, and infectious period, so that the simulated final outbreak size matches the theoretical prediction for the specified R&#8320; in the absence of vaccination. The SAR for uncrowded households is fixed at 20%.

#### Vaccination

| Parameter | Description |
|-----------|-------------|
| **Vaccine efficacy** | Proportional reduction in per-exposure infection risk for vaccinated individuals (leaky vaccine). Baseline 60%, consistent with a good influenza season. |
| **Community vaccination coverage** | Fraction of the general population vaccinated prior to the outbreak. Baseline 50%, consistent with U.S. adult influenza vaccine uptake. |
| **Agricultural worker vaccination coverage** | Fraction of agricultural workers vaccinated. Baseline 40%, reflecting observed lower uptake in this population. |

Vaccination reduces the force of infection multiplicatively: the effective force of infection for subpopulation *x* is scaled by (1 &minus; efficacy &times; coverage&#8339;). Because &beta; is calibrated without vaccination, adding vaccination reduces the effective reproduction number below R&#8320;.

#### Comorbidity (Obesity)

| Parameter | Description |
|-----------|-------------|
| **Agricultural worker obesity prevalence** | Fraction of agricultural workers with obesity. Baseline 55%, higher than the general population (40%, fixed). |
| **Obesity OR for symptomatic disease** | Odds ratio for developing symptomatic illness given infection, comparing obese to non-obese individuals. Baseline 1.5. |

The derived symptomatic fraction p&#8347;,&#7424; is displayed in the sidebar and used throughout. It is lower than the obese-only probability because it is a weighted mixture across obese and non-obese workers. Setting OR = 1 returns p&#8347; = 0.500 for both populations.

### Application Tabs

**Epidemic Curves** &mdash; Displays symptomatic case prevalence (infected fraction &times; p&#8347;) over time for agricultural workers and the general population, along with the symptomatic case ratio (agricultural workers / general population). Results are shown for all six NAWS regions simultaneously.

**Summary Statistics** &mdash; Tabulates key outbreak metrics across all six regions: peak symptomatic case prevalence, time to peak, and cumulative symptomatic cases for both populations.

**Crop Impact** &mdash; Estimates agricultural production losses by overlaying epidemic dynamics with seasonal crop harvesting schedules for California crops. The tab displays three linked panels:

- *Production loss by peak day*: Percent production loss as a function of epidemic peak timing, showing how outbreak seasonality interacts with each crop's harvest calendar. A vertical line marks the selected peak day.
- *Work-limiting symptomatic cases*: Daily prevalence of work-limiting symptomatic illness among agricultural workers and the general community, aligned to the selected epidemic peak day. This uses a 3-day symptomatic window (see Methodology below) and is the direct driver of the crop loss shown above.
- *Adjusted harvest volume*: Actual vs. adjusted daily harvest volumes for selected commodities, accounting for workforce losses due to symptomatic illness.

**Methodology:**
1. Symptoms are assumed to begin 1 day after infection and last 3 days.
2. Work-limiting symptomatic prevalence at time *t* is computed as the sum of new daily infections from the preceding 3 days, multiplied by p&#8347;,&#7424;. This 3-day window represents the period during which workers are assumed to be absent; it is shorter than the full 5-day mean infectious period, so the symptomatic curves here will appear lower than those in the Epidemic Curves tab (which show the full infected compartment scaled by p&#8347;).
3. The epidemic is aligned so that peak symptomatic infections in the general community fall on the selected peak day.
4. Daily crop shipment volumes are multiplied by workforce availability (1 &minus; work-limiting symptomatic prevalence) to obtain adjusted production.
5. Percent loss is computed by comparing adjusted vs. unadjusted annual totals.

### Data Sources

- **General population**: American Community Survey (ACS) 2022 5-year estimates &mdash; household size distributions, crowding rates, agricultural employment, and population by county, aggregated to NAWS regions using population-weighted averages.
- **Agricultural workers**: National Agricultural Workers Survey (NAWS) 2018&ndash;2022 &mdash; household size distributions and crowding rates by region.
- **Crop movements**: USDA Agricultural Marketing Service (AMS) &mdash; weekly shipment data for fruit and vegetable commodities from California origins, averaged across 2018&ndash;2024 and interpolated to daily volumes.

### NAWS Regions

| Region | States |
|--------|--------|
| East (EA) | CT, DE, DC, ME, MD, MA, NH, NJ, NY, PA, RI, VT, VA, WV |
| Southeast (SE) | AL, FL, GA, KY, LA, MS, NC, SC, TN |
| Midwest (MW) | IL, IN, IA, KS, MI, MN, MO, NE, ND, OH, SD, WI |
| Southwest (SW) | AZ, NM, OK, TX |
| Northwest (NW) | CO, HI, ID, MT, NV, OR, UT, WA, WY |
| California (CA) | CA |
