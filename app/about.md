<style>
table { border-collapse: collapse; width: 100%; }
th, td { padding: 8px 16px; text-align: left; }
</style>

## About This Tool

This interactive application accompanies the manuscript "Modeling the impact of respiratory disease outbreaks on the United States agricultural workforce" by Bardsley, de Pablo, Keppler Canada, Ormaza Zulueta, Mehrabi, and Kissler. It simulates respiratory disease outbreaks using a household-structured susceptible-infectious-recovered (SIR) model, comparing disease dynamics between **agricultural workers** and the **general population** across six U.S. regions defined by the National Agricultural Workers Survey (NAWS).

### Motivation

Respiratory disease outbreaks can cause societal and economic disruptions that cascade beyond their direct public health burden. Agricultural workers face elevated transmission risk due to higher rates of household crowding and larger household sizes, which amplify within-household transmission of respiratory pathogens. During the COVID-19 pandemic, household crowding was identified as a primary risk factor for SARS-CoV-2 infection among farmworkers. This tool allows users to explore how differences in household structure between agricultural workers and the general population translate into differential disease burden and downstream impacts on crop production.

### Model Structure

The model implements a deterministic household-structured SIR framework based on House & Keeling (2008):

1. **Household-level tracking**: The model tracks the distribution of households by composition (number susceptible, infected, and recovered), explicitly accounting for transmission within and between households of various sizes.

2. **Two transmission routes**:
   - *Within-household*: Infected members transmit to susceptible household members at rate &tau;, derived from the household secondary attack rate (SAR).
   - *Between-household*: Community-level transmission at rate &beta;, calibrated to achieve a target basic reproduction number (R&#8320;).

3. **Crowding effect**: Crowded households (>1 person per room) experience elevated within-household transmission. The crowding fold parameter controls how much more likely large households are to be crowded relative to small households.

4. **Two populations with assortative mixing**: Agricultural workers (A) and the general community (C) mix according to an assortativity parameter &eta;, where &eta; = 1 implies fully assortative mixing (groups interact only within themselves) and &eta; = 0 implies proportional mixing.

### Parameters

| Parameter | Description |
|-----------|-------------|
| **R&#8320;** | Basic reproduction number (1.2, 1.5, 2.0, or 3.0). Baseline R&#8320; = 1.5 reflects a moderate pandemic influenza scenario. |
| **Infectious period (1/&gamma;)** | Duration of infectiousness in days (3, 5, or 10). Baseline is 5 days, consistent with influenza. |
| **Assortativity (&eta;)** | Degree of preferential within-group mixing (0 to 1). Baseline &eta; = 2/3 reflects moderate within-group preference. |
| **SAR (crowded)** | Secondary attack rate in crowded households (20%&ndash;60%). Baseline is 40%, approximately twice the uncrowded rate of 20%. |
| **Crowding fold** | How much more likely the largest households are to be crowded vs. the smallest (1, 2, or 3). Baseline is 2. |

The between-household transmission rate &beta; is pre-calibrated for each combination of R&#8320;, SAR, crowding fold, and infectious period (180 combinations total), so that the simulated final outbreak size matches the theoretical prediction for the specified R&#8320;. The SAR for uncrowded households is fixed at 20%.

### Application Tabs

**Epidemic Curves** &mdash; Displays infection prevalence and incidence over time for agricultural workers and the general population, along with the prevalence ratio (agricultural workers / general population). Select a NAWS region to view regional dynamics driven by that region's household size distributions and crowding rates.

**Summary Statistics** &mdash; Tabulates key outbreak metrics across all six regions: peak prevalence, time to peak, and final attack rate for both populations.

**Crop Impact** &mdash; Estimates agricultural production losses by overlaying epidemic dynamics with seasonal crop harvesting schedules for California crops. The tab displays three linked panels:

- *Symptomatic infections*: Daily symptomatic disease prevalence among agricultural workers and the general community, aligned to the selected epidemic peak day.
- *Adjusted harvest volume*: Actual vs. adjusted daily harvest volumes for selected commodities, accounting for workforce losses due to symptomatic illness.
- *Production loss by peak day*: Percent production loss as a function of epidemic peak timing, showing how outbreak seasonality interacts with each crop's harvest calendar.

**Methodology:**
1. Symptoms are assumed to begin 1 day after infection and last 3 days.
2. Daily workforce availability is calculated as: 1 &minus; (symptomatic proportion among agricultural workers &times; proportion symptomatic, p&#8347;).
3. The epidemic is aligned so that peak symptomatic infections in the general community fall on the selected peak day.
4. Daily crop shipment volumes are multiplied by workforce availability to obtain adjusted production.
5. Percent loss is computed by comparing adjusted vs. unadjusted annual totals.

Because production loss scales linearly with the proportion symptomatic (p&#8347;), results for any symptomatic fraction can be obtained by rescaling. **79 commodities** are available from the bundled USDA AMS shipment data.

### Data Sources

- **General population**: American Community Survey (ACS) 2022 5-year estimates &mdash; household size distributions, crowding rates, agricultural employment, and population by county, aggregated to NAWS regions using population-weighted averages.
- **Agricultural workers**: National Agricultural Workers Survey (NAWS) 2018&ndash;2022 &mdash; household size distributions and crowding rates by region.
- **Crop movements**: USDA Agricultural Marketing Service (AMS) &mdash; weekly shipment data for 9 fruit and vegetable commodities from California origins, averaged across 2018&ndash;2024 and interpolated to daily volumes.

### NAWS Regions

| Region | States |
|--------|--------|
| East (EA) | CT, DE, DC, ME, MD, MA, NH, NJ, NY, PA, RI, VT, VA, WV |
| Southeast (SE) | AL, FL, GA, KY, LA, MS, NC, SC, TN |
| Midwest (MW) | IL, IN, IA, KS, MI, MN, MO, NE, ND, OH, SD, WI |
| Southwest (SW) | AZ, NM, OK, TX |
| Northwest (NW) | CO, HI, ID, MT, NV, OR, UT, WA, WY |
| California (CA) | CA |
