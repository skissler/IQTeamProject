## Household-Structured Epidemic Model

This interactive application explores a household-structured SIR (Susceptible-Infectious-Recovered) model comparing disease dynamics between **agricultural workers** and the **general population** across six NAWS regions.

### Why Agricultural Workers?

Agricultural workers face elevated disease transmission risk due to:

- **Higher household crowding rates**: More persons per room increases within-household transmission
- **Larger household sizes**: More opportunities for secondary infections
- **Occupational factors**: Close working conditions, shared transportation

### Model Structure

The model implements the **House & Keeling (2008)** framework:

1. **Household-level tracking**: Instead of tracking individuals, we track the distribution of households by composition (# susceptible, # infected, # recovered)

2. **Two transmission routes**:
   - *Within-household*: Infected members transmit to susceptible household members at rate tau
   - *Between-household*: Community transmission at rate beta proportional to overall prevalence

3. **Crowding effect**: Crowded households (>1 person/room) have elevated within-household transmission (tau + tau_boost)

4. **Two populations with mixing**: Agricultural workers (A) and general community (C) interact according to the assortativity parameter eta

### Parameters

| Parameter | Description |
|-----------|-------------|
| **Region** | NAWS region (determines household distributions and population sizes) |
| **R0** | Basic reproduction number — average secondary infections from one case |
| **eta (eta)** | Assortativity: 1 = groups don't mix (assortative), 0 = proportional mixing |
| **SAR (uncrowded)** | Secondary attack rate in uncrowded households |
| **SAR (crowded)** | Secondary attack rate in crowded households |
| **Crowding fold** | How much more likely large households are to be crowded |

### Color Convention

- **Blue** (#377EB8): Agricultural workers (A)
- **Red** (#E41A1C): General population (C)

This matches the color scheme used in the manuscript figures.

### Crop Impact Assessment

The **Crop Impact** tab estimates agricultural production losses by overlaying epidemic dynamics with seasonal crop harvesting schedules for California crops (lettuce, strawberries, oranges).

**Methodology:**
1. Compute daily symptomatic infections (symptoms last days 1-3 after onset)
2. Calculate workforce availability: 1 - (symptomatic proportion x p_symp)
3. Align the epidemic peak to a target calendar day
4. Multiply daily crop movements by workforce availability
5. Sum adjusted vs. unadjusted production to get percent loss

Because production loss scales linearly with p_symp, you can explore different assumptions about the fraction of infections that cause symptoms severe enough to prevent work.

**CSV Upload:** You can upload additional USDA AMS movements CSV files to add crops to the analysis. Files must contain columns: `origin`, `begin_date`, `commodity`, `1_lb_units`. California origins are filtered automatically.

### Data Sources

- **General population**: American Community Survey (ACS) 2022 5-year estimates — household size distributions, crowding rates, agricultural employment, population by county, aggregated to NAWS regions
- **Agricultural workers**: National Agricultural Workers Survey (NAWS) 2018-2022 — household size distributions and crowding rates by region
- **Crop movements**: USDA Agricultural Marketing Service (AMS) — weekly shipment data for lettuce, strawberries, and oranges from California origins

### NAWS Regions

| Region | States |
|--------|--------|
| East (EA) | CT, DE, DC, ME, MD, MA, NH, NJ, NY, PA, RI, VT, VA, WV |
| Southeast (SE) | AL, FL, GA, KY, LA, MS, NC, SC, TN |
| Midwest (MW) | IL, IN, IA, KS, MI, MN, MO, NE, ND, OH, SD, WI |
| Southwest (SW) | AZ, NM, OK, TX |
| Northwest (NW) | CO, HI, ID, MT, NV, OR, UT, WA, WY |
| California (CA) | CA |

### References

- House, T. & Keeling, M.J. (2008). Deterministic epidemic models with explicit household structure. *Mathematical Biosciences*, 213(1), 29-39.

- Madewell, Z.J. et al. (2020). Household Transmission of SARS-CoV-2: A Systematic Review and Meta-analysis. *JAMA Network Open*, 3(12), e2031756.
