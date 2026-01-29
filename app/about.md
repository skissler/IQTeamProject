## Household-Structured Epidemic Model

This interactive application explores a household-structured SIR (Susceptible-Infectious-Recovered) model comparing disease dynamics between **agricultural workers** and the **general population**.

### Why Agricultural Workers?

Agricultural workers face elevated disease transmission risk due to:

- **Higher household crowding rates**: More persons per room increases within-household transmission
- **Larger household sizes**: More opportunities for secondary infections
- **Occupational factors**: Close working conditions, shared transportation

### Model Structure

The model implements the **House & Keeling (2008)** framework:

1. **Household-level tracking**: Instead of tracking individuals, we track the distribution of households by composition (# susceptible, # infected, # recovered)

2. **Two transmission routes**:
   - *Within-household*: Infected members transmit to susceptible household members at rate τ
   - *Between-household*: Community transmission at rate β proportional to overall prevalence

3. **Crowding effect**: Crowded households (>1 person/room) have elevated within-household transmission (τ + τ_boost)

4. **Two populations with mixing**: Agricultural workers (A) and general community (C) interact according to the assortativity parameter ε

### Parameters

| Parameter | Description |
|-----------|-------------|
| **R₀** | Basic reproduction number - average secondary infections from one case |
| **ε (epsilon)** | Mixing parameter: 0 = groups don't mix, 1 = proportional mixing |
| **SAR (uncrowded)** | Secondary attack rate in uncrowded households |
| **SAR (crowded)** | Secondary attack rate in crowded households |
| **Crowding fold** | How much more likely large households are to be crowded |

### Key Findings

Under typical parameters:
- Agricultural workers experience **higher peak prevalence**
- Agricultural workers have **higher cumulative attack rates**
- The differential increases with higher R₀ values
- Reducing crowding could substantially reduce disparities

### References

- House, T. & Keeling, M.J. (2008). Deterministic epidemic models with explicit household structure. *Mathematical Biosciences*, 213(1), 29-39.

- Madewell, Z.J. et al. (2020). Household Transmission of SARS-CoV-2: A Systematic Review and Meta-analysis. *JAMA Network Open*, 3(12), e2031756.

### Data Sources

- **General population**: American Community Survey (ACS) 2022 5-year estimates
- **Agricultural workers**: National Agricultural Workers Survey (NAWS) 2018-2022
