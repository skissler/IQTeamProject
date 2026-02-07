# ==============================================================================
# compute_contact_fractions.R - Contact Mixing Analysis by Epsilon
# ==============================================================================
# Computes the fraction of contacts that occur within-group vs. between-group
# for agricultural workers and community members at each epsilon value used
# in sensitivity analyses.
#
# The mixing matrix is:
#   M = | m_CC  m_CA |   where m_CC = (1-eps) + eps*w_C
#       | m_AC  m_AA |         m_CA = eps*w_A
#                              m_AC = eps*w_C
#                              m_AA = (1-eps) + eps*w_A
#
# Since rows sum to 1, the matrix elements directly give contact fractions:
#   - For community (C): fraction within-group = m_CC, between-group = m_CA
#   - For agricultural (A): fraction within-group = m_AA, between-group = m_AC
#
# Requires: source('code/setup.R') or source('code/import_acs.R') to load data
# ==============================================================================

# Load dependencies if not already loaded
if (!exists("paths")) {
  source('code/setup.R')
}
if (!exists("acs_data_regional")) {
  source('code/import_acs.R')
}

library(tidyverse)

# ==============================================================================
# Get regional population fractions
# ==============================================================================

# prop_ag_workers should be the same for all hhSize rows within a region
regional_prop_ag <- acs_data_regional %>%
  group_by(REGION6) %>%
  summarise(
    prop_ag = first(prop_ag_workers),
    population = first(population)
  ) %>%
  left_join(region_map, by = "REGION6")

cat("Regional proportion of agricultural workers:\n")
print(regional_prop_ag)

# ==============================================================================
# Epsilon values from sensitivity analysis
# ==============================================================================

eps_values <- c(0, 0.33, 0.5, 0.7)

# ==============================================================================
# Compute contact fractions for each region and epsilon
# ==============================================================================

contact_fractions <- expand_grid(
  regional_prop_ag,
  eps = eps_values
) %>%
  mutate(
    # Population weights
    w_A = prop_ag,
    w_C = 1 - prop_ag,

    # Mixing matrix elements (these ARE the contact fractions since rows sum to 1)
    # For community population
    C_within = (1 - eps) + eps * w_C,  # m_CC
    C_between = eps * w_A,              # m_CA

    # For agricultural worker population
    A_within = (1 - eps) + eps * w_A,   # m_AA
    A_between = eps * w_C               # m_AC
  ) %>%
  select(REGION6, REGION_NAME, REGION_ABBREV, prop_ag, eps,
         C_within, C_between, A_within, A_between)

# ==============================================================================
# Summary statistics
# ==============================================================================

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("Contact Fractions by Region and Epsilon\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")

# Print formatted table
contact_fractions %>%
  mutate(
    across(c(C_within, C_between, A_within, A_between), ~round(. * 100, 1))
  ) %>%
  rename(
    `C within (%)` = C_within,
    `C between (%)` = C_between,
    `A within (%)` = A_within,
    `A between (%)` = A_between
  ) %>%
  print(n = 50)

# ==============================================================================
# Summary across regions for each epsilon
# ==============================================================================

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("Summary: Agricultural Worker Contact Fractions by Epsilon\n")
cat("(Range across regions)\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")

summary_by_eps <- contact_fractions %>%
  group_by(eps) %>%
  summarise(
    A_within_min = min(A_within) * 100,
    A_within_max = max(A_within) * 100,
    A_between_min = min(A_between) * 100,
    A_between_max = max(A_between) * 100,
    .groups = "drop"
  ) %>%
  mutate(
    A_within_range = sprintf("%.1f%% - %.1f%%", A_within_min, A_within_max),
    A_between_range = sprintf("%.1f%% - %.1f%%", A_between_min, A_between_max)
  )

print(summary_by_eps)

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("Summary: Community Contact Fractions by Epsilon\n")
cat("(Range across regions)\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")

summary_C_by_eps <- contact_fractions %>%
  group_by(eps) %>%
  summarise(
    C_within_min = min(C_within) * 100,
    C_within_max = max(C_within) * 100,
    C_between_min = min(C_between) * 100,
    C_between_max = max(C_between) * 100,
    .groups = "drop"
  ) %>%
  mutate(
    C_within_range = sprintf("%.1f%% - %.1f%%", C_within_min, C_within_max),
    C_between_range = sprintf("%.1f%% - %.1f%%", C_between_min, C_between_max)
  )

print(summary_C_by_eps)

# ==============================================================================
# Key finding for writeup
# ==============================================================================

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("KEY FINDING FOR WRITEUP\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")

baseline_eps <- 0.33
baseline_summary <- contact_fractions %>%
  filter(eps == baseline_eps)

cat(sprintf("At baseline epsilon = %.2f:\n", baseline_eps))
cat(sprintf("  - Agricultural workers: %.1f%% - %.1f%% of contacts within-group\n",
            min(baseline_summary$A_within) * 100,
            max(baseline_summary$A_within) * 100))
cat(sprintf("  - Agricultural workers: %.1f%% - %.1f%% of contacts between-group\n",
            min(baseline_summary$A_between) * 100,
            max(baseline_summary$A_between) * 100))
cat(sprintf("  - Community: %.1f%% - %.1f%% of contacts within-group\n",
            min(baseline_summary$C_within) * 100,
            max(baseline_summary$C_within) * 100))
cat(sprintf("  - Community: %.1f%% - %.1f%% of contacts between-group\n",
            min(baseline_summary$C_between) * 100,
            max(baseline_summary$C_between) * 100))

# ==============================================================================
# Save results
# ==============================================================================

write_csv(contact_fractions, "output/contact_fractions.csv")
cat("\nSaved: output/contact_fractions.csv\n")

write_csv(summary_by_eps, "output/contact_fractions_summary.csv")
cat("Saved: output/contact_fractions_summary.csv\n")
