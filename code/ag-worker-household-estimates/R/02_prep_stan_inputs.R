library(dplyr)
final_data <- readRDS("data/processed/final_data.rds")

naws_joint <- tibble::tribble(
  ~region, ~mu_crowd, ~se_crowd, ~mu_kids, ~se_kids,
  "Northeast", 0.20, 0.03, 0.50, 0.05,
  "Southeast", 0.28, 0.04, 0.55, 0.05,
  "Midwest",   0.22, 0.03, 0.52, 0.05,
  "Southwest", 0.30, 0.04, 0.56, 0.04,
  "West",      0.35, 0.03, 0.60, 0.04
)

joint_data <- final_data %>%
  filter(!is.na(region)) %>%
  left_join(naws_joint, by = "region") %>%
  mutate(region_index = as.integer(factor(region)))

nat_crowd <- weighted.mean(joint_data$prop_crowded, joint_data$prop_nonag)
nat_kids  <- weighted.mean(joint_data$prop_with_kids, joint_data$prop_nonag)

joint_data <- joint_data %>%
  mutate(gamma1 = prop_crowded / nat_crowd, gamma2 = prop_with_kids / nat_kids)

stan_joint <- list(
  N = nrow(joint_data),
  R = length(unique(joint_data$region_index)),
  region = joint_data$region_index,
  gamma = cbind(joint_data$gamma1, joint_data$gamma2),
  mu_hat = cbind(
    tapply(joint_data$mu_crowd, joint_data$region_index, unique),
    tapply(joint_data$mu_kids, joint_data$region_index, unique)
  ),
  se_hat = cbind(
    tapply(joint_data$se_crowd, joint_data$region_index, unique),
    tapply(joint_data$se_kids, joint_data$region_index, unique)
  )
)

saveRDS(stan_joint, file = "data/processed/stan_joint.rds")
saveRDS(joint_data, file = "data/processed/joint_data.rds")