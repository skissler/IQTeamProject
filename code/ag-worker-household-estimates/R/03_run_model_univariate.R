library(rstan)
options(mc.cores = parallel::detectCores())

stan_data <- readRDS("data/processed/stan_joint.rds")
crowding_data <- list(
  N = stan_data$N,
  R = stan_data$R,
  region = stan_data$region,
  gamma = stan_data$gamma[, 1],
  mu_hat = stan_data$mu_hat[, 1],
  se_hat = stan_data$se_hat[, 1]
)

fit_uni <- stan(
  file = "stan/crowding_model.stan",
  data = crowding_data,
  iter = 2000, chains = 4, seed = 2025
)

saveRDS(fit_uni, file = "results/fit_univariate.rds")