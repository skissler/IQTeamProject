library(rstan)
options(mc.cores = parallel::detectCores())

stan_joint <- readRDS("data/processed/stan_joint.rds")

fit_biv <- stan(
  file = "stan/bivariate_model.stan",
  data = stan_joint,
  iter = 2000, chains = 4, seed = 2025
)

saveRDS(fit_biv, file = "results/fit_bivariate.rds")