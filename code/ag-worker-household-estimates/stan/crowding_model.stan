data {
  int<lower=1> N;
  int<lower=1> R;
  int<lower=1,upper=R> region[N];
  vector[N] gamma;
  vector[R] mu_hat;
  vector<lower=0>[R] se_hat;
}
parameters {
  vector[R] mu;
  real<lower=0> sigma;
  vector[N] theta;
}
model {
  mu ~ normal(mu_hat, se_hat);
  theta ~ normal(mu[region] .* gamma, sigma);
}
generated quantities {
  vector[N] theta_pred;
  theta_pred = theta;
}
