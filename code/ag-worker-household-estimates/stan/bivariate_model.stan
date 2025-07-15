data {
  int<lower=1> N;
  int<lower=1> R;
  int<lower=1,upper=R> region[N];
  matrix[N, 2] gamma;
  matrix[R, 2] mu_hat;
  matrix[R, 2] se_hat;
}
parameters {
  matrix[R, 2] mu;
  vector<lower=0>[2] sigma;
  real<lower=-1, upper=1> rho;
  matrix[N, 2] theta;
}
transformed parameters {
  cov_matrix[2] Sigma;
  Sigma[1,1] = square(sigma[1]);
  Sigma[2,2] = square(sigma[2]);
  Sigma[1,2] = rho * sigma[1] * sigma[2];
  Sigma[2,1] = Sigma[1,2];
}
model {
  for (r in 1:R)
    for (d in 1:2)
      mu[r, d] ~ normal(mu_hat[r, d], se_hat[r, d]);
  for (n in 1:N)
    theta[n] ~ multi_normal(mu[region[n]] .* gamma[n], Sigma);
}
generated quantities {
  matrix[N, 2] theta_pred;
  theta_pred = theta;
}
