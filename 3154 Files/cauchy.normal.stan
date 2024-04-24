data {
  int<lower=1> N;
  vector[N] y;
  real m;
  real<lower=0> s;
}

parameters {
  real mu;
  real<lower=0> sigma;
}

model {
  mu ~ cauchy(m, s);
  sigma ~ cauchy(0, 1);

  y ~ normal(mu, sigma);
}
