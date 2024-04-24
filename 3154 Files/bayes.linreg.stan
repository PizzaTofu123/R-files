data { 
  int<lower=0> P;             // Number of predictors
  int<lower=0> N;             // Number of samples
  real y[N];                  // Targets
  matrix[N,P] x;              // Predictor matrix
} 

parameters {
  real beta0; 
  vector[P] beta;
  real<lower=0> sigma;
} 

model {
  // Data model -- linear regression
  for (i in 1:N)
  {
    y[i] ~ normal( beta0 + row(x,i)*beta, sigma );
  }

  // Prior distributions
  beta0 ~ cauchy(0,1);
  beta  ~ cauchy(0,sigma);
  sigma ~ cauchy(0,1);
}
