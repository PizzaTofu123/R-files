data { 
  int<lower=0> N; 
  int<lower=0> n[N];
  int<lower=0> y[N];
} 

parameters {
  real beta0;
  vector[N] beta;
  real<lower=0> lambda;
} 

transformed parameters {
  real theta0;
  vector[N] theta;
  
  theta0 = 1/(1+exp(-beta0));
  for (i in 1:N)
  {
    theta[i] = 1/(1+exp(-beta0-beta[i]));
  }  
}

model {
  for (i in 1:N)
  {
    y[i] ~ binomial( n[i], theta[i] );
  }
  

  beta0 ~ cauchy(0,1);
  beta ~ normal(0,lambda); // Normal in Stan expects (mu, sigma) not (mu, sigma^2)
  lambda ~ cauchy(0,1);
}