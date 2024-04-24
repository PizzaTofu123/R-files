data { 
  int<lower=0> N; 
  int<lower=0,upper=1> y[N];
  vector[N] x;
} 

parameters {
  real beta0;
  real beta1;
} 

model {
  for (i in 1:N)
  {
    y[i] ~ bernoulli( 1/(1+exp(-beta0-beta1*x[i])) );
  }

  beta0 ~ cauchy(0,1);
  beta1 ~ cauchy(0,1);
}
