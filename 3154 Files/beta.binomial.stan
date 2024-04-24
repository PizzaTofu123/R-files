data { 
  int<lower=0> N; 
  int<lower=0,upper=1> y[N];
  real<lower=0> a;
  real<lower=0> b;
} 

parameters {
  real<lower=0,upper=1> theta;
} 

model {
  theta ~ beta(a,b);
  y ~ bernoulli(theta);
  // Could also do:
  // for (j in 1:N)
  // {
  //    y[j] ~ bernoulli(theta);
  // }
}
