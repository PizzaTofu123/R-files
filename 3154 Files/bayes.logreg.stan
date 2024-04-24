data { 
  int<lower=0> P;             // Number of predictors
  int<lower=0> N;             // Number of samples
  int<lower=0,upper=1> y[N];  // Targets
  matrix[N,P] x;              // Predictor matrix
} 

parameters {
  real beta0;
  vector[P] beta;
} 

model {
  // Data model -- logistic regression
  real logOdds;
  for (i in 1:N)
  {
    // Compute the log-odds for observation 'i'
    logOdds = beta0;
    for (j in 1:P)
    {
      logOdds = logOdds + beta[j]*x[i,j];
    }
    // We could also replace the above line by: 
    //  logOdds = beta0 + row(x,i)*beta;
    // which uses matrix algebra and avoids as much looping

    y[i] ~ bernoulli( 1/(1+exp(-logOdds)) );
  }

  // Prior distributions
  beta0 ~ cauchy(0,1);
  beta ~ cauchy(0,1);
  // The above is the same as:
  // for (j in 1:P) { beta[j] ~ cauchy(0,1); }
  // because we are assigning the same prior to each of the P beta's
  
}
