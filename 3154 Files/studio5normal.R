# Code for analysing normal data (frequentist & Bayesian approaches)
# Studio 5, FIT3154

# Conventional frequentist analysis of mean
analyse.normal <- function(y, sigma)
{
  rv = list()
  
  # Compute mean and standard error
  n = length(y)
  rv$muhat = mean(y)
  rv$se = sigma/sqrt(n)
  
  # Compute the 95% CI
  rv$CI = rv$muhat + c(-1.96, 1.96)*rv$se
  
  return(rv)
}

# Q3.5
# Bayesian analysis of mean with normal prior
bayes.normal.normal <- function(y, sigma, m, s)
{
  rv = list()
  
  n = length(y)
  
  # Compute posterior distribution
  rv$post.sd = sqrt(1/(n/sigma^2 + 1/s^2))
  rv$post.mean = (n*s^2)/(n*s^2 + sigma^2)*(mean(y) - m) + m
  
  # Compute 95% credible intervals (posterior is normal so we can just use -/+ 1.96 rule in THIS case)
  rv$CI = rv$post.mean + c(-1.96, 1.96)*rv$post.sd
  # in general we would use quantile function if it exists, i.e.
  # rv$CI = qnorm(p=(0.025,0.975), post$mean, post$sd)
  
  return(rv)
}