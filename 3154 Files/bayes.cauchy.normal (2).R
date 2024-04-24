# Code for Bayesian analysis of normal distribution using the hierarchy:
#
#    y_j | mu, sigma ~ N(mu, sigma),   j = 1,...,n
#     mu | m, s      ~ C(m, s)
#  sigma             ~ C+(m,s)
#
# FIT3154 Studio 6. (c) Daniel F. Schmidt 2018

bayes.cauchy.normal <- function(y, m, s, n.samples = 1e4)
{
  # Initialise parameters/latent variables
  nu = 1
  w2 = 1
  sigma = sd(y)
  n = length(y)
  
  # Return values
  rv = list()
  rv$mu.samples = matrix(0,1,n.samples)
  rv$sigma.samples = matrix(0,1,n.samples)
  
  # Sample ...
  for (i in 1:n.samples)
  {
    # Sample mu | w2, y
    rv.n.n = bayes.normal.normal(y, sigma, m, s*sqrt(w2))
    mu = rnorm(1, mean = rv.n.n$post.mean, sd = rv.n.n$post.sd)
    
    # Sample w2 | mu, s, m
    scale = ((mu-m)^2/s^2 + 1)/2
    w2 = scale / rgamma(1, 1, 1)
    
    # Sample sigma | nu, mu, y
    shape = (n+1)/2
    scale = 1/nu + sum((mu-y)^2)/2
    sigma = sqrt(scale / rgamma(1,shape,1))
    
    # Sample nu | sigma
    scale = 1 + 1/sigma^2
    nu = scale / rgamma(1,1,1)
    
    # Store samples
    rv$mu.samples[i] = mu
    rv$sigma.samples[i] = sigma
  }
  
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