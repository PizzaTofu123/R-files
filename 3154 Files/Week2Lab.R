n = 10
s = seq(0,10)
alpha = 0
theta.hat = (s + alpha)/(n + 2*alpha)
theta.hat
bias = alpha*(2*theta.hat-1)/(2*alpha+n)
var  = n*(1-theta.hat)*theta.hat/(2*alpha+n)^2
risk = bias^2 + var
plot(theta.hat,risk,type="l")

n = 10
s = seq(0,10)
alpha = 0.5
theta.hat = (s + alpha)/(n + 2*alpha)
theta.hat

# Binomial risk function for smoothed estimator of proportion
# FIT3154 Studio 2
bin.risk <- function(alpha, n)
{
  # Make a grid of true population proportion (theta) values
  theta = seq(0,1,length.out=1e3)
  
  # Evaluate the bias and variance at each true population parameter
  bias = alpha*(2*theta-1)/(2*alpha+n)
  var  = n*(1-theta)*theta/(2*alpha+n)^2
  
  # Return the theta values and corresponding risks
  rv = list()
  rv$theta = theta
  rv$risk = bias^2 + var
  return(rv)
}

# theta at 0.5 is highest risk because the observation is 
# going to be extremely random pr 50-50
rv=bin.risk(alpha=0, n=10)
plot(rv$theta,rv$risk,type="l")

rv=bin.risk(alpha=1/2, n=10)
lines(rv$theta,rv$risk,col="red")

n = 10
s = seq(0,10)
alpha = sqrt(n)/2
theta.hat = (s + alpha)/(n + 2*alpha)
theta.hat

rv=bin.risk(alpha, n)
lines(rv$theta, rv$risk, col = "green")

rv=bin.risk(alpha=0, n=100)
plot(rv$theta,rv$risk,type="l")
rv=bin.risk(alpha=1/2, n=100)
lines(rv$theta,rv$risk,col="red")
rv=bin.risk(alpha=sqrt(n)/2, n=100)
lines(rv$theta,rv$risk,col="blue")

#2.3

sigma2 = 1
sigma2.hat = seq(0.1, 10, length.out=1e3)
kl = (1/2)*log(sigma2.hat/sigma2) + sigma2/2/sigma2.hat - 1/2
plot(sigma2.hat, kl, type="l")

sigma2 = 2
sigma2.hat = seq(0.1, 10, length.out=1e3)
kl = (1/2)*log(sigma2.hat/sigma2) + sigma2/2/sigma2.hat - 1/2
lines(sigma2.hat, kl, type="l", col = "red")
# both have divergence of 0 when correctly estimating and 
# both prefer overestimation

#2.4

# Functions to compute the expected KL loss (KL risk) for the normal distribution using
# estimators
#
#  mu.hat    = 1/n sum y_i
#  sigma2.hat = 1/(n-k) sum (y_i - mu.hat)^2
#
# for the mean and variance.
#
# FIT3154 Studio 2, Copyright (c) Daniel F Schmidt 2019

# Approximate risk using simulation
normal.kl.risk <- function(mu, sigma2, n, k = 0, m = 1e5)
{
  # To store the KL divergences for each iteration of the simulation
  kl = matrix(0, m, 1)
  
  # Do 'm' iterations
  for (i in 1:m)
  {
    # Generate a sample from our population, i.e., y1,...,yn ~ N(mu, sigma2)
    y = rnorm(n, mean = mu, sd = sqrt(sigma2))
    
    # Estimate mu and sigma2 from the sample
    mu.hat = mean(y)
    sigma2.hat = sum((y-mu.hat)^2) / (n-k)
    
    # Compute KL divergence for these estimates
    kl[i] = (1/2)*log(sigma2.hat/sigma2) + sigma2/2/sigma2.hat + (mu-mu.hat)^2/2/sigma2.hat - 1/2
  }
  
  # Average the KL divergences for the 'm' simulations to estimate the risk (average/expected loss)
  return(mean(kl))
}

# Exact formula for the risk
normal.kl.risk.exact <- function(n, k=0)
{
  return( (1/2)*(digamma((n-1)/2) + log(2/(n-k))) + (n+1)*(n-k)/2/(n-3)/n - 1/2 )
}

normal.kl.risk(mu=0,sigma2=1,n=10,k=0,m=1e6)
normal.kl.risk(mu=0,sigma2=1,n=10,k=1,m=1e6)
normal.kl.risk(mu=0,sigma2=1,n=10,k=2,m=1e6)
normal.kl.risk(mu=0,sigma2=1,n=10,k=3,m=1e6)
normal.kl.risk(mu=0,sigma2=1,n=10,k=4,m=1e6)
normal.kl.risk(mu=0,sigma2=1,n=10,k=5,m=1e6)


#2.7
normal.kl.risk.exact(n=10,k=0)
normal.kl.risk.exact(n=10,k=1)

normal.kl.risk.exact(n=100,k=0)
normal.kl.risk.exact(n=100,k=1)
#the difference gets smaller, the effect of k gets smaller



