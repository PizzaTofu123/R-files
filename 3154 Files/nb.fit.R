################################################################################
# Tools for negative binomial distributions for FIT3154
# (c) Daniel F. Schmidt 2023
################################################################################

# Negative binomial neg-log-likelihood
nb.neg.ll <- function(y, mu, r)
{
  n = length(y)
  S = sum(y)

  # Gradients
  g.mu = (y+r)/(r+mu) - y/mu
  g.r = digamma(r) - digamma(y+r) - log(r) - 1 + log(r+mu) + (y+r)/(r+mu)
  G = matrix(0,n,2)
  G[,1] = g.mu
  G[,2] = g.r
  
  list(L=n*lgamma(r) - sum(lgamma(y+r)) + sum(lgamma(y+1)) - n*r*log(r) - S*log(mu) + (S+n*r)*log(r+mu),
       g=c(sum(g.mu),sum(g.r)), G=G)
}


# Fit an NB using ML
nb.ml <- function(y)
{
  n = length(y)
  temp.l<-function(log.r,y) { nb.neg.ll(y,mean(y),exp(log.r))$L }
  r = exp(optimize(temp.l, c(-10,10), y=y)$minimum)
  
  return(list(mu = mean(y), r = r))
}

# Draw posterior samples for an NB model using simple MH sampler
nb.sample <- function(y,a,b,n.samples=1e4)
{
  # Initialise
  burn.in = 1e3
  k       = 1
  iter    = 1
  thin    = 1
  
  mu.sample = matrix(0,n.samples,1)
  r.sample  = matrix(0,n.samples,1)
  
  # Initial values
  rv.ml = nb.ml(y)
  log.mu = log(rv.ml$mu)
  log.r = log(rv.ml$r)
  
  rv = nb.neg.ll(y,rv.ml$mu,rv.ml$r)
  rv$G[,1] = rv$G[,1]*exp(log.mu)
  rv$G[,2] = rv$G[,2]*exp(log.r)
  H = t(rv$G) %*% rv$G
  L.c = rv.ml$L

  xp = c(log.mu, log.r)
  
  # Sample using MH
  while (k <= n.samples)
  {
    # Proposal
    x = simple.rmvn(c(log.mu,log.r), solve(H)*2.5)
    
    # MH test?
    rv.prop = nb.neg.ll(y,exp(x[1]),exp(x[2]))

    L.num = rv.prop$L + nb.neg.log.prior(x[1],x[2],a,1/b)
    L.den = L.c + nb.neg.log.prior(xp[1],xp[2],a,1/b)
    
    A = min(c(exp(-(L.num - L.den)),1))
    if (runif(1) < A)
    {    
      log.mu = x[1]
      log.r = x[2]
      xp = x
      L.c = rv.prop$L
    }
    
    # Store?
    if (iter > burn.in & iter %% thin == 0)
    {
      mu.sample[k] = exp(log.mu)
      r.sample[k] = exp(log.r)
      k = k+1
    }
    iter = iter+1
  } 
  
  # Done
  return(list(mu.sample=mu.sample,r.sample=r.sample))
}

nb.neg.log.prior <- function(log.mu, log.r, a, b)
{
  #nb.log.bp(log.mu,a,b) + nb.log.bp(log.r,1/2,1/2)
  nb.neg.log.gamma(log.mu,a,b) + nb.neg.log.bp(log.r,1/2,1/2)
}

nb.neg.log.bp <- function(x, a, b)
{
  -a*x + (a+b)*log(exp(x)+1)
}

nb.neg.log.gamma <- function(x, a, b)
{
  exp(x)/b - (a-1)*x + x + lgamma(a) + a*log(b)
}

simple.rmvn <- function(mu, S)
{
  p = length(mu)
  L = chol(S)
  t(L)%*%as.vector(rnorm(p)) + mu
}


