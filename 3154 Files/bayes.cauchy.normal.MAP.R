# Posterior mode (MAP) estimator for the normal distribution:
# y_i   ~ N(mu, sigma^2)
# mu    ~ C(m, s)
# sigma ~ C+(0,1)
#
bayes.cauchy.normal.MAP <- function(y, m, s)
{
  n = length(y)
  Y = sum(y)
  
  # Initial guess
  mu    = mean(y)
  sigma = sd(y)
  
  # CWD loop
  while (1)
  {
    # Save current estimates
    mu.old = mu
    sigma.old = sigma
    
    ## Update the estimate for 'mu' given current value of 'sigma'
    # Solve polynomial
    c0 = s^2*Y + m^2*Y + 2*m*sigma^2
    c1 = -2*m*Y - 2*sigma^2 - n*s^2 - m^2*n
    c2 = Y + 2*m*n
    c3 = -n
    r = polyroot(c(c0,c1,c2,c3))    
    
    # Pick root with smallest score
    # Remove complex roots
    r = Re(r[abs(Im(r)) < 1e-5]) 
    mu = r[which.min(normal.bayes.lp(r,Y,n,m,s))]
    
    ## Update the estimate for 'sigma' given current estimate for 'mu'
    S = sum((y - mu)^2)
    r = Re(polyroot(c(-S,n-S,n+2)))
    r = r[r>0] # Select the positive root
    sigma = sqrt(r)
    
    # Are we done?
    if ( ((sigma - sigma.old)^2 + (mu-mu.old)^2) < 1e-4)
    {
      break
    }
  }
  
  # Store return values
  rv = list()
  rv$mu.hat = mu
  rv$sigma.hat = sigma
  
  return(rv)
}

# Negative log-posterior (for mu)
normal.bayes.lp = function(mu, Y, n, m, s) 
{ 
  1/2/sigma2*(-2*Y*mu + n*mu^2) + log(1+(m-mu)^2/s^2)
}

