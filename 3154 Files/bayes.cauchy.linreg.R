# Code for Bayesian analysis of normal linear regression using the hierarchy:
#
#    y_i | x_i, beta, beta0 ~ N(x_i*beta + beta0, sigma),   i = 1,...,n
#                     beta0 ~ C(m, 1)
#                    beta_j ~ C(0, 1),                      j = 1,...,p
#                     sigma ~ C+(0, 1)
#
# FIT3154 Studio 4. (c) Daniel F. Schmidt 2018
#
bayes.cauchy.linreg <- function(formula, data, m = 0, n.samples = 1e4)
{
  # Figure out the target variable
  mf = model.frame(formula,data=data)
  target.var = names(mf)[[1]]

  p = ncol(mf)-1
  n = nrow(mf)
  
  # Initialise parameters/latent variables
  nu = 1
  w2 = matrix(1,p+1,1)
  sigma = 1
  beta0 = 0
  beta = matrix(0,p,1)

  # Extract data
  y = as.matrix(mf[,1])
  X = as.matrix(mf[,-1])
  
  # Standardise data
  stdX = bayesreg.standardise(X)
  X    = stdX$X
  XtX  = t(X) %*% X
  
  # WAIC
  waicProb   = matrix(0,n,1)  
  waicLProb  = matrix(0,n,1)
  waicLProb2 = matrix(0,n,1)

  # Return values
  rv = list()
  class(rv) <- "bayes.cauchy.linreg"
  rv$terms <- stats::terms(x = formula, data = data)
  
  rv$beta.samples  = matrix(0,p+1,n.samples)
  rv$sigma.samples = matrix(0,1,n.samples)
  rownames(rv$beta.samples)[2:(p+1)] = names(mf)[2:(p+1)]
  rownames(rv$beta.samples)[1] = "(Intercept)"
  
  # Sample ...
  for (i in 1:n.samples)
  {
    ## Sample beta0 | beta, sigma, w2[1]
    rv.n.n = bayes.normal.normal(y - X%*%beta, sigma, m, sqrt(w2[1]))
    beta0 = rnorm(1, mean = rv.n.n$post.mean, sd = rv.n.n$post.sd)
    
    ## Sample beta using Rue's algorithm
    bs = bayesreg.fastmvg2_rue(X/sigma, (y-beta0)/sigma, sigma^2*w2[2:(p+1)], XtX/sigma^2)
    beta = bs$x

    # Sample latent variable w[1]
    scale = ((beta0-m)^2 + 1)/2
    w2[1] = scale / rgamma(1, 1, 1)
    
    # Sample other latent variables w2[2...p+1]
    scale = (beta^2 + 1)/2
    w2[2:(p+1)] = scale / rgamma(p, 1, 1)

    ## Sample sigma | nu, beta0, beta, w2, y
    e2    = (y - beta0 - X%*%beta)^2
    shape = (n+p+1)/2
    scale = 1/nu + sum(e2)/2 + t(beta/w2[2:(p+1)]) %*% beta/2
    sigma = as.numeric(sqrt(scale / rgamma(1,shape,1)))

    # Sample nu | sigma
    scale = 1 + 1/sigma^2
    nu = scale / rgamma(1,1,1)
    
    # Compute likelihoods statistics (for WAIC)
    Lprob      = (1/2)*log(2*pi*sigma^2) + e2/2/sigma^2
    waicProb   = waicProb + exp(-Lprob)
    waicLProb  = waicLProb + Lprob
    waicLProb2 = waicLProb2 + Lprob^2
    
    # Store samples
    rv$beta.samples[1,i] = beta0
    rv$beta.samples[2:(p+1),i] = beta
    rv$sigma.samples[i] = sigma
  }
  
  # ===================================================================
  # Rescale the coefficients
  if (p == 1)
  {
    rv$beta.samples[2:(p+1),]  <- t(as.matrix(apply(t(rv$beta.samples[2:(p+1),]), 1, function(x)(x / stdX$stdX))))
  }
  else
  {
    rv$beta.samples[2:(p+1),]  <- as.matrix(apply(t(rv$beta.samples[2:(p+1),]), 1, function(x)(x / stdX$stdX)))
  }
  rv$beta.samples[1,] <- rv$beta.samples[1,] - stdX$meanX %*% rv$beta.samples[2:(p+1),]
  
  #rv$muBeta  <- rv$muBeta / t(stdX$stdX)
  #rv$muBeta0 <- rv$muBeta0 - stdX$meanX %*% rv$muBeta
  
  rv$stdX = stdX  
  
  ## Finally, compute posterior summary statistics
  rv$beta.mean   = rowMeans(rv$beta.samples)
  rv$beta.sd     = sqrt((rowSums(rv$beta.samples^2) - rowSums(rv$beta.samples)^2/n.samples)/n.samples)
  rv$beta.CI.75  = apply(rv$beta.samples,1,function(x) stats::quantile(x,probs=c(0.125,0.875)))
  rv$beta.CI.95  = apply(rv$beta.samples,1,function(x) stats::quantile(x,probs=c(0.025,0.975)))
  rv$t.stat      = rv$beta.mean / rv$beta.sd
  
  rv$sigma.mean  = mean(rv$sigma.samples)
  rv$sigma.sd    = sd(rv$sigma.samples)                       
  rv$sigma.CI.75 = quantile(rv$sigma.samples,probs=c(0.125,0.875))
  rv$sigma.CI.95 = quantile(rv$sigma.samples,probs=c(0.025,0.975))
 
  rv$WAIC        = -sum(log(waicProb/n.samples)) + sum(waicLProb2/n.samples) - sum((waicLProb/n.samples)^2)
   
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
  # in general we would use quantile function if it exists, i.e.
  # rv$CI = qnorm(p=(0.025,0.975), post$mean, post$sd)
  
  return(rv)
}

# ============================================================================================================================
# function to standardise columns of X to have mean zero and unit length
bayesreg.standardise <- function(X)
{
  n = nrow(X)
  p = ncol(X)
  
  # 
  r       = list()
  r$X     = X
  if (p > 1)
  {
    r$meanX = colMeans(X)
  } else
  {
    r$meanX = mean(X)
  }
  r$stdX  = t(apply(X,2,stats::sd)) * sqrt(n-1)
  
  # Perform the standardisation
  if (p == 1)
  {
    r$X <- as.matrix(apply(X,1,function(x)(x - r$meanX)))
    r$X <- as.matrix(apply(r$X,1,function(x)(x / r$stdX)))
  } else
  {
    r$X <- t(as.matrix(apply(X,1,function(x)(x - r$meanX))))
    r$X <- t(as.matrix(apply(r$X,1,function(x)(x / r$stdX))))
  }
  
  return(r)
}

# ============================================================================================================================
# function to generate multivariate normal random variates using Rue's algorithm
bayesreg.fastmvg2_rue <- function(Phi, alpha, d, PtP = NA)
{
  Phi   = as.matrix(Phi)
  alpha = as.matrix(alpha)
  r     = list()
  
  # If PtP not precomputed
  if (any(is.na(PtP)))
  {
    PtP = t(Phi) %*% Phi
  }
  
  p     = ncol(Phi)
  if (length(d) > 1)
  {
    Dinv  = diag(as.vector(1/d))
  }
  else
  {
    Dinv   = 1/d
  }
  L     = t(chol(PtP + Dinv))
  v     = forwardsolve(L, t(Phi) %*% alpha)
  r$m   = backsolve(t(L), v)
  w     = backsolve(t(L), stats::rnorm(p,0,1))
  
  r$x   = r$m + w
  return(r)
}

# ============================================================================================================================
# Predict function
predict.bayes.cauchy.linreg <- function(object, df)
{
  if (!inherits(object,"bayes.cauchy.linreg")) stop("Not a valid bayes.cauchy.linreg object")

  # Build the fully specified formula using the covariates that were fitted
  f <- stats::as.formula(paste("~",paste(attr(object$terms,"term.labels"),collapse="+")))
  
  # Extract the design matrix
  X = stats::model.matrix(f, data=df)
  X = as.matrix(X[,-1])
  n = nrow(X)
  p = ncol(X)
  
  # Make predictions
  yhat = X %*% as.vector(object$beta.mean[2:(p+1)]) + as.numeric(object$beta.mean[1])
  
  return(yhat)
}