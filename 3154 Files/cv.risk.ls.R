library(glmnet)
source("wrappers.R")

# Cross-validation estimation of risk for least-squares using real data
# FIT3154 Studio 4
cv.risk.ls <- function(formula, data, train.prop, niters = 100)
{
  # Error checking
  if (train.prop <= 0 || train.prop >=1)
  {
    stop("train.prop must be between 0 and 1")
  }
  
  # Return values
  rv = list()
  rv$MSPE = matrix(0, niters, 1)
  colnames(rv$MSPE)=c("LS")
  
  # Figure out the target variable
  mf = model.frame(formula,data=data)
  target.var = names(mf)[[1]]
  
  # Do the CV experiment
  n = nrow(data)
  Ix = 1:n
  train.size = floor(train.prop*n)
  
  # 
  rv$n.nz.p = matrix(0,niters,1)
  colnames(rv$n.nz.p)=c("LS")
  
  for (i in 1:niters)
  {
    # Split data into training/test groups (use ?sample to see how it works)
    trIx = sample(Ix, train.size)
    tstIx = Ix[-trIx]
    
    trData = data[trIx,]
    tstData = data[tstIx,]
    
    ## Fit with least squares and predict onto testing data
    fit.lm = lm(formula, data=trData)
    rv$MSPE[i,1] = mean( (tstData[,target.var] - predict(fit.lm,newdata=tstData))^2 )
    rv$n.nz.p[i,1] = length(coefficients(fit.lm))
  }
  
  # Compute the average MSPE
  rv$av.MSPE = colMeans(rv$MSPE)
  rv$av.nz.p = colMeans(rv$n.nz.p)
  
  return(rv)
}

# Shrunken least-squares
shrunken.lm <- function(formula, data)
{
  # Figure out the target variable
  mf = model.frame(formula,data=data)
  target.var = names(mf)[[1]]   # <------ THIS TELLS YOU THE NAME OF THE COLUMN OF THE TARGET
  #         I.E., data[,target.var] IS YOUR "y"
  
  # First fit with least-squares
  fit.lm = lm(formula,data=data)
  
  # Now shrink the coefficients using formulas from Studio 2
  n = nrow(data)
  p = length(fit.lm$coefficients)-1
  
  ## SOLUTIONS
  TSS  = sum(data[,target.var]^2)
  ybar = mean(data[,target.var])
  sigma2 = mean( fit.lm$residuals^2 )
  
  kappa = 1 - sigma2*(p-2)/(TSS - n*(ybar^2 + sigma2))
  alpha = (1-kappa)*ybar
  
  # Adjust coefficients
  fit.lm$coefficients[1] = fit.lm$coefficients[1]*kappa + alpha
  fit.lm$coefficients[2:(p+1)] = fit.lm$coefficients[2:(p+1)]*kappa
  
  return(fit.lm)
}