library(glmnet)
source("wrappers.R")

# Cross-validation estimation of risk using real data
#
#  Compares least squares against shrunken LS, lasso and step-wise regression with BIC
#  You could easily extend this to add your own methods
#
# FIT3154 Studio 4
#
cv.risk <- function(formula, data, train.prop, niters = 100)
{
  # Error checking
  if (train.prop <= 0 || train.prop >=1)
  {
    stop("train.prop must be between 0 and 1")
  }
  
  # Return values
  rv = list()
  rv$MSPE = matrix(0, niters, 4)
  colnames(rv$MSPE)=c("LS","Shrunken LS","Lasso","SW-BIC")

  # Figure out the target variable
  mf = model.frame(formula,data=data)
  target.var = names(mf)[[1]]

  # Do the CV experiment
  n = nrow(data)
  Ix = 1:n
  train.size = floor(train.prop*n)

  # 
  rv$n.nz.p = matrix(0,niters,4)
  colnames(rv$n.nz.p)=c("LS","Shrunken LS","Lasso","SW-BIC")
  
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
    rv$n.nz.p[i,1] = length(coefficients(fit.lm)) - 1
    
    ## Fit with shrunken least-squares and predict onto testing data
    fit.lm = shrunken.lm(formula, data=trData)
    rv$MSPE[i,2] = mean( (tstData[,target.var] - predict(fit.lm,newdata=tstData))^2 )
    rv$n.nz.p[i,2] = rv$n.nz.p[i,1]
    
    ## Fit with lasso and predict onto testing data
    fit.lasso = cv.glmnet.f(formula, data=trData, alpha=1)
    rv$MSPE[i,3] = mean( (tstData[,target.var] - predict.glmnet.f(fit.lasso, data=tstData))^2 )
    rv$n.nz.p[i,3] = sum(coefficients(fit.lasso)!=0)
    
    # Fit with stepwise regression using BIC
    fit.sw.bic = step(lm(formula, data=trData), k = log(nrow(trData)), trace=0)
    rv$MSPE[i,4] = mean( (tstData[,target.var] - predict(fit.sw.bic, newdata=tstData))^2 )
    rv$n.nz.p[i,4] = sum(coefficients(fit.sw.bic)!=0)
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
  target.var = names(mf)[[1]]   
  
  # First fit with least-squares
  fit.lm = lm(formula,data=data)
  
  # Now shrink the coefficients using formulas from Studio 4, Q3
  n = nrow(data)
  p = length(fit.lm$coefficients)-1
  
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