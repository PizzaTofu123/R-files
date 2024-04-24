setwd("D:/Desktop/R files/3154 Files")
batting = read.csv("batting.csv")

#2.2
sum ((batting$First90 - batting$Truth)^2)

#2.3
# Implementation of the James-Stein estimator
# FIT3154, Studio 2
my.JS <- function(x)
{
  # Dimensionality of vector
  p = length(x)
  
  # Compute the shrinkage factor
  c = as.numeric( 1 - (p-2)/(sum(x^2)) )
  
  # Shrink and return estimates
  rv = list()
  rv$muhat = c*x
  rv$c     = c
  
  return(rv)
}

n = 90
x = sqrt(4*n)*asin(sqrt(batting$First90))
rv = my.JS(x)
mu_js = rv$muhat
rv$c # little shrinkage
theta_js = (sin(mu_js/sqrt(4*n)))^2
sum((theta_js-batting$Truth)^2)
# JS estimator is worse than MLE
# because jse guarantees for infinite samples are going to perform better than mle

#2.4
boxplot(x)

#2.5
# Implementation of the James-Stein estimator with Lindley modification
# FIT3154, Studio 2
my.JSL <- function(x)
{
  # Dimensionality of vector
  p = length(x)
  
  # Compute the grand mean
  m = mean(x)
  
  # Compute the differences from the grand mean
  d = x - m
  
  # Compute the shrinkage factor
  c = as.numeric( 1 - (p-3)/(sum(d^2)) )
  
  # Shrink onto grand mean, and return
  rv = list()
  rv$muhat = (c*d + m)
  rv$c     = c
  
  return(rv)
}

rv = my.JSL(x)
mu_jsl = rv$muhat
rv$c
theta_jsl = sin(mu_jsl/sqrt(4*n))^2
sum((theta_jsl - batting$Truth)^2)

#js good for all not for single data point

#2.6
batting$JSL = theta_jsl

#3.1
# Shrunken least squares using leave-one-out cross validation
#
# FIT3154 Studio 4
#
shrunken.lm.cv <- function(formula, data, c)
{
  # Figure out the target variable
  mf = model.frame(formula,data=data)
  target.var = names(mf)[[1]]   # <------ THIS TELLS YOU THE NAME OF THE COLUMN OF THE TARGET
  #         I.E., data[,target.var] IS YOUR "y"
  
  # Save the data before modifying it
  dataOld = data
  
  # Zero-mean the predictors to make the intercept estimate "decouple" from the coefficients
  p = ncol(data) - 1
  Xmean = colMeans(as.matrix(data[,2:(p+1)]))
  for (i in 1:p) 
  { 
    data[,i+1] = data[,i+1]-Xmean[i] 
  }
  
  # Compute the LS estimates for each of the CV partitions  
  n = nrow(data)
  cv.err = rep(0,length(c))
  y = data[,target.var]
  for (i in 1:n)
  {
    # Fit the model to all but the i-th data point
    ls.est = lm(formula,data=data[-i,])
    
    # Make a prediction for the withheld data point
    # Remember: yp = X*b + b0
    yp = predict(ls.est, data[i,])
    
    # Adjust the prediction for all the different shrinkage values we are trying
    #  Use the fact that: yp.shrunk = c*X*b + b0   =>  yp.shrunk = (yp - b0)*c + b0
    b0 = ls.est$coefficients["(Intercept)"]
    yp.shrunk = (yp - b0) * c + b0
    
    # Compute the CV test error for all values of c
    cv.err = cv.err + (data[i,target.var] - yp.shrunk)^2
  }
  
  # Now we have figured out which is the "best" kappa (smallest CV error)
  # we can fit a linear model using all the data and adjust the coefficients appropriately
  best.c = c[which.min(cv.err)]
  
  fit.lm = lm(formula,data=dataOld)
  fit.lm$coefficients[2:(p+1)] = fit.lm$coefficients[2:(p+1)]*best.c  
  
  # Adjust the intercept so it fits the data better after shrinking the coefficients
  fit.lm$coefficients[1] = fit.lm$coefficients[1]*best.c + (1-best.c)*mean(y)
  
  # Return the CV errors and the "best" kappa (smallest CV error)
  retval = list()
  retval$fit.lm = fit.lm
  retval$c = c
  retval$cv.err = cv.err/n
  retval$best.c = c[which.min(cv.err)]
  
  retval
}

#3.2

diatrain = read.csv("diabetes.train.csv")
diatest = read.csv("diabetes.test.csv")

#3.3
fit.lm = lm(Y ~ ., data=diatrain)
Yp = predict(fit.lm, diatest)

#3.4
mean((diatest$Y - Yp)^2)

#3.5
rv = shrunken.lm.cv(Y ~ ., diatrain, c=seq(0,1,length.out=500))
plot(rv$c, rv$cv.err, ty="l", xlab="c", ylab="LOO CV Error")
#starts introducing bias as c shrinks and the bias outweighs the reductinon of 
#the variance
rv$best.c

#3.6
Yp.shrunk = predict(rv$fit.lm, diatest)
mean((diatest$Y - Yp.shrunk)^2)
mean((diatest$Y - Yp)^2)
#improved but not much

#3.7
# noise is big and signal small is cool and good and less shrinkage (makes c close to 1)
# the signal is the total sqrd - mean and variance of the total power, and we are left with the information/signal

#3.8
p = ncol(diatrain)-1
n = nrow(diatrain)
TSS = sum(diatrain$Yˆ2)
ybar = mean(diatrain$Y)
sigma2 = mean( fit.lm$residuals^2 )
c = 1 - sigma2*(p-2)/(TSS - n*(ybar^2 + sigma2))
c
rv$best.c

#4.1
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
source("cv.risk.ls.R")

#4.2
wine = read.csv("wine.csv")

rv = cv.risk.ls(quality ~ ., data=wine, train.prop = 0.01, niter = 100)
rv$av.MSPE
rv = cv.risk.ls(quality ~ ., data=wine, train.prop = 0.02, niter = 100)
rv$av.MSPE
rv = cv.risk.ls(quality ~ ., data=wine, train.prop = 0.05, niter = 100)
rv$av.MSPE
rv = cv.risk.ls(quality ~ ., data=wine, train.prop = 0.1, niter = 100)
rv$av.MSPE

#4.3
source("cv.risk.R")

#4.4
rv = cv.risk(quality ~ ., data=wine, train.prop = 0.01, niter = 20)
rv$av.MSPE
rv$av.nz.p
rv = cv.risk(quality ~ ., data=wine, train.prop = 0.02, niter = 20)
rv$av.MSPE
rv$av.nz.p
rv = cv.risk(quality ~ ., data=wine, train.prop = 0.05, niter = 20)
rv$av.MSPE
rv$av.nz.p

#4.5
my.formula = my.make.formula("quality",wine,use.squares=T)
rv = cv.risk(my.formula, data=wine, train.prop = 0.01, niter = 20)
rv$av.MSPE
rv$av.nz.p

rv = cv.risk(my.formula, data=wine, train.prop = 0.02, niter = 20)
rv$av.MSPE
rv$av.nz.p

rv = cv.risk(my.formula, data=wine, train.prop = 0.05, niter = 20)
rv$av.MSPE
rv$av.nz.p

rv = cv.risk(my.formula, data=wine, train.prop = 0.2, niter = 20)
rv$av.MSPE
rv$av.nz.p









