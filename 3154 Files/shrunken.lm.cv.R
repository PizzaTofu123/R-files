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