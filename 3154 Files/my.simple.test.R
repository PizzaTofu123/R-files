# Simple one-at-a-time testing of high dimensional data
# FIT3154 Studio 8
#
my.simple.test <- function(target, data, family)
{
  p = ncol(data)-1
  
  # Find the column containing our target
  I = (names(data)==target)
  
  # Do one-at-a-time testing on all other variables
  z = rep(0,p)
  p.value = rep(0,p)
  k = 1
  for (i in 1:(p+1))
  {
    if (!(i %% 100))
    {
      cat("Tested ", i, "/", p, "\n")
    }
    
    L = levels(data[,target])
    
    # If it is not the target, test for association
    if (!I[i])
    {
      f = as.formula(paste(target, "~", names(data)[i]))
      rv = simple.logreg.test(f, data, family)
      
      # Extract statistics
      z[k] = rv$z
      p.value[k] = rv$p.value
      
      k = k+1
    }
  }
  return(list("z"=z,"p.value"=p.value))
}

# simple significance test
simple.logreg.test <- function(formula,data,family)
{
  fit = glm(formula,data=data,family=family)
  dev.diff = fit$null.deviance - fit$deviance
  p.value = 1-pchisq(dev.diff, 1)
  z = sign(summary(fit)$coefficients[2]) * sqrt(dev.diff)
  
  return(list(z=z,p.value=p.value))
}


# make a formula from a target and list of variable names
my.simple.make.formula <- function(target, varnames)
{
  s = paste(target,"~")
  for (i in 1:length(varnames))
  {
    s = paste(s,varnames[i])
    if (i < length(varnames))
    {
      s=paste(s,"+")
    }
  }
  as.formula(s)
}