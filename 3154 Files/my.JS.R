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