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