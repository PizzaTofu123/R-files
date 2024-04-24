####################################################################################
#	Script-file:   studio9.solns.R
#	Project:       FIT3154 - Studio 9
# Author:        Daniel Schmidt
#
# Purpose:  	   Solutions for questions in Studio 9
####################################################################################

setwd("/Users/Daniel/Dropbox/Teaching 2020/FIT3154 S2 2020/Studios/Studio 9/")

rm(list=ls())
library(rstan)
library(ggplot2)

source("my.density.plot.R")

# ----------------------------------------------------------------------------------
# Question 1: Baseball data, revisited
# ----------------------------------------------------------------------------------

# 2.2
batting = read.csv("batting.jsl.csv")
my.data = list(N = 18, n = rep(90, 18), y = round(batting$First90*90))

# 2.3
fit <- stan(file = "bayes.binomial.shrink.stan", data = my.data, iter = 2e4, chains = 4)
fit

# theta0 is the estimate of the cohort average probability of success

# 2.4
params = extract(fit)

theta.hat = colMeans(params$theta)

plot(batting$First90, batting$First90, xlab="ML Estimate", ylab="Estimates")
points(batting$First90, batting$JSL, col="red")
points(batting$First90, colMeans(params$theta), col="blue")

# JS estimates are shrunken, slope is < 1
# Bayes estimates are even more shrunken, slope is smaller
# => Probabilities smaller than cohort average pushed upwards towards cohort average
#    Probabilities larger than cohort average pushed downards towards cohort average

# 2.5
sum((theta.hat - batting$Truth)^2)
sum((batting$First90 - batting$Truth)^2)  # ML estimates
sum((batting$JSL - batting$Truth)^2)      # JSL estimates

# JSL estimates obtain about 50% savings in squared error over unshrunken ML estimates
# Bayes estimates make a small improvement over JSL estimates

# 2.6
my.density.plot(params$theta[,18])
# Distribution is negatively skewed with most probability massed towards the cohort average
# of 0.25

# 2.7
my.density.plot(params$theta[,1])
# Distribution is positive skewed with most probability massed towards the cohort average
# of 0.25.
# => as above, values larger than cohort average are shrunk downwards towards 0.25
#    values smalelr than cohort average are shrunk upwards towards 0.25

# 2.8
CI.95 = apply(params$theta,2,function(x) stats::quantile(x,probs=c(0.025,0.975)))
((CI.95[1,] < batting$Truth) & (CI.95[2,] > batting$Truth))
# We would expect 18 * 0.05 \approx 1 value to fall outside of the interval, as there
# is a 0.05 chance the interval will not contain the truth
# We see 2 values outside of intervals, which is not too bad
# In fact, probability of this occuring is
dbinom(2, 18, 0.05)
# which is around 16%. So hardly unlikely. So our priors and probability model seem
# like they may be decent matches to reality.


# ----------------------------------------------------------------------------------
# Question 3: Simple One-Dimensional Optimisation
# ----------------------------------------------------------------------------------

# 3.1
bpdata = read.csv("bpdata.csv")

n = length(bpdata$BP)
Y = sum(bpdata$BP)
sigma2 = var(bpdata$BP)
m = 118
s = 0.31

# 3.2
lp = function(mu, sigma2, Y, n, m, s) { 1/2/sigma2*(-2*Y*mu + n*mu^2) + log(1+(m-mu)^2/s^2) }
mu = seq(110,120,length.out=1e3)
plot(mu, lp(mu,sigma2,Y,n,m,s), ty="l", ylab="h(mu)")
# There are 3 turning points. This makes sense as a cubic has three roots, and
# a turning point is a point where the derivative is zero

# 3.3
c0 = s^2*Y + m^2*Y + 2*m*sigma2
c1 = -2*m*Y - 2*sigma2 - n*s^2 - m^2*n
c2 = Y + 2*m*n
c3 = -n
r = polyroot(c(c0,c1,c2,c3))

points(r, lp(r,sigma2,Y,n,m,s), col="red")
r
# All roots are real.
# The roots of the cubic are all located at the turning points of the negative log-posterior
# (i.e., our objective function). Visually it would seem that the one near 115 
# would be the one that minimises our negative log-posterior

# 3.4
lp(r, sigma2, Y, n, m, s)
r
mean(bpdata$BP)
# The solution 114.9594 minimises the negative log-posterior
# Compared to the sample mean of 114, it is larger as it is pushed towards
# the prior 'best guess' of m=118

# 3.5
m = 122
plot(mu, lp(mu,sigma2,Y,n,m,s), ty="l", ylab="h(mu)")
# There now appears to be only a single turning point, at least in the range
# we are examining

# 3.6
c0 = s^2*Y + m^2*Y + 2*m*sigma2
c1 = -2*m*Y - 2*sigma2 - n*s^2 - m^2*n
c2 = Y + 2*m*n
c3 = -n
polyroot(c(c0,c1,c2,c3))
# Two of the roots are now complex, and are thus not permissible solutions for
# the mean of a normal distribution.
# The solution 114.3865 is the single real solution that minimises the 
# negative log-posterior.


# ----------------------------------------------------------------------------------
# Question 4: Optimisation by Coordinate-wise Descent
# ----------------------------------------------------------------------------------

# 4.2
mu = mean(bpdata$BP)
S = sum((bpdata$BP - mu)^2)
polyroot(c(-S, n-S, n+2))
# One of the two solutions is negative, which is not permissble for a variance,
# so the solution must be sigma2 = 25.542, which implies that sigma equals ...
sqrt(25.542)

# 4.3
source("bayes.cauchy.normal.MAP.R")

# 4.4
rv.map = bayes.cauchy.normal.MAP(bpdata$BP, m=122, s=0.31)
rv.map

# 4.5
source("bayes.cauchy.normal.R")
rv = bayes.cauchy.normal(bpdata$BP, m=122, s=0.31, n.samples=1e5)

mean(rv$mu.samples)
mean(rv$sigma.samples)

rv.map

# The MAP and mean estimate for mu seem about the same
# The MAP estimate for sigma seems substantially (around 10%) smaller than the
# posterior mean estimate for sigma

my.density.plot(rv$mu.samples)
# The histogram for mu shows the posterior distribution for mu seems to be roughly
# symmetric, so the posterior mean and mode will be very similar

my.density.plot(rv$sigma.samples)
# The histogram for sigma shows the posterior distribution for sigma seems to be
# postively skewed with a heavy tail for larger sigma
# This will 'drag' the mean towards infinity a bit, resulting in a larger mean
# than a mode.