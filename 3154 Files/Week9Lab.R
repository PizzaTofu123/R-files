####################################################################################
#	Script-file:   studio9.solns.R
#	Project:       FIT3154 - Studio 9
# Author:        Daniel Schmidt
#
# Purpose:  	   Solutions for questions in Studio 9
####################################################################################

setwd("D:/Desktop/R files/3154 Files")

rm(list=ls())
library(rstan)
library(ggplot2)
load("bayes.binomial.shrink.results.Rdata")

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


# 2.4
params = extract(fit)

theta.hat = colMeans(params$theta)

plot(batting$First90, batting$First90, xlab="ML Estimate", ylab="Estimates")
points(batting$First90, batting$JSL, col="red")
points(batting$First90, colMeans(params$theta), col="blue")

# 2.5
sum((theta.hat - batting$Truth)^2)
sum((batting$First90 - batting$Truth)^2)  # ML estimates
sum((batting$JSL - batting$Truth)^2)      # JSL estimates

source("my.density.plot.R")
# 2.6
my.density.plot(params$theta[,18])

# 2.7
my.density.plot(params$theta[,1])

# 2.8
CI.95 = apply(params$theta,2,function(x) stats::quantile(x,probs=c(0.025,0.975)))
((CI.95[1,] < batting$Truth) & (CI.95[2,] > batting$Truth))

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

# 3.3
c0 = s^2*Y + m^2*Y + 2*m*sigma2
c1 = -2*m*Y - 2*sigma2 - n*s^2 - m^2*n
c2 = Y + 2*m*n
c3 = -n
r = polyroot(c(c0,c1,c2,c3))


points(r, lp(r,sigma2,Y,n,m,s), col="red")
r

# 3.4
lp(r, sigma2, Y, n, m, s)
r
mean(bpdata$BP)

# 3.5
m = 122
plot(mu, lp(mu,sigma2,Y,n,m,s), ty="l", ylab="h(mu)")

# 3.6
c0 = s^2*Y + m^2*Y + 2*m*sigma2
c1 = -2*m*Y - 2*sigma2 - n*s^2 - m^2*n
c2 = Y + 2*m*n
c3 = -n
polyroot(c(c0,c1,c2,c3))


# ----------------------------------------------------------------------------------
# Question 4: Optimisation by Coordinate-wise Descent
# ----------------------------------------------------------------------------------

# 4.2
mu = mean(bpdata$BP)
S = sum((bpdata$BP - mu)^2)
polyroot(c(-S, n-S, n+2))

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

my.density.plot(rv$mu.samples)
hist(rv$mu.samples, probability = T, breaks = 100)

my.density.plot(rv$sigma.samples)
hist(rv$sigma.samples, probability = T, breaks = 100)


