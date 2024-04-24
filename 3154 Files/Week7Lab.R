####################################################################################
#	Script-file:   studio7.code.R
#	Project:       FIT3154 - Studio 7
# Author:        Daniel Schmidt
#
# Purpose:  	   Code for questions in Studio 7 
####################################################################################
install.packages("rstan")
library(rstan)
setwd("D:/Desktop/R files/3154 Files")
load("studio7.RData")
view(my.density.plot)

# ----------------------------------------------------------------------------------
# Question 2: Beta-Bernoulli
# ----------------------------------------------------------------------------------

# 2.3
my.data = list(N = 12, y = c(0,1,0,1,0,1,1,0,1,1,1,0), a=1, b=1)

# 2.4
fit <- stan(file = "beta.binomial.stan", data = my.data, iter = 2000, chains = 4)

fit = fit.q2.4

# 2.5
fit
# lp is the numerator, 4000 lps, find the maximum and return it

# 2.6
k = sum(my.data$y) # total number of success
n = my.data$N
a = 1
b = 1

(k+a)/(n+a+b) # close to theta mean

sqrt( (k+a)*(n-k+b) / (n+a+b)^2 / (n+a+b+1) ) # sd

qbeta(k+a,n-k+b,p=c(0.025,0.975)) # 95% confidence interval

# 2.7
params = as.data.frame(extract(fit))
hist(params$theta,probability = T,xlab="theta")
t = seq(0,1,by=0.001)
lines(x=t,y=dbeta(t,7+1,5+1),col="red")
#sampling did well, close match to the exact posterior distribution,
# as would be expected from a simple problem

# 2.8
(k+a-1)/(n+a+b-2)

params$theta[which.max(params$lp__)]
# this looks for the sample with the largest log-posterior probability
# and then finds the theta value for this sample, oe tries to find the mode
# from the samples we have drawn

# close to the exact mode -- maximum found by this method will always be less accurate than the posterior 
# mean 

# 2.9
my.data = list(N = 12, y = c(0,1,0,1,0,1,1,0,1,1,1,0), a=11, b=1)
fit <- stan(file = "beta.binomial.stan", data = my.data, iter = 1000, chains = 4)
fit = fit.q2.9
fit
params = as.data.frame(extract(fit))
hist(params$theta,probability = T,xlab="theta")
t = seq(0,1,by=0.001)
lines(x=t,y=dbeta(t,k+11,n-k+1),col="red")
#goes towards prior beliefs

# ----------------------------------------------------------------------------------
# Question 3: Simple Bayesian Logistic Regression
# ----------------------------------------------------------------------------------

rm(list=ls())
library(rstan)
source("my.density.plot.R")

# 3.2
maize = read.csv("maize.snp.csv")

# 3.3
fit.lr = glm(FM ~ .,data=maize,family="binomial")
summary(fit.lr)


# 3.4
my.data = list(N=2266, y=maize$FM, x=maize$SNP.1)
fit.stan = stan(file="bayes.simple.logreg.stan", data=my.data, iter=2e3)
fit.stan = fit.q3.4
fit.stan

#3.4a
# post mean and sd very close to max likelihood


#3.4b
#not include 0 , it is likely associated with target

# 3.5
params = extract(fit.stan)
mean(params$beta1)/sd(params$beta1)
# the bayesian t-stat is basically the same as z-scpre

# 3.6
my.density.plot(params$beta1,xlabel="beta1",ylabel="P(beta1 | y)",bins=20)


# 3.7
beta0.mu = mean(params$beta0)
beta1.mu = mean(params$beta1)
px.0 = 1 / (1 + exp(-beta0.mu)) # x = 0
px.1 = 1 / (1 + exp(-beta0.mu - beta1.mu)) # x = 1
px.2 = 1 / (1 + exp(-beta0.mu - beta1.mu*2)) # x = 2

px.1 / px.0

px.2 / px.0

# 3.8
theta.x0 = 1 / (1 + exp(-params$beta0))
theta.x0

my.density.plot(theta.x0,xlabel="theta | x=0",ylabel="P((theta | x=0) | y)",bins=20)
mean(theta.x0)
quantile(theta.x0, p=c(0.025,0.125,0.5,0.875,0.975))


# 3.9
theta.x1 = 1 / (1 + exp(-params$beta0 - params$beta1))

my.density.plot(theta.x1, xlabel = "theta | x=1", bins=20)
mean(theta.x1) # higher than previous
quantile(theta.x1,p=c(0.025,0.125,0.5,0.875,0.975)) # distribution is shifted to higher prob

# x = 2
theta.x2 = 1 / (1 + exp(-params$beta0 - params$beta1*2))

my.density.plot(theta.x2, xlabel = "theta | x=2", bins=20)
mean(theta.x2)
quantile(theta.x2,p=c(0.025,0.125,0.5,0.875,0.975))


# ----------------------------------------------------------------------------------
# Question 4: Extending Our Bayesian Logistic Regression
# ----------------------------------------------------------------------------------

# 4.3
my.data = list(N=2266,P=1,y=maize$FM,x=as.matrix(maize$SNP))
fit.stan = stan(file = "bayes.logreg.stan", data=my.data, iter=2e3)
fit.stan = fit.q4.3
fit.stan

# 4.4
maize.5snps = read.csv("maize.5snps.csv")
my.data = list(N=2266,P=5,y=maize.5snps$FM,x=as.matrix(maize.5snps[,-1]))
fit.stan = stan(file = "bayes.logreg.stan", data=my.data, iter=2e3)
fit.stan = fit.q4.4
fit.stan

# 4.5
params = as.data.frame(extract(fit.stan))
params
params = params[,-7]
apply(params,2,function(x) mean(x)) / apply(params,2,function(x) stats::sd(x))

# 4.6
apply(params,2,function(x) stats::quantile(x,probs=c(0.125,0.875)))
# only beta 4 includes 0 so we cross it out

# 4.7
theta = 1/(1 + exp(-params$beta0 - params$beta.1*2 - params$beta.3*2))
# beta 2 and 5 is neg so set to 0
# beta 4 also 0
my.density.plot(theta, xlabel="theta", ylabel="P(theta | y)", bins=20)
mean(theta)
quantile(theta,p=c(0.025,0.975))

