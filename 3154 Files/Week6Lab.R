############################################################################################################
# Question 2
rm(list=ls())

# Q2.1
source("bayes.cauchy.normal.R")

# Q2.2
bpdata = read.csv("bpdata.csv")

s = 0.31
pcauchy(126,122,s) - pcauchy(118,122,s)
# Close enough to 95%

# Q2.4
rv = bayes.cauchy.normal(bpdata$BP, m=0, s=1, n.samples=1e6)
]
# Q2.5
hist(rv$mu.samples, probability=T)

# Q2.5a
mean(rv$mu.samples)

# Q2.5b
sd(rv$mu.samples)

# Q2.5c
quantile(p=c(0.025,0.975), rv$mu.samples)

# Q2.5d
# mean is much closer to sample mean
# posterior s.d. is actually a little smaller (why? -- because we are also now estimated sigma, not assuming
# it is equal to 13.5)
# CI is shorter but includes the sample mean

# Q2.6a
hist(rv$sigma.samples, probability=T)
mean(rv$sigma.samples)

# Q2.6b
quantile(p=c(0.025,0.975), rv$sigma.samples)
# our estimated value of sigma for this population is nowhere near the 13.5 we assumed last week
# obviously our population of interest is a sub-population from the general US population
# that may have been selected for a specific disease etc. that might affect blood pressure?


############################################################################################################
# Question 3

rm(list=ls())

# Q3.1
diabetes.train = read.csv("diabetes.train.studio6.csv")
diabetes.test  = read.csv("diabetes.test.studio6.csv")

# Q3.2
fit.lm6 = lm(Y ~., diabetes.train)
summary(fit.lm6)
# SEX, BMI, BP, S1, S2 and S5 appear strongly associated (very small p-values)

# Q3.3
mean((diabetes.test$Y - predict(fit.lm,diabetes.test))^2)

# Q3.4
library(bayesreg)

# Q3.5
fit.bayes6 = bayesreg(Y ~., model="gaussian", diabetes.train, prior="lasso", n.samples = 2e4)
fit.bayes.sum6 = summary(fit.bayes6)

fit.lm6$coefficients
t(fit.bayes.sum6$mu.coef)

summary(fit.lm6)
t(fit.bayes.sum6$se.coef)
# Posterior means:
#  * a little smaller for BMI, BP
#  * substantial shrinkage for SEX, S3, S4, S5
#  * heavily shrunk for AGE, S1, S2
#
# Posterior SD's for S1, S2, S3, S4, S5 are substantially smaller due to shrinkage

# Q3.6
summary(fit.lm6)
t(fit.bayes.sum6$t.stat)
# t-stats:
#  * t-stat for AGE is substantially smaller due to shrinkage
#  * t-stats for S1, S2, S3, S4, S5 are all reduced
#  *  other t-stats are somewhat similar or a bit reduced; though
#     S6 has increased, probably due to other predictors being shrunk

# Q3.7
# AGE
hist(fit.bayes$beta[1,], probability=T)
# seems to be basically centered on, and widely spread around zero, so unlikely to be important

# SEX
hist(fit.bayes$beta[2,], probability=T)
# is centered away from zero -- appears very likely to be associated

# Q3.8
mean((diabetes.test$Y - predict(fit.bayes,diabetes.test))^2)
# The prediction error is better, because of the shrinkage
# provided by the Laplace prior distributions

# Q3.9
fit.bayes$waic

# Q3.10
summary(fit.bayes,CI=75)
# variables for which 75% CI (i.e., 12.5% to 87.5%) does not contain zero are: SEX, BMI, BP, S5
# these are clearly the variables with the strongest association

# Q3.11
# AGE has the smallest t-stat, so lets start by removing that
fit.bayes.red = bayesreg(Y ~ SEX + BMI + BP + S1 + S2 + S3 + S4 + S5 + S6, 
                         prior = "lasso", diabetes.train,n.samples = 2e4)
fit.bayes.red$waic
# The WAIC is ever so slightly smaller

# Q3.12
mean((diabetes.test$Y - predict(fit.bayes.red,diabetes.test))^2)
# We have managed a *very* small reduction in prediction error; suggesting we
# can get about the same performance with a reduced model not including AGE

# Q3.13
t(fit.bayes$mu.beta)
t(fit.bayes.red$mu.beta)
# they are very similar, though SEX, S2 and S4 are a little bigger
# overall the two models are very close, suggesting the variable
# we removed had a very marginal association with y at best

# Q3.14
# Variable S2 has the smallest t-stat, so let's try removing that ...
fit.bayes.red.2 = bayesreg(Y ~ SEX + BMI + BP + S1 + S3 + S4 + S5 + S6, 
                           prior = "lasso", diabetes.train,n.samples = 2e4)
fit.bayes.red.2$waic
# similar WAIC, but less variables so might fit future data better?

# Let's test
mean((diabetes.test$Y - predict(fit.bayes.red.2,diabetes.test))^2)
# Another small improvement, so it seems S2 is superfluous

fit.bayes.red.2$waic
# t-stats for S1 and S5 have increased quite a bit
# this is because removing variables allows other variables to "take up the slack"
# and get their chance to shine ...

# Q3.15
fit.bayes.int = bayesreg(Y ~ . + .*., prior = "lasso", diabetes.train,n.samples = 2e4)
mean((diabetes.test$Y - predict(fit.bayes.int,diabetes.test))^2)
# A small improvement despite adding many more predictors

# Q3.16
summary(fit.bayes.int, sort.rank=T)
# Most variables added seem unimportant
# SEX has drastically reduced in effect size due to the extra predictors being
# added, but the shrinkage helps avoid overfitting







############################################################################################################
# Question 4
# 4.2a
alpha = 1
beta = 1
theta = rbeta(1e6,alpha,beta)
L = log(theta/(1-theta))
hist(L,breaks=100,probability=T)
quantile(L,c(0.025,0.975))

# 4.2b
alpha = 5
beta = 5
theta = rbeta(1e6,alpha,beta)
L = log(theta/(1-theta))
hist(L,breaks=100,probability=T)
quantile(L,c(0.025,0.975))

# 4.2c
alpha = 1/2
beta = 1/2
theta = rbeta(1e6,alpha,beta)
L = log(theta/(1-theta))
hist(L,breaks=100,probability=T)
quantile(L,c(0.025,0.975))

# 4.2d
alpha = 1
beta = 5
theta = rbeta(1e6,alpha,beta)
L = log(theta/(1-theta))
hist(L,breaks=100,probability=T)
quantile(L,c(0.025,0.975))

# 4.3
alpha = 1
beta = 1
theta = rbeta(1e6,alpha,beta)
L = log(theta/(1-theta))
hist(L,breaks=100,probability=T)

# Plot exact probability distribution over histogram
L = seq(-10,10,by=0.01)
lines(L, exp(-L)/(1+exp(-L))^2, col="red")
# a good match ...

# 4.4a
L = rnorm(1e6,0,1)
theta = 1/(1+exp(-L))
hist(theta, breaks=100, probability=T)

# 4.4b
L = rnorm(1e6,0,2)
theta = 1/(1+exp(-L))
hist(theta, breaks=100, probability=T)

# 4.4
L = rnorm(1e6,1,1)
theta = 1/(1+exp(-L))
hist(theta, breaks=100, probability=T)

# 4.6
check.theta.prior <- function(m, s)
{
  L = rnorm(1e6,m,s)
  theta = 1/(1+exp(-L))
  hist(theta, breaks=100, probability=T)
  
  t = seq(0.001,1-0.001,by=0.001)
  lines(t, 1/t/(1-t) * (1/2/pi/s^2)^(1/2) * exp(-(log(t)-log(1-t)-m)^2/2/s^2), col="red")  
}

# L ~ N(m=0,s=1)
check.theta.prior(0,1)

# L ~ N(m=0,s=2)
check.theta.prior(0,2)

# L ~ N(m=1, s=1)
check.theta.prior(1,1)

# you can play around with m, s and see how much the prior on theta changes