####################################################################################
#	Script-file:   studio8.solns.R
#	Project:       FIT3154 - Studio 8
# Author:        Daniel Schmidt
#
# Purpose:  	   Solutions for questions in Studio 8
####################################################################################

rm(list=ls())

# Install gam package first
library(mgcv)

# ----------------------------------------------------------------------------------
# Question 2: Simple Smoothing
# ----------------------------------------------------------------------------------

# 2.1
my.f1 <- function(x) {1/(1+x^2) + x*cos(5*x) }

n = 100
x = sort(runif(100,-1,1))
y = my.f1(x) + rnorm(n, 0, 0.1)
plot(x,y)

# 2.2
x.test = seq(-1,1,by=0.001)
y.test = my.f1(x.test)
lines(x.test,y.test)

sqrt(var(my.f1(x.test)))/0.1 # close to 4

# If we want an SNR of 1 we would need to choose a sigma of
# SNR = sqrt(var(f))/sigma, so sigma = sqrt(var(f))/SNR
#
sqrt(var(my.f1(x.test))) / 1  # For SNR = 1
sqrt(var(my.f1(x.test))) / 10 # For SNR = 10
#
# You clearly need 10 times smaller sigma to increase SNR by 10 ...

# 2.3
df = data.frame(y=y,x=x)
df.test = data.frame(y=y.test,x=x.test)
fit.gam = gam(y ~ s(x), data=df, method="REML")

# 2.4
summary(fit.gam)
# The only parametric term we have is the intercept
#
# The GAM used ~8.6 degrees-of-freedom to estimate the smooth curve.
# This is equivalent to using 8-9 polynomials to build our curve
#
# The p-value for the smooth term s(x) is < 2e-16, i.e., it is tiny
# there is clearly a non-linear relationship between x and y (which we know to be true as we created it!)
#
# The 'scale est' is 0.009, but despite the name, this is actually the variance
# so the estimated standard deviation is sqrt(0.009) ~ 0.95 which 
# is quite a good estimate of the real sigma (of 0.1)

# 2.5
lines(x.test, predict(fit.gam, df.test), col="red")
# The estimated curve quite accurately models the true, unknown curve. The degrees-of-freedom (number
# of basis terms, complexity) seem to have been estimated quite well.

# 2.6
yp = predict(fit.gam,data.frame(x=c(-0.5,0,0.5)), se.fit=T)
yp

# To get an approximate 95% confidence interval from the standard errors, we can use the usual
# CI = (est - 1.96*se, est + 1.96*se) rule, i.e.
#
yp$fit - 1.96*yp$se.fit
yp$fit + 1.96*yp$se.fit

# 2.7
mean( (y.test - predict(fit.gam,df.test))^2)
#
# This is the average squared deviation between our estimated curve and the true underlying curve
# The score is quite small relative to the range of the y-values, indicating a good fit

# 2.8
y = my.f1(x) + rnorm(n, 0, 0.4)
plot(x,y)
lines(x.test,y.test)
df = data.frame(y=y,x=x)
fit.gam = gam(y ~ s(x), data=df, method="REML")

lines(x.test, predict(fit.gam, df.test), col="red")
mean( (y.test - predict(fit.gam,df.test))^2)

# The fit is somewhat worse, and the error is about 5 times larger than when SNR=1
# The increase in error is roughly proportional to the decrease in signal to noise ratio.

summary(fit.gam)
# The GAM has only used ~6 DF to fit the function now, instead of almost 8.7 before, because
# there is less signal in the data relative to the noise. In general, the greater the noise
# level relative to the signal level (i.e, the smaller the SNR) the less degrees-of-freedom
# will be used as there is less information to guide the fitting of more complex models.

# 2.9
motorcycle = read.csv("motorcycle.csv")
motorcycle.smooth = gam(f ~ s(t),data=motorcycle,method="REML")
summary(motorcycle.smooth)
#
# The GAM code has chosen to use ~8.6 degrees-of-freedom to smooth this curve. That is essentially equivalent
# to using 8-9 polynomials to smooth the data.
#
# We can estimate the signal to noise ratio by using the following approach:
#
# \hat{SNR}^2 = (var(y) - \hat{sigma2}) / \hat{sigma2}
#
# because var(y) = var(f) + sigma2
#
sqrt( (var(motorcycle$f) - motorcycle.smooth$sig2) / motorcycle.smooth$sig2 )
#
# Estimated SNR is around 3.6 -- so about 3.6 times as much signal as noise

# 2.10
plot(motorcycle.smooth, residuals=T)
# The variability is highest at the edges (i.e., near t=0 and t=60). This is because
# we have the least amount of data near the edges to estimate the relationship well
yp = predict(motorcycle.smooth, data.frame(t=30), se=T)
yp$fit
c(yp$fit - 1.96*yp$se.fit, yp$fit + 1.96*yp$se.fit)
#
# So our best guess of the force at time t=30 is ~ 29.2
# It could be anywhere from 17.32 to 41.26
#
# Check against the plot you will see at t=30 the force appears to be about 55, not 29.
# Why? This is because the functions for each predictor are chosen to have *zero mean*
# Therefore, to get a prediction from these functions we also need to add in the overall intercept
# which is -25.5. So 55 + (-25) ~ 30, which matches our prediction using predict(.), which 
# obviously includes the intercept.


# ----------------------------------------------------------------------------------
# Question 3: Additive Modelling
# ----------------------------------------------------------------------------------

# 3.1
diabetes.train = read.csv("diabetes.train.studio8.csv")
diabetes.test = read.csv("diabetes.test.studio8.csv")

# 3.2
fit.lin = gam(Y ~ AGE+SEX+BMI+BP+S1+S2+S3+S4+S5+S6, data=diabetes.train, method="REML")
summary(fit.lin)
fit.lin$aic
#
# AGE, S1, S2, S3, S4, S6 seem unlikely to be associated as their p-values are all quite large.
# Of course, they *could* be associated in a non-linear fashion

# 3.3
mean((diabetes.test$Y - predict(fit.lin,diabetes.test))^2)

# 3.4
fit.gam = gam(Y ~ s(AGE) + SEX + s(BMI) + s(BP) + s(S1) + s(S2) + s(S3) + s(S4)
              + s(S5) + s(S6), data=diabetes.train, method="REML")
summary(fit.gam)
# it appears the p-values for AGE and S6 have dropped, but overall
# the story is the same -- AGE, S1, S2, S3, S4, S6 are probably
# unassociated with Y

fit.gam$aic
# the AIC score has dropped by around 17 units, which is quite considerable
# this strongly suggests that there are some non-linear relationships in the data

# We don't model SEX as nonlinear because it is a factor variable with only 2 levels
# Only numeric variables benefit from this type of nonlinear modelling 

# 3.5
mean((diabetes.test$Y - predict(fit.gam,diabetes.test))^2)
# Prediction error for straight linear model was 1669; 
# prediction error for GAM is decreased

# 3.6
plot.gam(fit.gam,residuals=T)
# Most of the relationships seem mostly linear. 
# S5 seems the most nonlinear

# 3.7
summary(fit.gam)
# BP, S1, S2, S3, S4, have edf's esentially equal to 1 -- which is why
# the relationships in the plots looked basically linear.
# Even the variables that have some nonlinear relationship have edfs between 2 and 3
# which suggests they are not *too* nonlinear

# 3.8
fit.gam.new = gam(Y ~ s(AGE) + SEX + s(BMI) + s(BP) + s(S1) + s(S2) + s(S4)
                  + s(S5) + s(S6), data=diabetes.train, method="REML")
summary(fit.gam.new)
#
# The new model with S3 removed seems virtually identical to the full model;
# there are still a number of variables that could be pruned

# 3.9
# Remove S4 ..
fit.gam.new = gam(Y ~ s(AGE) + SEX + s(BMI) + s(BP) + s(S1) + s(S2) + s(S5) + s(S6), data=diabetes.train, method="REML")
summary(fit.gam.new)

# Remove AGE next ...
fit.gam.new = gam(Y ~ SEX + s(BMI) + s(BP) + s(S1) + s(S2) + s(S5) + s(S6), data=diabetes.train, method="REML")
summary(fit.gam.new)

# Finally remove S6
fit.gam.new = gam(Y ~ SEX + s(BMI) + s(BP) + s(S1) + s(S2) + s(S5), data=diabetes.train, method="REML")
summary(fit.gam.new)

# All remaining variables have p-values < 0.005, i.e., the Bonferroni multiple testing threshold
# The R^2 of the final pruned model is only a bit smaller than the full model (R^2 ~ 0.516)
# so it is likely that the variables we removed do not have a great deal of effect
# Two of the remaining variables seem to have non-linear relationships with Y: BMI and S5

# 3.10
mean((diabetes.test$Y - predict(fit.gam.new,diabetes.test))^2)
# The prediction error is now 1566, so substantially smaller than either the basic linear model
# or the full GAM using all variables

# 3.11
plot(fit.gam.new)
# The relationship between S5 and Y appears to be the most nonlinear

# 3.12
fit.final = gam(Y ~ SEX + s(BMI) + BP + S1 + S2 + s(S5), data=diabetes.train, method="REML")
summary(fit.final)
# The model is identical in fit, but now we have explicit coefficients 
# for BP, S1, S2 and SEX which makes interpretation easier.
plot(fit.final)
# only plots for BMI and S5 produced now ...

# We can set all the other predictors to arbitrary values, as the effects are additive
# i.e., no interactions between variables
predict(fit.final,data.frame(S5=4.5,SEX=0,BMI=0,BP=0,S1=0,S2=0)) - predict(fit.final,data.frame(S5=3.5,SEX=0,BMI=0,BP=0,S1=0,S2=0))
# ~ 44.3 increase in E[Y]
predict(fit.final,data.frame(S5=5.5,SEX=0,BMI=0,BP=0,S1=0,S2=0)) - predict(fit.final,data.frame(S5=3.5,SEX=0,BMI=0,BP=0,S1=0,S2=0))
# ~ 127.7 increase in E[Y]
# From S5=3.5 to S5=4.5 is 1 unit increase in S5
# From S5=3.5 to S5=5.5 is 2 units increase in S5
# If effects were close to linear we would expect change in E[Y] from S5=3.5 to S5=5.5 to be twice
# change in E[Y] from S5=3.5 to S5=4.5, but
44.3 * 2 
# much less than 127


# 3.13
fit.final.linonly = gam(Y ~ SEX + BMI + BP + S1 + S2 + S5, data=diabetes.train, method="REML")
summary(fit.final.linonly)
# The R^2 has dropped compared to our final GAM model
mean((diabetes.test$Y - predict(fit.final.linonly,diabetes.test))^2)
# Prediction error is worse than our pruned GAM, though
# it is better than our unpruned GAM or full linear model
# This is because the non-linear relationships are *not* that non-linear,
# so a linear model with unimportant variables removed will still outperform
# the non-linear model with too many unassociated variables included.


# 3.14
pima = read.csv("pima.csv")

fit.log = gam(DIAB ~ PREG+PLAS+BP+SKIN+INS+BMI+PED+AGE, family="binomial", data=pima, method="REML")
summary(fit.log)
fit.log$aic

fit.gam = gam(DIAB ~ s(PREG)+s(PLAS)+s(BP)+s(SKIN)+s(INS)+s(BMI)+s(PED)+s(AGE), family="binomial", data=pima, method="REML")
summary(fit.gam)
fit.gam$aic
# appears to be some non-linear relationships

plot(fit.gam)
# BMI, PED, AGE have some interesting looking curves
#
# Play around yourself ...