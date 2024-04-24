#Q1.7
r = 2
n = 5

mu = seq(0.1,10)
a = 0
b = 0
bias = (a-b*mu)/(n+b)
var = (n*mu*(r+mu))/(r*(n+b)**2)
risk = r*(bias**2+var)/(mu*(r+mu))
plot(mu,risk,type="l", ylim = c(0.1,1), main = "plot of risk")

mu = seq(0.1,10)
a = 0
b = 1
bias = (a-b*mu)/(n+b)
var = (n*mu*(r+mu))/(r*(n+b)**2)
risk = r*(bias**2+var)/(mu*(r+mu))
lines(mu,risk,type="l", col= "red")
mu
risk

mu = seq(0.1,10)
a = 2
b = 1
bias = (a-b*mu)/(n+b)
var = (n*mu*(r+mu))/(r*(n+b)**2)
risk = r*(bias**2+var)/(mu*(r+mu))
lines(mu,risk,type="l", col= "green")

mu = seq(0.1,10)
a = 4
b = 1
bias = (a-b*mu)/(n+b)
var = (n*mu*(r+mu))/(r*(n+b)**2)
risk = r*(bias**2+var)/(mu*(r+mu))
lines(mu,risk,type="l", col= "blue")
legend(x="topright",legend=c("a=0, b=0", "a=0, b=1", "a=2, b=1", "a=4, b=1"), 
       fill = c("black","red","green","blue")
)

#Q2.1
setwd("D:/Desktop/R files/3154 Files")

source("nb.fit.R")
covid = read.csv("covid.2023.csv")
nb = nb.ml(covid$days)
#mu is 15.97 
nb$mu

# Calculating the 95% confidence interval
n = nrow(covid)
jn_mu = (n*nb$r)/(nb$mu*(nb$r+nb$mu))
start_95_conf = nb$mu - 1.96*sqrt(1/jn_mu)
end_95_conf = nb$mu + 1.96*sqrt(1/jn_mu)
start_95_conf
end_95_conf

#Q2.2
n = nrow(covid)
nb$r
jn_mu0 = (n*nb$r)/(14.46*(nb$r+14.46))
z = (nb$mu - 14.46 )/(sqrt(1/jn_mu0))
z
p_val = pnorm(-z)
2*p_val
# since the p value is so low, the null hypothesis which states that the average
# recovery time for Covid-19 in the Australian population as the same as the Israeli
# population is rejected.


#Q2.3
# 14 = a/b
# b = a/14
a = 5.95
pgamma(27, a, a/14) - pgamma(5, a, a/14)
b = a/14
b

#Q2.4
set.seed(32210213)
gen_sam = nb.sample(covid$days, a, a/14,100000)
x = seq(0,27)
plot(dgamma(x, a, a/14), main = "prior plot" ,xlab = "mu", ylab = "density")
lines(dgamma(x, a, a/14))
hist(gen_sam$mu.sample, probability = T, main = "post plot" , xlab= "mu")


#Q2.5
# post mean
mean(gen_sam$mu.sample)
# confidence interval of mu
quantile(p=c(0.025, 0.975), gen_sam$mu.sample)

#Q2.6
# yes because the mean of the australian population is 
# not in the 95% confidence interval of the observed israeli study

#Q2.7
qnbin = qnbinom(0.9, gen_sam$r.sample, mu = gen_sam$mu.sample)
qnbin
quantile(p=c(0.025,0.975), qnbin)

#Q2.8
post_mu = mean(gen_sam$mu.sample)
post_r = mean(gen_sam$r.sample)
plot(density(covid$days), col = "blue", main= "proportions of that many days-to-recovery", xlab = "Days")
lines(dnbinom(seq(min(covid$days),max(covid$days)), 
       post_r, mu= post_mu), col = "red")
legend(x="topright",legend=c("Population in data set", "Negative binomial model"), 
       fill = c("blue","red")
)

#Q3.1
setwd("D:/Desktop/R files/3154 Files")
concrete = read.csv("concrete.2023.csv")
fit.lm = lm(Strength ~ . , concrete)
summary(fit.lm)
# cement, blast.furnace.slag, fly.ash, age

#Q3.2
library(bayesreg)
fit.bayes = bayesreg(Strength ~. ,concrete , prior = "lasso", n.samples = 50000)
fit.bayes.sum = summary(fit.bayes)

# post mean comparison
fit.lm$coefficients
t(fit.bayes.sum$mu.coef)

# std comparison
summary(fit.lm)
t(fit.bayes.sum$se.coef)

#t-value and t-stats
summary(fit.lm)
t(fit.bayes.sum$t.stat)

#Q3.3
summary(fit.bayes)

#Q3.4
fit.bayes = bayesreg(Strength ~.-Coarse.Aggregate-Fine.Aggregate ,concrete , prior = "lasso", n.samples = 50000)
fit.bayes.sum = summary(fit.bayes)

#3.5
#a
prediction = 0.11025*480 + 0.08318*24 + 0.06538*110 + -0.20942*220 + 0.24987*4.2 + 
  0.10467*25 + 26.8932
prediction
#b
post_dist_pred = 480*fit.bayes$beta[1,] + 24*fit.bayes$beta[2,] + 110*fit.bayes$beta[3,] +
  220*fit.bayes$beta[4,] + 4.2*fit.bayes$beta[5,] + 25*fit.bayes$beta[6,] + 26.8932
hist(post_dist_pred, probability = T , xlab = "Strength", main = "Posterior distribution
     of the mean compressive strength")
quantile(p=c(0.025,0.975), post_dist_pred)


#3.6
concrete$age_cubed = (concrete$Age)**3
fit.bayes.2 = bayesreg(Strength ~.-Coarse.Aggregate-Fine.Aggregate ,concrete , prior = "lasso", n.samples = 50000)
fit.bayes.sum2 = summary(fit.bayes.2)
















