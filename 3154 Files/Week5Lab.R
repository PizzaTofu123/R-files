######################################################
# FIT3154 Studio 5 Solutions
######################################################

# ==================================================================================================================
# Question 2

# Q2.4
y = c(0, 1, 0, 1, 0, 1, 1, 0, 1, 1, 1, 0)
k = sum(y)
n = length(y)

# Q2.4.a
theta_ml = k/n

# Q2.4.b
se = sqrt(theta_ml*(1-theta_ml)/n)

# Q2.4.c
CI = c(-1.96,1.96)*se + theta_ml

# Q2.5 
# Q2.5.a
alpha = 1
beta = 1  # Uniform prior

theta = seq(0,1,length.out=1e4)
plot(theta, dbeta(theta, shape1=k+alpha, shape=(n-k)+beta), type="l", xlab="theta", ylab="p(theta | y)")

# Q2.5.b
theta_pm = (k+alpha)/(n+alpha+beta)
post_sd = sqrt( (k+alpha)*(n-k+beta)/(n+alpha+beta)^2/(n+alpha+beta+1) )

# Q2.5.c
qbeta(p=c(0.025,0.975),shape1=k+alpha,shape2=n-k+beta)

# Q2.6
alpha = 11
beta = 1

theta = seq(0,1,length.out=1e4)
lines(theta, dbeta(theta, shape1=k+alpha, shape=(n-k)+beta), col="red")

# Q2.5.b
theta_pm = (k+alpha)/(n+alpha+beta)
post_sd = sqrt( (k+alpha)*(n-k+beta)/(n+alpha+beta)^2/(n+alpha+beta+1) )

# Q2.5.c
qbeta(p=c(0.025,0.975),shape1=k+alpha,shape2=n-k+beta)


# ==================================================================================================================
# Question 3
source("studio5.normal.R")

# Q3.6
bpdata = read.csv("bpdata.csv")
rv = analyse.normal(bpdata$BP, 13.5)
rv

# Q3.7
rv.bayes = bayes.normal.normal(bpdata$BP, 13.5, 122, 2)
rv.bayes

# Q3.8
rv.bayes = bayes.normal.normal(bpdata$BP, 13.5, 122, 13.5)
rv.bayes

# Q3.10
# Q3.10a
# If 75% lie between (75,105), then because normal is symmetric we can choose to have m = (75+105)/2
# then just adjust s until we get the percentiles we want
# s = 13 approximately achieves this
qnorm(0.125, 90, 13)
qnorm(1-0.125, 90, 13)

# Q3.10b
rv.bayes = bayes.normal.normal(bpdata$Weight, 18, 90, 13)
mu = seq(75,110,by=0.01)
plot(mu, dnorm(mu, rv.bayes$post.mean, rv.bayes$post.sd), xlab="mu", ylab="p(mu|y)",type="l")

# Q3.10c
rv = analyse.normal(bpdata$Weight, 18)
rv
rv.bayes = bayes.normal.normal(bpdata$Weight, 18, 90, 13)
rv.bayes

