####################################################################################
#	Script-file:   studio5_solns.R
#	Project:       FIT2086 - Studio 5
# Author:        Daniel Schmidt
#
# Purpose:  	   Solutions for questions in Studio 5
####################################################################################

# ----------------------------------------------------------------------------------
# Question 2
# ----------------------------------------------------------------------------------
getwd()

# Q2.4
bpdata = read.csv("bpdata.csv")

# Q2.5
t.test(x=bpdata$BP, mu=120)

# Q2.6
t.test(x=bpdata$BP, mu=120, alternative="less")

# 2.8
t.test(x=bpdata$BP, mu=120, conf.level=0.95)
t.test(x=bpdata$BP, mu=120, conf.level=0.99)
t.test(x=bpdata$BP, mu=120, conf.level=0.999)


# ----------------------------------------------------------------------------------
# Question 3
# ----------------------------------------------------------------------------------

# Q3.1
SP500 = read.csv("SP500.csv")

# Plot the data -- it is clear there is a big difference so we do expect small p-value
plot((SP500$Index), type="l", xlab="Week since 7th September, 2007", ylab="S&P Index", lwd=2.5)
lines(x=59:108,y=SP500$Index[59:108], col="red", lwd=2.5)

# Relevant statistics, calculated last week (see Studio 4 solution)
mu_pre  = 1381.703
mu_post = 886.916

sigma2_pre  = 9383.026
sigma2_post = 7002.371

n_pre  = 58
n_post = 50

# Approximate test for difference of means with unknown variances
diff = mu_pre - mu_post
se_diff = sqrt(sigma2_pre/n_pre + sigma2_post/n_post)

z = diff/se_diff
p = 2*pnorm(-abs(z))

# Q3.2
y_pre = SP500$Index[1:58]
y_post = SP500$Index[59:108]

# Q3.2.i
t.test(y_pre, y_post, var.equal = F)

rv = t.test(y_pre, y_post, var.equal=F)
rv$p.value
# t.test returns a lot of information you can get access to

t.test(y_pre, y_post, var.equal = T)$p.value


# ----------------------------------------------------------------------------------
# Question 4
# ----------------------------------------------------------------------------------

# Q4.1
mx = 4
nx = 12
theta_hat_x = mx/nx

# Testing against theta = 1/2 (fair coin)
theta_0 = 1/2

z = (theta_hat_x - theta_0) / sqrt(theta_0*(1-theta_0)/nx)
p = 2*pnorm(-abs(z))

p

# Q4.2
# the exact procedure is based on the binomial distribution.
binom.test(x = mx, n = nx, p = 1/2)


# Q4.3
binom.test(x = 3, n = nx, p = 1/2)

binom.test(x = 2, n = nx, p = 1/2)

# Q4.4
my = 10
ny = 12
theta_hat_y = my/ny
theta_p = (mx+my)/(nx+ny)

theta_hat_x - theta_hat_y

z = (theta_hat_x - theta_hat_y)/sqrt(theta_p*(1-theta_p)*(1/nx+1/ny))
p = 2*pnorm(-abs(z))
p

# Q4.5
prop.test(x = c(mx,my), n = c(nx,ny))