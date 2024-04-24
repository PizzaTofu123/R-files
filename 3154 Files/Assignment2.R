setwd("D:/Desktop/R files/3154 Files")
nb = read.csv("nb.shc.csv", stringsAsFactors = T)

#Q1.1
hist(nb$mu, probability=T)
#post mean
mean(nb$mu)
#95% confidence interval
quantile(p=c(0.025,0.975), nb$mu)

# post mean is very close to the gamma prior but slightly higher
# confidence interval has a larger range and is also slightly higher

#Q1.4
n = length(nb)
Y = sum(nb)
nb.map = function(y,s,r){
  n = length(y)
  Y = sum(y)
  c2 = 2*n*r+3
  c1 = s*(2*n*r+1) + r*(3-2*Y)
  c0 = s*r*(1-2*Y)
  r = polyroot(c(c0,c1,c2))
  return(r)
}

#Q1.5
covid = read.csv("covid.2023.csv", stringsAsFactors = T)
r = mean(nb$r)
map_estimator = nb.map(covid$days, 14,r)
map_estimator
# the estimated value is smaller because it is shifted
# towards our prior guess of 14 which is smaller

#Q1.6
map_estimator[1]
s = exp(seq(log(0.01),log(1e4),length.out=100))
map_estimator = nb.map(covid$days, s,r)
columns = c('s', 'estimator')
temp = data.frame(matrix(nrow = 0, ncol = length(columns)))
colnames(temp) = columns
for (i in 1:length(s)){
  temp[nrow(temp)+1,] = c(s[i],max(Re(nb.map(covid$days,s[i],r))))
}
length(s)
temp
plot(temp$s, temp$estimator,log = 'x', xlab = "Estimator", ylab= "s values")
abline(h = max(Re(nb.map(covid$days,14,r))), col= 'red')

#Q1.7 
nb.map.0 = function(y,r){
  n = length(y)
  Y = sum(y)
  return((2*Y*r-3*r)/(2*n*r+3))
}
nb.map.inf = function(y,r){
  n = length(y)
  Y = sum(y)
  return((2*Y*r-r)/(2*n*r+1))
}

#comparing
nb.map(covid$days,0,r)
nb.map.0(covid$days,r)
nb.map(covid$days,9999999999999999999,r)
nb.map.inf(covid$days,r)

# as s increases and goes to infinite, the estimator also increases
# until it plateus

#Q1.9
v = seq(0,50,length.out = 100)
s = 10
cauchy1 = sqrt(exp(v))/(sqrt(s)*pi*(1+(exp(v)/s)))
s = 100
cauchy2 = sqrt(exp(v))/(sqrt(s)*pi*(1+(exp(v)/s)))
s = 1000
cauchy3 = sqrt(exp(v))/(sqrt(s)*pi*(1+(exp(v)/s)))

plot(cauchy1, type = 'l', xlim= c(0,25), xlab=v)
lines(cauchy2, col= 'red')
lines(cauchy3, col = 'blue')
legend(x="topright",legend=c("s = 10", "s = 100", 's=1000'), 
       fill = c("black","red","blue")
)

#2.1a
library(mgcv)
daily.case =  read.csv("daily.cases.10.weeks.2023.csv", stringsAsFactors = T)
fit.gam = gam(Cases ~ s(Day) + Day.Of.Week, data=daily.case, method="REML", family=nb(link=log,theta=NULL))
fit.gam
summary(fit.gam)
# The GAM used 7.407 (around 7 to 8) degrees-of-freedom to estimate the smooth curve 
# in Days with a p-value of <2e-16 which is extremely small which means that it is
# very likely that it is non-linear
# talk about p value of day of week

#2.1b
# Saturday will decrease the number of cases slightly 
# Wednesday will increase the number of cases slightly

#2.2
fit.gam.identity = gam(Cases ~ s(Day) + Day.Of.Week, data=daily.case, method="REML", family=nb(link=identity,theta=NULL))
summary(fit.gam.identity)

# has a slightly higher edf for Days
# the effects (sgn) of being in each day is the same and also p-value ranking also the same
# Higher values for coefficients and error since no longer using log values

#2.3
plot(daily.case$Day, daily.case$Cases, ylab= "cases", xlab = "days", main= "identityGam identity performances")
lines(daily.case$Day, exp(predict(fit.gam, daily.case)), col="red")
lines(daily.case$Day, predict(fit.gam.identity, daily.case), col="blue")
legend(x="topleft",legend=c("link log Gam", "link identity Gam"), 
       fill = c("red","blue")
)

# the log-link catches the trend more and has an advantage over
# identity because it is better at capturing data that is overdispersed
# such as in our case a negative binomial problem

# MSE for log-link
mean((exp(predict(fit.gam, daily.case)) - daily.case$Cases)^2)

# MSE for identity
mean((predict(fit.gam.identity, daily.case) - daily.case$Cases)^2)

# The mse for log-link is lower



#2.6
func.2.6 =  function(y,X,beta0,beta,r){
  ret_list = list()
  n = length(y)
  ret_list$negative.log.likelihood = -sum(lgamma(y+r)) + sum(lgamma(y+1)) + n*lgamma(r)- n*r*log(r) - sum(y)*beta0 - sum(y*rowSums(X%*%beta)) + 
    sum((y+r)*log(r+exp(beta0+rowSums(X%*%beta))))
  
  ret_list$beta0 = -sum(y) + sum((y+r)*(exp(beta0+rowSums(X%*%beta))/(r+exp(beta0+rowSums(X%*%beta)))))
  
  ret_list$betaj = c()
  for (j in 1:length(beta)){
    ret_list$betaj= append(ret_list$betaj, (-sum(y*X[,j]) + sum(((y+r)*X[,j]*(exp(beta0+rowSums(X%*%beta))))/(r+exp(beta0+rowSums(X%*%beta))))))
  }
  
  return(ret_list)
}

#2.7
source("nbreg.gd.R")
nb.reg = read.csv("nbreg.test.csv", stringsAsFactors = T)
nb.reg
matrix = df2matrix(y ~ X.1+X.2+X.3,nb.reg)
nbreg.gd(matrix$X, matrix$y, r=2)

#2.8
daily.case =  read.csv("daily.cases.10.weeks.2023.csv", stringsAsFactors = T)
library(mgcv)
fit.gam = gam(Cases ~ s(Day) + Day.Of.Week, data=daily.case, method="REML", family=nb(link=log,theta=NULL))
r = fit.gam$family$getTheta(F) 
new_data_mat = df2matrix(Cases ~ I(Day) + I(Day^2) + I(Day^3) + I(Day^4) + I(Day^5) + I(Day^6) + I(Day^7) + I(Day^8) + I(Day^9) + I(Day^10) + Day.Of.Week, daily.case)
neg_bin_reg = nbreg.gd(new_data_mat$X, new_data_mat$y, r)
neg_bin_reg
new_data_mat$X

# Intercept and coefficients
# Beta0 8.914758
# Day -1.531248e-01
# Day^2 3.741269e-02
# Day^3 -4.477987e-03
# Day^4 3.094077e-04
# Day^5 -1.316930e-05
# Day^6 3.573469e-07
# Day^7 -6.185783e-09
# Day^8 6.589129e-11
# Day^9 -3.924316e-13
# Day^10 9.981338e-16
# Day.Of.WeekMonday 1.382623e-01
# Day.Of.WeekSaturday -1.134408e-01
# Day.Of.WeekSunday -7.187197e-02
# Day.Of.WeekThursday 1.431769e-01 
# Day.Of.WeekTuesday 2.173670e-01
# Day.Of.WeekWednesday 2.132349e-01

nb_reg_pred = exp(ans$beta0.hat + new_data_mat$X%*%ans$beta.hat)
plot(nb_reg_pred, xlab = "Days", ylab= "Cases", main = "NBreg model ML vs GAM", type = 'l')
lines(daily.case$Day, exp(predict(fit.gam, daily.case)), col="red")
legend(x="topright",legend=c("NBReg model ML", "GAM"), 
       fill = c("black","red")
)

# Very similar trend and prediction, slightly shifted to the right of the GAM

#2.9 the coefficients of the functions will most likely become smaller especially
# and the impact of each predictor will be smaller which depends on the shrinkage parameter.










