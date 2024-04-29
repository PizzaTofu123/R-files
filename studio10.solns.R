####################################################################################
#	Script-file:   studio10.solns.R
#	Project:       FIT2086 - Studio 10
#
# Purpose:  	   Solutions for questions in Studio 10
####################################################################################


# ----------------------------------------------------------------------------------
# Question 2
# ----------------------------------------------------------------------------------

rm(list=ls())
library(boot)

# 2.1
bpdata = read.csv("bpdata.csv")

# 2.2
t.test(x = bpdata$BSA, conf.level=0.95)

# 2.3
bs.mean = boot(bpdata$BSA, function(x,I) { return(mean(x[I])) }, 10000)
bs.mean

# 2.4
boot.ci(bs.mean, 0.95, type="basic")
boot.ci(bs.mean, 0.95, type="bca")

# 2.5
bs.med = boot(bpdata$BSA, function(x,I) { return(median(x[I])) }, 1000)

boot.ci(bs.med, 0.95, type="basic")
boot.ci(bs.med, 0.95, type="bca")

# 2.6
plot(bs.mean)
plot(bs.med)

# The complete set of different values the median of any bootstrap sample can take
# is equal to the unique values in the original sample. This is because the median
# finds the middle value in a sample and uses this as an estimate
# (technically it will average two values in the case of a tie, but the basic theory
# remains -- the number of different values is much smaller than the different
# means you can obtain by bootstrapping as the mean uses all the values)
#
# There is a spike at 1.98 as this is the median of the original sample, i.e.,
#
median(bpdata$BSA)
#
# is 1.98.
#
# The median  # is resistant to modifications of data because it uses the middle 
# value, i.e., median([1,2,6,8,9]) = 6, but so does median([1,1,6,8,9]), 
# median([2,2,6,8,9]), median([1,2,6,8,8]), median([2,2,6,8,8]) and so on. 
# As long as the middle value remains '6' the median will be six. This means that 
# when we are bootstrapping there is a high chance the median of the bootstrap 
# sample will remain the same as the original sample.

# ----------------------------------------------------------------------------------
# Question 3
# ----------------------------------------------------------------------------------

# 3.1
rm(list=ls())
pima.train = read.csv("pima.train.csv", stringsAsFactors = T)
source("my.prediction.stats.R")

# 3.2
fit = glm(DIABETES ~ ., pima.train, family=binomial)
my.pred.stats(predict(fit,pima.train,type="response"), pima.train$DIABETES)

rv = my.pred.stats(predict(fit,pima.train,type="response"), pima.train$DIABETES, display=F)
rv

# 3.3
boot.auc = function(formula, data, indices)
{
  # Create a bootstrapped version of our data
  d = data[indices,]
  
  # Fit a logistic regression to the bootstrapped data
  fit = glm(formula, d, family=binomial)
  
  # Compute the AUC and return it
  target = as.character(fit$terms[[2]])
  rv = my.pred.stats(predict(fit,d,type="response"), d[,target], display=F)
  return(rv$auc)
}

bs = boot(data=pima.train, statistic=boot.auc, R=1000, formula=DIABETES ~ .)
boot.ci(bs,conf=0.95,type="bca")
plot(bs)

# 3.4
pima.test = read.csv("pima.test.csv")
my.pred.stats(predict(fit,pima.test,type="response"), pima.test$DIABETES)


# ----------------------------------------------------------------------------------
# Question 4
# ----------------------------------------------------------------------------------

# 4.1
source("perm.log.reg.R")
rv = perm.log.reg(DIABETES ~ ., pima.train, R=1000)
rv$p.value

# 4.2
rv = perm.log.reg(DIABETES ~ ., pima.train, R=10000)
summary(fit)
rv$p.value

# The p-values are similar, though all of them appear to be smaller,
# suggesting the approximation used by glm() is a bit crude
# Some are an order of magnitude smaller (BP, PED) and some are a bit smaller
# (INS, AGE).
# The p-values exactly equal to zero occur because the number of permutations
# we have tried (10000) is not enough to be able to capture a non-zero p-value 
# smaller than 1/10000 (see Lecture 10). If we do M permutations, then the smallest
# non-zero p-value is 1/M. The "0" pvalues should be read as "< 1e-4". If we want more
# accurate estimation of this p-values we would need to run for more permutations.

# 4.3
hist(rv$perm.coef[,"INS"])
hist(rv$perm.coef[,"BP"])
