library(boot)

# 2.1
bpdata = read.csv("bpdata.csv")

# 2.2
t.test(x = bpdata$BSA, conf.level=0.95)

# 2.3
bs.mean = boot(bpdata$BSA, function(x,I) {return(mean(x[I])) }, 1000 )

# 2.4
boot.ci(bs.mean, 0.95, type="basic")
boot.ci(bs.mean, 0.95, type="bca")

# 2.5
median(bpdata$BSA)

bs.med = boot(bpdata$BSA, function(x,I) {
  print(x[I])
  return(median(x[I])) }, 1000)

# 2.6
plot(bs.mean)
plot(bs.med)

# 3.1
source("studio10.prediction.stats.R")

# 3.2
pima.train = read.csv("pima.train.csv", stringsAsFactors = T)
fit = glm(DIABETES ~ ., pima.train, family=binomial)
my.pred.stats(predict(fit,pima.train,type="response"), pima.train$DIABETES)
rv = my.pred.stats(predict(fit,pima.train,type="response"), pima.train$DIABETES)

# 3.3
boot.auc = function(formula, data, indices)
{
  # Create a bootstrapped version of our data
  d = data[indices,]
  # Fit a logistic regression to the bootstrapped data
  fit = glm(formula, d, family=binomial)
  # Compute the AUC and return it
  target = as.character(fit$terms[[2]])
  rv = my.pred.stats(predict(fit,d,type="response"), d[,target], display = F)
  return(rv$auc)
}
bs = boot(data=pima.train, statistic=boot.auc, R=1000, formula=DIABETES ~ .)
boot.ci(bs,conf=0.95,type="bca")
plot(bs)

# 3.4
pima.test = read.csv("pima.test.csv", stringsAsFactors = T)
my.pred.stats(predict(fit,pima.test,type="response"), pima.test$DIABETES)


# 4.1
source("perm.log.reg.R")
rv = perm.log.reg(DIABETES ~ ., pima.train, R=1000)
rv$p.value

# 4.2
rv = perm.log.reg(DIABETES ~ ., pima.train, R=10000)
summary(fit)

# 4.3
hist(rv$perm.coef[,"INS"])
hist(rv$perm.coef[,"BP"])

