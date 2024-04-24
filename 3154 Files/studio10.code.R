####################################################################################
#	Script-file:   studio9.code.R
#	Project:       FIT3154 - Studio 9
# Author:        Daniel Schmidt
#
# Purpose:  	   Code for questions in Studio 9 -- Intro to "Big Data"
####################################################################################

rm(list=ls())

setwd("D:/Desktop/R files/3154 Files")

source("my.density.plot.R")
source("my.prediction.stats.R")
source("my.simple.test.R")
source("wrappers.R")
library(ggplot2)
library(glmnet)

# ----------------------------------------------------------------------------------
# Question 2: Microarray Analysis
# ----------------------------------------------------------------------------------

# 2.1
leukemia = read.csv("leukemia.csv", stringsAsFactors = T)
table(leukemia$Type)

my.density.plot(leukemia$Gene.1, bins=10)
my.density.plot(leukemia$Gene.300, bins=10)
my.density.plot(leukemia$Gene.1051, bins=10)
my.density.plot(leukemia$Gene.2602, bins=10)
my.density.plot(leukemia$Gene.3000, bins=10)

# 2.2
rv = my.simple.test("Type", leukemia, "binomial")

# 2.3
hist(rv$z, probability=T, ylim=c(0,0.4))
x=seq(-5,5,by=0.01)
lines(x, dnorm(x,0,1), col="red")

# 2.4
plot(-log10(rv$p.value),xlab="Gene",ylab="-log10(p.value)")

# 2.5
p = length(rv$z)
0.05/p
lines(c(1,p), c(-log10(0.05/p),-log10(0.05/p)), col="red")
lines(c(1,p), c(-log10(0.05),-log10(0.05)), col="green", lw=2)

# 2.6
sqrt(2*log(p))

p.universal = 2*pnorm(-sqrt(2*log(p)))
p.universal

lines(c(1,p), c(-log10(p.universal),-log10(p.universal)), col="blue", lw=2)


# 2.7
sum(rv$p.value <= 0.05)
sum(rv$p.value <= p.universal)
sum(rv$p.value <= 0.05/p)

# 2.8
leukemia.test = read.csv("leukemia.test.csv", stringsAsFactors = T)
rv.test = my.simple.test("Type",leukemia.test,family="binomial")

# 2.9
sum(rv.test$p.value[rv$p.value<=0.05] <= 0.05) # "True" discoveries, real
sum(rv.test$p.value[rv$p.value<=0.05] > 0.05) # "False" discoveries, false positives

# Universal threshold
sum(rv.test$p.value[rv$p.value<=p.universal] <= 0.05) # "True" discoveries 
sum(rv.test$p.value[rv$p.value<=p.universal] > 0.05)  # "False" discoveries

# Bonferroni
sum(rv.test$p.value[rv$p.value<=(0.05/p)] <= 0.05) # "True" discoveries 
sum(rv.test$p.value[rv$p.value<=(0.05/p)] > 0.05)  # "False" discoveries

# 2.10
plot(rv$z, rv.test$z, xlab="Z-score (training)", ylab="Z-score (validation)")
cor(rv$z,rv.test$z)





# ----------------------------------------------------------------------------------
# Question 3: Prediction with the Microarray Data
# ----------------------------------------------------------------------------------

# 3.1
f = my.simple.make.formula("Type",names(leukemia)[which(rv$p.value<=(0.05/p))+1])
fit.glm = glm(f, data=leukemia, family="binomial")
summary(fit.glm)

# 3.2
my.pred.stats(predict(fit.glm, leukemia.test, type="response"), leukemia.test$Type)

# 3.3
fit.bon = cv.glmnet.f(f,data=leukemia,family="binomial",alpha=0,nfolds=45)
my.pred.stats(predict.glmnet.f(fit.bon, leukemia.test, type="response"),
              leukemia.test$Type)
#penalized reg is way better and more useful if p large

# 3.4
f = my.simple.make.formula("Type",names(leukemia)[which(rv$p.value<=p.universal)+1])
fit.uni = cv.glmnet.f(f,data=leukemia,family="binomial",alpha=0,nfolds=45)
my.pred.stats(predict.glmnet.f(fit.uni, leukemia.test, type="response"), leukemia.test$Type)
# little diff but more test so see the price?

# 3.5
lasso.fit=cv.glmnet.f(f,data=leukemia,family="binomial", nfolds=45, alpha=1)
glmnet.tidy.coef(lasso.fit)
my.pred.stats(predict.glmnet.f(lasso.fit,leukemia.test,type="response"),
              leukemia.test$Type)

# 3.6
lasso.fit=cv.glmnet.f(Type~.,data=leukemia,family="binomial", nfolds=45, alpha=1)
glmnet.tidy.coef(lasso.fit)
my.pred.stats(predict.glmnet.f(lasso.fit,leukemia.test,type="response"),leukemia.test$Type)


# 3.7
library(randomForest)
rf = randomForest(Type~.,data=leukemia)
my.pred.stats(predict(rf, leukemia.test, type="prob")[,2], leukemia.test$Type)
