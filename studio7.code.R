####################################################################################
#	Script-file:   studio7_solns.R
#	Project:       FIT2086 - Studio 7
#
# Purpose:  	   Code for questions in Studio 7
####################################################################################

library(pROC)

# ===========================================================================================
#
#                                       Question 2
#
# ===========================================================================================

rm(list=ls())


# -------------------------------------------------------------------------------------------
# 2.1
gene.train = read.csv("gene.train.csv", header=T, stringsAsFactors=T)
levels(gene.train$Disease)
levels(gene.train$SNP2)


# -------------------------------------------------------------------------------------------
# 2.2
fullmod=glm(Disease ~ ., data=gene.train, family=binomial)
summary(fullmod)


fullmod$null.deviance
fullmod$deviance
fullmod$deviance + 2*length(fullmod$coefficients)


# -------------------------------------------------------------------------------------------
# 2.3
gene.test = read.csv("gene.test.csv", stringsAsFactors=T)
prob = predict(fullmod, gene.test, type="response")
pred = factor(predict(fullmod,gene.test)>1/2, c(F,T), c("N","Y"))

table(pred, gene.test$Disease)
# TN FN
# FP TP
# sensitivity = 2nd col, dspecificity = 1st col


# -------------------------------------------------------------------------------------------
# 2.4
mean(pred == gene.test$Disease)
roc.fullmod = roc(response=as.numeric(gene.test$Disease)-1, prob)
roc.fullmod$auc
plot(roc.fullmod)


# -------------------------------------------------------------------------------------------
# 2.5
source("my.prediction.stats.R")
my.pred.stats(prob, gene.test$Disease)


# -------------------------------------------------------------------------------------------
# 2.6
# Prune out variables using stepwise regression
step.fit.aic = step(fullmod,direction="both")
summary(step.fit.aic)
my.pred.stats(predict(step.fit.aic,gene.test,type="response"), gene.test$Disease)
# compare to 
my.pred.stats(prob, gene.test$Disease)

# -------------------------------------------------------------------------------------------
# 2.7
step.fit.bic = step(fullmod, k = log(nrow(gene.train)), direction="both", trace = 0)
my.pred.stats(predict(step.fit.bic,gene.test,type="response"), gene.test$Disease)
# compare to 2.6


# -------------------------------------------------------------------------------------------
# 2.8
summary(step.fit.bic)


# ===========================================================================================
#
#                                       Question 3
#
# ===========================================================================================

rm(list=ls())
library(pROC)
source("my.prediction.stats.R")


# -------------------------------------------------------------------------------------------
# 3.1 
# Load the data
pima.train = read.csv("pima.train.csv", stringsAsFactors=T)
fullmod = glm(DIABETES ~ . , data=pima.train, family=binomial)
summary(fullmod)


# -------------------------------------------------------------------------------------------
# 3.2 
pima.test = read.csv("pima.test.csv", stringsAsFactors=T)
my.pred.stats(predict(fullmod,pima.test,type="response"),pima.test$DIABETES)


# -------------------------------------------------------------------------------------------
# 3.3
# Prune
back.fit = step(fullmod, trace=0, k = log(668), direction="both")
summary(back.fit)
my.pred.stats(predict(back.fit,pima.test,type="response"),pima.test$DIABETES)

# -------------------------------------------------------------------------------------------
# 3.4
try.mod = glm(DIABETES ~ . + log(BMI), data = pima.train, family = binomial)
summary(fullmod)
summary(try.mod)


# -------------------------------------------------------------------------------------------
# 3.5
try.mod = glm(DIABETES ~ . + I(PLAS^2), data = pima.train, family = binomial)
summary(try.mod)


# -------------------------------------------------------------------------------------------
# 3.6
try.mod = glm(DIABETES ~ . + SKIN * AGE, data = pima.train, family = binomial)
summary(fullmod)
summary(try.mod)


# -------------------------------------------------------------------------------------------
# 3.7
# Fit a full model with all interactions, logs of all variables and squares of all variables
# Resulting model has 53 predictors.
fullmod = glm(DIABETES ~ . + .*. + log(PREG+1) + log(PLAS) + log(BP) + log(SKIN) + log(INS) + log(BMI) + log(PED) + log(AGE) 
              + I(PREG^2) + I(PLAS^2) + I(BP^2) + I(SKIN^2) + I(INS^2) + I(BMI^2) + I(PED^2) + I(AGE^2) , 
              data=pima.train, family=binomial)
my.pred.stats(predict(fullmod,pima.test,type="response"),pima.test$DIABETES)

forward.fit.bic = step(fullmod, k = log(668), trace=0, direction="both")
summary(forward.fit.bic)
my.pred.stats(predict(forward.fit.bic,pima.test,type="response"),pima.test$DIABETES)

forward.fit.bic$coefficients


# -------------------------------------------------------------------------------------------
# 3.8
forward.fit.kic = step(fullmod, k = 3, trace=0, direction="both")
summary(forward.fit.kic)
my.pred.stats(predict(forward.fit.kic,pima.test,type="response"),pima.test$DIABETES)

