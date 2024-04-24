setwd("D:/Desktop/R files/3152 Files")
# Setting up data

library(tree)
library(e1071)
library(ROCR)
library(rpart)
library(ggplot2)
#install.packages("gridExtra")
library("gridExtra") # used to arrange ggplots
#install.packages("adabag")
library(adabag)
#install.packages("randomForest")
library(randomForest)

#3
jc = read.csv("JapaneseCredit.csv", stringsAsFactors = TRUE)
set.seed(9999)
train.row = sample(1:nrow(jc), 0.7*nrow(jc))
jc.train = jc[train.row,]
jc.test = jc[-train.row,]

jc.tree = tree(Class~., data = jc.train)
plot(jc.tree)
text(jc.tree)

jc.pred = predict(jc.tree, jc.test, type = "class")
table(Predicted = jc.pred, Actual = jc.test$Class)

jc.pred.tree = predict(jc.tree, jc.test, type = 'vector')
jcd.pred = prediction(jc.pred)
jc.tree
summary(jc.tree)

#5
library(neuralnet)
library(car)
library(dplyr)

jcc = read.csv("JapaneseCredit.csv")
jcc = jcc[complete.cases(jcc),]
jcc$Class = recode(jcc$Class, '+' = '0', '-' = '1')
jcc$Class = as.numeric(jcc$Class)

train.row = sample(1:nrow(jcc), 0.8*nrow(jcc))
jcc.train = jcc[train.row,]
jcc.test = jcc[-train.row,]

#using only numeric values
jcc.nn = neuralnet(Class ~ ., jcc.train, hidden = 3, linear.output = FALSE)
jcc.nn = neuralnet(Class ~ A2+A3+A8+A11+A14+A15, jcc.train, hidden = 3, linear.output = FALSE)

jcc.pred = predict(jcc.nn,jcc.test[c(2,3,8,11,14,15)])
pred = ifelse(jcc.pred > 0.5,1,0)

t = table(observerd = jcc.test$Class, predicted = pred)
t

rm(list = ls())
#install.packages("neuralnet")
library(neuralnet)
options(digits=4)
pttrainread <- read.csv("playtennistrainTF.csv")
pttestread <- read.csv("playtennistestTF.csv")
ptcombined = rbind(pttrainread,pttestread)
ptcombined
ptmm = model.matrix(~Outlook+Temperature+Humidity+Wind, data=ptcombined)
ptcombined = cbind(ptcombined,ptmm)
ptcombined
ptcombined = ptcombined[,c(1, 8, 9, 10, 11, 12, 13, 6)]
pttest = ptcombined[15:20,]
pttrain = ptcombined[1:14,]
set.seed(9999) #random seed
#resampling with replacement to create a larger training set
pttrain = pttrain[sample(nrow(pttrain), 100, replace = TRUE),]
pttrain = as.data.frame(pttrain)
PT.nn = neuralnet(Play~ OutlookRain + OutlookSunny + TemperatureHot +TemperatureMild+ HumidityNormal + WindWeak, pttrain, hidden=1)
PT.nn$result.matrix
PT.pred = compute(PT.nn, pttest[,2:7])
PT.predr = round(PT.pred$net.result,0)
table(observed = pttest$Play, predicted = PT.predr)


















