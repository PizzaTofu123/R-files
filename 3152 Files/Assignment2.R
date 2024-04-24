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
#install.packages("neuralnet")


setwd("D:/Desktop/R files/3152 Files")
rm(list = ls())
WAUS <- read.csv("HumidPredict2023D.csv")
L <- as.data.frame(c(1:49))
set.seed(32210213) # Your Student ID is the random seed
L <- L[sample(nrow(L), 10, replace = FALSE),] # sample 10 locations
WAUS <- WAUS[(WAUS$Location %in% L),]
WAUS <- WAUS[sample(nrow(WAUS), 2000, replace = FALSE),] # sample 2000 rows

# Q1
#proportion of days where it is more humid tomorrow compared to it is lesser
more_humid = WAUS[WAUS$MHT == 1,]
dim(more_humid)

WAUS$WindDir3pm = as.factor(WAUS$WindDir3pm)
WAUS$WindDir9am = as.factor(WAUS$WindDir9am)
WAUS$RainToday = as.factor(WAUS$RainToday)
WAUS$WindGustDir = as.factor(WAUS$WindGustDir)
WAUS$MHT = as.factor(WAUS$MHT)

#Description of the predictors
for (i in 1:ncol(WAUS)){
  print(colnames(WAUS)[i])
  print(summary(WAUS[,i]))
}

p1 = ggplot(data.frame(WAUS), aes(x = WindGustDir)) + geom_bar()
p2 = ggplot(data.frame(WAUS), aes(x = WindDir9am)) + geom_bar()
p3 = ggplot(data.frame(WAUS), aes(x = WindDir3pm)) + geom_bar()
p4 = ggplot(data.frame(WAUS), aes(x = RainToday)) + geom_bar()
grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)

#Next we exclude Year and location
WAUS = WAUS[-c(1:2)]
WAUS

#Q2
#Replacing the NA values of 
#Evaporation, Sunshine, pressure9am, pressure3pm, Cloud9am, Cloud3pm
WAUS$Evaporation[is.na(WAUS$Evaporation)] = mean(WAUS$Evaporation, na.rm = TRUE)
WAUS$Sunshine[is.na(WAUS$Sunshine)] = mean(WAUS$Sunshine, na.rm = TRUE)
WAUS$Cloud9am[is.na(WAUS$Cloud9am)] = mean(WAUS$Cloud9am, na.rm = TRUE)
WAUS$Cloud3pm[is.na(WAUS$Cloud3pm)] = mean(WAUS$Cloud3pm, na.rm = TRUE)
WAUS$Pressure9am[is.na(WAUS$Pressure9am)] = mean(WAUS$Pressure9am, na.rm = TRUE)
WAUS$Pressure3pm[is.na(WAUS$Pressure3pm)] = mean(WAUS$Pressure3pm, na.rm = TRUE)


# removing the rest of the NA values
comp = complete.cases(WAUS)
WAUS = WAUS[comp,]
# checking what is left of the dataset
dim(WAUS)

#Q3
#Splitting the dataset to train and test data sets
set.seed(32210213) #Student ID as random seed
train.row = sample(1:nrow(WAUS), 0.7*nrow(WAUS))
WAUS.train = WAUS[train.row,]
WAUS.test = WAUS[-train.row,]
dim(WAUS.train)
dim(WAUS.test)

#Q4
#Decision tree
tree.fit=tree( MHT~ ., data=WAUS.train)
summary(tree.fit)
par(mar=c(5.1,4.1,10,2.1))
plot(tree.fit)
text(tree.fit, pretty = 0, srt = 0, cex = 0.75)

#Naive Bayes
naive.model=naiveBayes(MHT~., data = WAUS.train)

#Bagging
#Creating 12 trees
bag.model = bagging(MHT ~ ., data=WAUS.train, mfinal=12)

#Boosting
boost.model = boosting(MHT ~ ., data=WAUS.train, mfinal=12)

#Random Forest
rf.model = randomForest(MHT ~ ., data=WAUS.train)

#Q5
#Decision tree prediction
tree.predict = predict(tree.fit, WAUS.test, type = "class")
t = table(predicted = tree.predict, actual = WAUS.test$MHT)
t
tree.accuracy = (t[1,1]+t[2,2])/(t[1,1]+t[1,2]+t[2,1]+t[2,2])
tree.accuracy

#Naive Bayes prediction
nb.predict = predict(naive.model, WAUS.test)
t = table(predicted = nb.predict, actual = WAUS.test$MHT)
t
nb.accuracy = (t[1,1]+t[2,2])/(t[1,1]+t[1,2]+t[2,1]+t[2,2])
nb.accuracy
 
#Bagging prediction
bag.pred = predict.bagging(bag.model, newdata = WAUS.test)
t = bag.pred$confusion
t
bag.accuracy = (t[1,1]+t[2,2])/(t[1,1]+t[1,2]+t[2,1]+t[2,2])
bag.accuracy

#Boosting prediction
boost.pred = predict.boosting(boost.model, newdata = WAUS.test)
t = boost.pred$confusion
t
boost.accuracy = (t[1,1]+t[2,2])/(t[1,1]+t[1,2]+t[2,1]+t[2,2])
boost.accuracy

#Random Forest prediction
rf.pred = predict(rf.model, WAUS.test)
t = table(observed = WAUS.test$MHT, predicted = rf.pred)
t
rf.accuracy = (t[1,1]+t[2,2])/(t[1,1]+t[1,2]+t[2,1]+t[2,2])
rf.accuracy

#Q6
#Decision tree ROC
tree.predict = predict(tree.fit, WAUS.test, type = "vector")
pred <- prediction( tree.predict[,2], WAUS.test$MHT)
perf <- performance(pred,"tpr","fpr")
plot(perf,col = "blueviolet", main= "ROC plots")
abline(0,1)
tree.auc = performance(pred, "auc")
print(as.numeric(tree.auc@y.values))

#Naive Bayes ROC
nb.predict = predict(naive.model, WAUS.test, type = "raw")
pred <- prediction( nb.predict[,2], WAUS.test$MHT)
perf <- performance(pred,"tpr","fpr")
plot(perf, add=TRUE, col = "blue")
abline(0,1)
nb.auc = performance(pred, "auc")
print(as.numeric(nb.auc@y.values))

#Bagging ROC
bag.pred = predict.bagging(bag.model, newdata = WAUS.test, type = "vector")
pred <- prediction( bag.pred$votes[,2], WAUS.test$MHT)
perf <- performance(pred,"tpr","fpr")
plot(perf, add=TRUE, col = "red")
abline(0,1)
bag.auc = performance(pred, "auc")
print(as.numeric(bag.auc@y.values))

#Boosting ROC
boost.pred = predict.boosting(boost.model, newdata = WAUS.test, type = "vector")
pred <- prediction( boost.pred$votes[,2], WAUS.test$MHT)
perf <- performance(pred,"tpr","fpr")
plot(perf, add=TRUE, col = "darkblue")
abline(0,1)
boost.auc = performance(pred, "auc")
print(as.numeric(boost.auc@y.values))

#Random forest ROC
rf.pred = predict(rf.model, WAUS.test, type = "prob")
pred <- prediction( rf.pred[,2], WAUS.test$MHT)
perf <- performance(pred,"tpr","fpr")
plot(perf, add=TRUE, col = "green")
abline(0,1)
rf.auc = performance(pred, "auc")
print(as.numeric(rf.auc@y.values))

legend(0,1,legend=c("Tree", "NB","Bag","Boost","RF"),
       col=c("blueviolet","blue", "red","darkblue","green"), lty=1, cex = 0.65)

#Q7
class.name = c("Decision tree","Naive Bayesian", "Bagging", "Boosting", "Random Forest")
accuracy = c(tree.accuracy,nb.accuracy,bag.accuracy,boost.accuracy,rf.accuracy)
auc.val = c(as.numeric(tree.auc@y.values), as.numeric(nb.auc@y.values), as.numeric(bag.auc@y.values), as.numeric(boost.auc@y.values), as.numeric(rf.auc@y.values))
class.compare = data.frame(class.name, accuracy, auc.val)
class.compare

#8
summary(tree.fit)
bag.model$importance
boost.model$importance
rf.model$importance

#9
#training custom tree
custom.tree.fit=tree( MHT~ WindDir9am+WindGustDir+WindDir3pm+Temp9am+Rainfall, data=WAUS.train)
summary(custom.tree.fit)
par(mar=c(5.1,4.1,10,2.1))
plot(custom.tree.fit)
text(custom.tree.fit, pretty = 0, srt = 0, cex = 0.75)
custom.tree.fit

#Accuracy testing
custom.tree.predict = predict(custom.tree.fit, WAUS.test, type = "class")
t = table(predicted = custom.tree.predict, actual = WAUS.test$MHT)
t
tree.accuracy = (t[1,1]+t[2,2])/(t[1,1]+t[1,2]+t[2,1]+t[2,2])
tree.accuracy

#ROC plotting and AUC value
custom.tree.predict = predict(custom.tree.fit, WAUS.test, type = "vector")
pred <- prediction( custom.tree.predict[,2], WAUS.test$MHT)
perf <- performance(pred,"tpr","fpr")
plot(perf,col = "red", main = "Custom tree ROC plot")
abline(0,1)
custom.tree.auc = performance(pred, "auc")
print(as.numeric(custom.tree.auc@y.values))

#10
cv.fit = cv.tree(tree.fit,FUN = prune.misclass)
cv.fit
prune.tree.fit = prune.misclass(tree.fit, best = 5)
summary(prune.tree.fit)
prune.predict = predict(prune.tree.fit, WAUS.test, type = "class")
t = table(predicted = prune.predict, actual = WAUS.test$MHT)
t
prune.accuracy = (t[1,1]+t[2,2])/(t[1,1]+t[1,2]+t[2,1]+t[2,2])
prune.accuracy

prune.tree.predict = predict(prune.tree.fit, WAUS.test, type = "vector")
pred <- prediction( prune.tree.predict[,2], WAUS.test$MHT)
perf <- performance(pred,"tpr","fpr")
plot(perf,col = "red")
abline(0,1)
prune.tree.auc = performance(pred, "auc")
print(as.numeric(prune.tree.auc@y.values))


cust.boost.model = boosting(MHT ~ ., data=WAUS.train, mfinal=40)

cust.boost.pred = predict.boosting(cust.boost.model, newdata = WAUS.test)
t = cust.boost.pred$confusion
t
cust.boost.accuracy = (t[1,1]+t[2,2])/(t[1,1]+t[1,2]+t[2,1]+t[2,2])
cust.boost.accuracy

cust.boost.pred = predict.boosting(cust.boost.model, newdata = WAUS.test, type = "vector")
pred <- prediction( cust.boost.pred$votes[,2], WAUS.test$MHT)
perf <- performance(pred,"tpr","fpr")
plot(perf, col = "darkblue", main = "Better boosting model")
abline(0,1)
cust.boost.auc = performance(pred, "auc")
print(as.numeric(cust.boost.auc@y.values))



#11
#install.packages("neuralnet")
library(neuralnet)
library(car)
ann.WAUS.train = WAUS.train
ann.WAUS.test = WAUS.test

ann.WAUS.trainmm = as.data.frame(model.matrix(~WindDir3pm+WindDir9am+RainToday+WindGustDir, data = ann.WAUS.train))
ann.WAUS.testmm = as.data.frame(model.matrix(~WindDir3pm+WindDir9am+RainToday+WindGustDir, data = ann.WAUS.test))
ann.WAUS.train = cbind(ann.WAUS.train,ann.WAUS.trainmm)
ann.WAUS.test = cbind(ann.WAUS.test,ann.WAUS.testmm)
ann.WAUS.test

ann.WAUS.train = ann.WAUS.train[,c(1:5,7,10:17,19,20,22:(ncol(ann.WAUS.train)))]
ann.WAUS.test = ann.WAUS.test[,c(1:5,7,10:17,19,20,22:(ncol(ann.WAUS.test)))]

ann = neuralnet(MHT ~ ., data = ann.WAUS.train, hidden = 5)
ann.pred = compute(ann, ann.WAUS.test)
ann.pred = as.data.frame(round(ann.pred$net.result,0))
t = table(observed = ann.WAUS.test$MHT, predicted = ann.pred[,2])
t
accuracy =  t[2,1]/(t[1,1]+t[2,1]) 
accuracy


#Selecting a few variables
ann.WAUS.train = ann.WAUS.train[-c(1:2,4:12,14:15,47)]
ann.WAUS.test = ann.WAUS.test[-c(1:2,4:12,14:15,47)]

ann = neuralnet(MHT ~ ., data = ann.WAUS.train, hidden = 5)
ann.pred = compute(ann, ann.WAUS.test)
ann.pred
prob = ann.pred$net.result
pred = ifelse(prob>0.5, 1, 0)
prob = ann.pred$net.result
pred = ifelse(prob>0.5, 1, 0)
t = table(observed = ann.WAUS.test$MHT, predicted = pred[,1])
t
ann.accuracy = (t[1,1]+t[2,2])/(t[1,1]+t[1,2]+t[2,1]+t[2,2])
ann.accuracy

#12
svm.fit = svm(MHT ~ ., data = WAUS.train)
svm.fit.pred = predict(svm.fit, WAUS.test, type = "class") 
t = table(predicted = svm.fit.predict, actual = WAUS.test$MHT)
t
svm.fit.accuracy = (t[1,1]+t[2,2])/(t[1,1]+t[1,2]+t[2,1]+t[2,2])
svm.fit.accuracy

library(pROC)
svm.roc = roc(response = WAUS.test$MHT, predictor = as.numeric(svm.fit.pred))
plot(svm.roc, col = "green", main = "SVM ROC plot", print.auc=TRUE)










