rm(list = ls())
install.packages("tree")
library(tree)
install.packages("e1071")
library(e1071)
install.packages(("ROCR"))
library(ROCR)

#5
pconfidence = c(0.7,0.9,0.4,0.1,0.9,0.8,0.3,0.6,0.7,0.6)
plabels = c(0,1,0,0,1,1,1,1,1,0)
cpred = prediction(pconfidence, plabels)
cperf = performance(cpred,"tpr","fpr")
plot(cperf)
cpred@cutoffs
cauc = performance(cpred,"auc")
as.numeric(cauc@y.values)

#lift  = TP*(TP+FP)/(TP+FN)(TP+TN+FP+FN)
clift = performance(cpred, "lift")
plot(clift)

#6
zoo = read.csv("zoo.data.csv")
zoo[,2:ncol(zoo)] = lapply(zoo[2:ncol(zoo)],factor)
train.rows = sample(1:nrow(zoo), round(0.7*nrow(zoo)),replace = FALSE)
zoo.train = zoo[train.rows,]
zoo.test = zoo[-train.rows, ]
fit_tree2 = tree(type ~. - animal_name, data = zoo.train)
summary(fit_tree2)

zoo.predict =  predict(fit_tree2, zoo.test, type = "class")
t1 = table(actual = zoo.test$type, predicted = zoo.predict)

test.fit = cv.tree(fit_tree2, FUN=prune.misclass)
print(test.fit) # dev is misclasses nd size is for vars
prune.fit = prune.misclass(fit_tree2, best = 5)
print(summary(prune.fit))
plot(prune.fit)
text(prune.fit,pretty = 0)

zp.predict = predict(prune.fit, zoo.test, type = "class")
t2 = table(predicted = zp.predict, actual = zoo.test$type)
print(t2)
print(t1)
