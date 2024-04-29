source('my.prediction.stats.R')
source("wrappers.R")
library(glmnet)
library(rpart)
library(randomForest)
library(kknn)

diabetes.train = read.csv("diabetes.train.csv")
diabetes.test = read.csv("diabetes.test.csv")
summary(diabetes.train)

#2.2
tree.diabetes = rpart(Y ~ ., diabetes.train)

#2.3
tree.diabetes

#2.4
plot(tree.diabetes)
text(tree.diabetes, digits=3)

#2.5
tree.diabetes$variable.importance # is because it is the root, redices the variance/ neg log likelihood the most
max(tree.diabetes$variable.importance)

#2.6
sqrt(mean((predict(tree.diabetes, diabetes.test) - diabetes.test$Y)**2))

#2.7
cv = learn.tree.cv(Y ~.,data=diabetes.train,nfolds=10,m=1000)
plot.tree.cv(cv)

sqrt(mean((predict(tree.diabetes, diabetes.test) - diabetes.test$Y)**2))

#2.9
lasso.fit = cv.glmnet.f(Y ~ ., data=diabetes.train)
glmnet.tidy.coef(lasso.fit)
sqrt(mean((predict.glmnet.f(lasso.fit,diabetes.test)-diabetes.test$Y)**2))

#3.1
rf.diabetes = randomForest(Y ~ ., data=diabetes.train)

#3.2
rf.diabetes

#3.3
sqrt(mean((predict(rf.diabetes, diabetes.test) - diabetes.test$Y)**2))


#3.4
rf.diabetes = randomForest(Y ~ ., data=diabetes.train, importance=TRUE, ntree=5000)

#3.5
round( importance( rf.diabetes ), 2)

#4
ytest.hat = fitted( kknn(Y ~ ., diabetes.train, diabetes.test) )
sqrt(mean((ytest.hat - diabetes.test$Y)**2))
kernels = c("rectangular","triangular","epanechnikov","gaussian","rank","optimal")
knn = train.kknn(Y ~ ., data = diabetes.train, kmax=25, kernel=kernels)
ytest.hat = fitted( kknn(Y ~ ., diabetes.train, diabetes.test,
                         kernel = knn$best.parameters$kernel, k = knn$best.parameters$k) )


