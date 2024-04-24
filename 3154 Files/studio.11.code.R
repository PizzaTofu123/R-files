####################################################################################
#	Script-file:   studio11.solns.R
#	Project:       FIT3154 - Studio 11
# Author:        Daniel Schmidt
#
# Purpose:  	   Code for questions in Studio 11 -- Neural Networks
####################################################################################

rm(list=ls())

install.packages("nnet")

setwd("/Users/daniel/Dropbox/Teaching 2021/FIT3154 S2 2021/Studios/Studio 11/Downloaded")

# ----------------------------------------------------------------------------------
# Question 1: Simple function smoothing
# ----------------------------------------------------------------------------------
library("nnet")

set.seed(3154)

my.f1 <- function(x) {1/(1+x^2) + x*cos(5*x) }
n <- 100
x.train <- sort(runif(n,-1,1))
y.train <- my.f1(x.train) + rnorm(n, 0, 0.1)
plot(x.train,y.train)

# Q1.2
model.nn <- nnet(x=x.train, y=y.train, size=16, linout=T, maxit=5000)

# Q1.3
# 16x2 + 16 + 1
# weight is 1 for each param but bias is 1 for each neuron
# each neuron has 2 params weight nd bias, output is 16 weight + 1 bias


# Q1.4
x.test <- seq(-1,1, by=0.001)
y.test <- my.f1(x.test)
lines(x.test, y.test, col="red")

y.predict <- predict(model.nn, newdata=as.data.frame(x.test))
lines(x.test, y.predict, col="blue")

#MSE = 
mean((y.test - y.predict)^2)

# Q1.5 
y.train.noisy <- my.f1(x.train) + rnorm(n, 0, 0.4)
plot(x.train,y.train.noisy)
lines(x.test, y.test, col="red")

# Q1.6
model.nn.noisy <- nnet(x=x.train, y=y.train.noisy, size=16, linout=T, maxit=5000)

y.noisy.predict <- predict(model.nn.noisy, newdata=as.data.frame(x.test))
lines(x.test, y.noisy.predict, col="blue")#

#MSE = 
mean((y.test - y.noisy.predict)^2)

# Q1.7
plot(x.train,y.train.noisy)
lines(x.test, y.test, col="red")

# a) Size = 3
model.nn.noisy <- nnet(x=x.train, y=y.train.noisy, size=3, linout=T, maxit=5000)
y.noisy.predict <- predict(model.nn.noisy, newdata=as.data.frame(x.test))
lines(x.test, y.noisy.predict, col="blue")

#MSE = 
mean((y.test - y.noisy.predict)^2)

# b) Size = 2
model.nn.noisy <- nnet(x=x.train, y=y.train.noisy, size=2, linout=T, maxit=5000)
y.noisy.predict <- predict(model.nn.noisy, newdata=as.data.frame(x.test))
lines(x.test, y.noisy.predict, col="green")

#MSE = 
mean((y.test - y.noisy.predict)^2)

# Q1.8
plot(x.train,y.train.noisy)
lines(x.test, y.test, col="red")

# Decay = 0.1
model.nn.noisy <- nnet(x=x.train, y=y.train.noisy, size=16, linout=T, maxit=5000, decay = 0.1)
y.noisy.predict <- predict(model.nn.noisy, newdata=as.data.frame(x.test))
lines(x.test, y.noisy.predict, col="blue")

#MSE = 
mean((y.test - y.noisy.predict)^2)

# Decay = 0.01
model.nn.noisy <- nnet(x=x.train, y=y.train.noisy, size=16, linout=T, maxit=5000, decay = 0.01)
y.noisy.predict <- predict(model.nn.noisy, newdata=as.data.frame(x.test))
lines(x.test, y.noisy.predict, col="green")

#MSE = 
mean((y.test - y.noisy.predict)^2)


# Q1.9
n.large <- 1000
x.train.large <- sort(runif(n.large,-1,1))
y.train.large <- my.f1(x.train.large) + rnorm(n.large, 0, 0.4)
plot(x.train.large, y.train.large)
lines(x.test, y.test, col="red")

model.nn.large <- nnet(x=x.train.large, y=y.train.large, size=16, linout=T, maxit=5000)

y.predict.large <- predict(model.nn.large, newdata=as.data.frame(x.test))
lines(x.test, y.predict.large, col="blue")

#MSE = 
mean((y.test - y.predict.large)^2)

# ----------------------------------------------------------------------------------
# Question 2: The Digit Data
# ----------------------------------------------------------------------------------

rm(list=ls())

install.packages("dplyr")

source("my.density.plot.R")
source("wrappers.R")
source("display.digit.R")
library(ggplot2)
library(glmnet)
library(dplyr)
library(reshape2)
setwd("D:/Desktop/R files/3154 Files")

# 2.1
zip.train = read.csv("zip.train.csv", stringsAsFactors = T)
zip.test  = read.csv("zip.test.csv", stringsAsFactors = T)
table(zip.train$Digit)
# 2.2
my.density.plot(zip.train[,30])
my.density.plot(zip.train[,60])
my.density.plot(zip.train[,156])
my.density.plot(zip.train[,210])

# 2.3
X = matrix(as.numeric(zip.train[1,2:257]),nrow=16,ncol=16)
X

# 2.4
display.digit(zip.train[1,])
zip.train$Digit[1]


display.digit(zip.train[12,]) 
zip.train$Digit[12]

display.digit(zip.train[1612,]) 
zip.train$Digit[1612]


display.digit(zip.train[1122,]) 
zip.train$Digit[1122]

# 2.5
lasso.fit=cv.glmnet.f(Digit ~ ., data=zip.train, family="multinomial", nfolds=10, alpha=1)
# takes a while ...

table(predict.glmnet.f(lasso.fit,zip.test,type="class"),zip.test$Digit)
sum(diag(table(predict.glmnet.f(lasso.fit,zip.test,type="class"),zip.test$Digit)))/7291


# 2.6
fit.nn = nnet(Digit ~ ., data=zip.train, size=5, rang=0.1, decay=0, maxit=5e2, skip = T, MaxNWts=1e6)

table(zip.train$Digit, predict(fit.nn, zip.train, type="class"))

sum(diag(table(zip.test$Digit, predict(fit.nn, zip.test, type="class"))))/7291
table(zip.test$Digit, predict(fit.nn, zip.test, type="class"))


# 2.7
set.seed(3154)
fit.nn = nnet(Digit ~ ., data=zip.train, size=20, rang=0.1, decay=1.5e-4, maxit=5e2, skip = T, MaxNWts=1e6)

table(zip.train$Digit, predict(fit.nn, zip.train, type="class"))

sum(diag(table(zip.test$Digit, predict(fit.nn, zip.test, type="class"))))/7291
table(zip.test$Digit, predict(fit.nn, zip.test, type="class"))

#256x20+20+20+1

# ----------------------------------------------------------------------------------
# Question 3: Keras (optional)
# ----------------------------------------------------------------------------------

rm(list=ls())

install.packages(c("keras","curl"))

library(keras)
install_keras() # you will get warnings (even an error?) about curl but everything seems to work nonetheless

# Q2.1
mnist <- dataset_mnist()
x_train <- mnist$train$x
y_train <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y

# reshape
dim(x_train) <- c(nrow(x_train), 784)
dim(x_test) <- c(nrow(x_test), 784)
# rescale
x_train <- x_train / 255
x_test <- x_test / 255

y_train <- to_categorical(y_train, 10)
y_test <- to_categorical(y_test, 10)

# Q2.2
model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 256, activation = "relu", input_shape = c(784)) %>% 
  layer_dense(units = 128, activation = "relu") %>%
  layer_dense(units = 10, activation = "softmax")

summary(model)

# Q2.3
model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_sgd(),
  metrics = "accuracy"
)

model %>% fit(
  x_train, y_train, 
  epochs = 100, batch_size = 128
)


# Q2.4
model %>% evaluate(x_test, y_test,verbose = 1)

y.pred <- model %>% predict_classes(x_test)
y_test_class <- mnist$test$y
table(y.pred, y_test_class)

# Total accuracy is 97.72%
sum(diag(table(y.pred, y_test_class)))/10000