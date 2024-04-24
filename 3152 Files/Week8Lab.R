#6
data(mtcars)
attach(mtcars)
summary(mpg)
cons = ifelse(mpg >= 19.20, "yes", "no")
carsclass = cbind(cons, mtcars)
head(carsclass)

# train nd test
set.seed(9999)
train.rows = sample(nrow(carsclass),round(nrow(carsclass)*0.7), replace = FALSE)
c.train = carsclass[train.rows,]
c.test = carsclass[-train.rows,]

#fitting a decision tree momento
install.packages("tree")
library(tree)
cl.train = c.train[sample(nrow(c.train),100 , replace = TRUE),]
cl.train$cons = factor(cl.train$cons)
fit_tree = tree(cons~. -mpg, data = cl.train)
summary(fit_tree)

#predicting
c.test$cons = factor(c.test$cons)
c.predict = predict(fit_tree, c.test,type = "class")
c.predict
c.test
table(actual = c.test$cons, predicted = c.predict)

#plot the decision tree
plot(fit_tree)
text(fit_tree)

#7
zoo = read.csv("zoo.data.csv")
zoo
zoo[,2:ncol(zoo)] = lapply(zoo[2:ncol(zoo)],factor)
train.rows = sample(1:nrow(zoo), round(0.7*nrow(zoo)),replace = FALSE)
zoo.train = zoo[train.rows,]
zoo.test = zoo[-train.rows, ]
fit_tree2 = tree(type ~. - animal_name, data = zoo.train)
summary(fit_tree2)

zoo.predict =  predict(fit_tree2, zoo.test, type = "class")
table(actual = zoo.test$type, predicted = zoo.predict)
plot(fit_tree2)
text(fit_tree2, pretty = 0)
