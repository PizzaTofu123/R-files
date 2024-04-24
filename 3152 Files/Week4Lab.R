library(ggplot2)

#3.a
ir = iris
ir$sepal_area = ir$Sepal.Length * ir$Sepal.Width * pi /4

#OR

area = function(length,width) length*width*pi/4

ir$petal_area = area(ir$Petal.Length,ir$Petal.Width)

#3.b
plot(ir$petal_area,ir$sepal_area,col = ir$Species, xlab= "petal_area", ylab="sepal_area")
legend("topright", legend=unique(ir$Species), col = unique(ir$Species), pch = 1)

#OR
ggplot(data = ir) +
  geom_point(aes(x = sepal_area, y = petal_area, col = Species)) + labs(title = "petal vs sepal area")

#3.c
max.speal = by(ir,ir$Species,function(df) df[which.max(df$sepal_area),])
max.speal
max.petal = by(ir,ir$Species,function(df) df[which.max(df$petal_area),])
max.petal
max.speal = do.call(rbind,max.speal)[c(6,7)]
max.petal = do.call(rbind,max.petal)[c(6,7)]
cbind(max.speal,max.petal)

#4
getwd()
setwd("D:/Desktop/R files/3152 Files")
body = read.csv("body.dat.csv")

#male = body[body$Gender == "Male",]
#female = body[body$Gender == "Female",]
aggregate(body[c(1:24)],body[25],mean)

means = by(body[,c(1:24)], body$Gender, function(df) sapply(df,mean))
means = as.data.frame(do.call(cbind,means))
plot(log10(means$Female), log10(means$Male),xlab = "Female (log10)", ylab = "Male (log10)")
abline(0,1)
text(log10(means$Female), log10(means$Male),row.names(means), cex = 0.5, pos = 3, col = "red")

#5
dunn = read.csv("Dunnhumby1-20.csv")
#exclude visit date
dunn = subset(dunn,selecy = c(customer_id,visit_Delta,visit_spend))
dunn = dunn[,c(1,3,4)]
head(dunn)
#average of visit _delta and visit spend for each customer id

dunn.avg = aggregate(dunn[,2:3], dunn[1],mean, na.rm = TRUE)
#OR
dunn.avg = aggregate(dunn[,2:3], list(dunn$customer_id),mean, na.rm = TRUE)

#change col name
colnames(dunn.avg) = c("customerID", "aveDelta", "aveSpend")
head(dunn.avg)

# getting the correlation
dunnCor = by(dunn, dunn[1], function(df) cor(df$visit_delta, df$visit_spend, use = "complete.obs"))
dunnCor = as.table(dunnCor)
dunnCor
dunnCor = as.data.frame(dunnCor)
dunnCor

#getting the observations
dunn_count = by(dunn, dunn[1], nrow)
dunn_count = as.data.frame(as.table(dunn_count))
colnames(dunn_count) = c("customerID", "N")

#Regression
lm(dunn$visit_spend ~ dunn$visit_delta)

dunn_int = by(dunn, list(dunn$customer_id), function(df) lm(df$visit_spend ~ dunn$visit_delta)$coefficients[1])


dunn_int = as.data.fram(as.table(dunn_int))
colnames(dunn_int) = c("customerID", "intercept")


dunn_slope = by(dunn, list(dunn$customer_id), function(df) lm(df$visit_spend ~ dunn$visit_delta)$coefficients[2])
dunn_slope = as.data.fram(as.table(dunn_int))
colnames(dunn_slope) = c("customerID", "slope")

dunn_comp = cbind(dunn.avg, dunnCor[2],dunn_count[2],dunn_int[2],dunn_slope[2])
head(dunn_comp)
