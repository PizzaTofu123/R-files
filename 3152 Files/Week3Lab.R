library(ggplot2)
?mpg

#3.a corr, linear reg, scatterplot
cor(mpg$cty, mpg$hwy)
plot(mpg$cty, mpg$hwy, xlab = "city", ylab = "highway", main = "City to Highway relationship")
fitted = lm(formula = mpg$hwy ~ mpg$cty)
abline(fitted)

#3.b
qplot(data = mpg,cty, geom="histogram", facets = manufacturer ~.)
boxplot(formula = cty ~ manufacturer, data = mpg, ylab="city fuel consumption", las = 3) # las to change orientation
boxplot(formula = hwy ~ manufacturer, data = mpg, ylab="city fuel consumption", las = 3) 
ggplot(data = mpg , mapping = aes(x = cty, y = manufacturer)) + geom_boxplot() + ggtitle (label = "City vs type of manufacturer")

#3.c
plot(x = mpg$displ, y = mpg$cty, col = mpg$cyl)
legend("topright", legend = unique(mpg$cyl), title = "Cylinders", col = unique(mpg$cyl), pch = 1)

#3.d
table(mpg$manufacturer, mpg$trans)
plot(mpg[c("displ","year","cyl","cty","hwy")])

#3.e
attach(mpg)
a = as.data.frame(mpg[year == 1999, "hwy"])
b = as.data.frame(mpg[year == 2008, "hwy"])

t.test(a$hwy, b$hwy, "two.sided") # no change

#4
attach(dsmall)
set.seed(9999) # Random seed to make subset reproducible
dsmall <- diamonds[sample(nrow(diamonds), 1000), ] # sample of 1000 rows
qplot(x = carat, y = price, size = clarity, col = color, alpha = cut)

#5
detach()
body = read.csv("body.dat.csv")
View(body)
summary(body)
head(body)
cor(body[1:24])

#5.a
by(body,body[,25],function(d) cor(d[,1:23],d[24]))
colnames(body)

#5.b
body = body[, c(1:22,24,23,25)]
by(body,body[,25],function(d) cor(d[,1:23],d[24]))

#5.c
result = by(body,body[,25],function(d) cor(d[1:24]))

library(reshape2)
female = melt(result$Female)
g = ggplot(data = female, aes (x=Var1, y = Var2, fill = value))
g = g + geom_tile(color = "white") + scale_fill_gradient(low = "green",high = "red")
g


