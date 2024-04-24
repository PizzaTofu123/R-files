x <- 1:10
y <- x^2 + 10 
plot(x,y)
plot(x, y, main="A Simple Plot", xlab="X-value", ylab="Y-value")

#1
celc <- c(0:100)
far <- celc *9/5 + 32
plot(far, celc, main = "Fahrenheit and celcius plot", xlab = "Celcius", ylab = "Fahrenheit")

x <- c(1,4)
z <- runif(1000,50,100) # 1000 random values between 50 and 100
z

#2
z <- c(runif(500,10,20), runif(500,20,50))
z

mean(z)
max(z)
min(z)
sd(z)

# sampling 1000 from rnorm
x <- rnorm(1000)
y <- 3*x + 10 + rnorm(1000)

myData <- data.frame(x,y,z)
head(myData)

plot(myData)
plot(y~x,data=myData)

#3


fit <- lm(y~x,data=myData)
summary(fit)
abline(fit,col='red')

#4


getwd()
# use this to decompress ozone <- read.table(gzfile("hourly_44201_2014‐06.csv.gz"),header=TRUE,sep=",")
ozone <- read.table("hourly_44201_2014-06.csv",header=TRUE,sep=",")
head(ozone)

#5a
tail(ozone)

#5b
nrow(ozone)

str(ozone)
head(ozone[,c(6:7, 10)]) # select columns 6 7 and 10
ozone$Latitude[1:10]
unique(ozone$State.Name)

#6 
# 53 states

#7
summary(ozone$Sample.Measurement)

quantile(ozone$Sample.Measurement, seq(0, 1, 0.1))

boxplot(ozone$Sample.Measurement, ylab="Ozone level (ppm)")
boxplot(ozone$Sample.Measurement~ozone$State.Name,ylab="Ozone level (ppm)")

par(las = 2, mar = c(10, 4, 2, 2), cex.axis = 0.8)

boxplot(Sample.Measurement ~ State.Name,ozone,range=0,ylab="Ozone level (ppm)")

ozone$region <- factor(ifelse(ozone$Longitude > -100, "west", "east"))
str(ozone)

#8
head(ozone)
library(maps)
map("state")
abline(v = -100, lwd = 3)
text(-120, 30, "West")
text(-75, 30, "East")

library(dplyr)
grp <- group_by(ozone,region)
summarise(grp,mean=mean(Sample.Measurement),median=median(Sample.Measurement))

fltr <- filter(ozone, State.Name != "Country of Mexico")
grp <- group_by(fltr, region)
summarise(grp, mean=mean(Sample.Measurement),median=median(Sample.Measurement))


data(iris)
iris

install.packages("ggplot2")
library(ggplot2)
ggplot(data = airquality, mapping = aes(x = Ozone, y = Temp)) +
geom_point() + geom_smooth(method = lm)

ggplot(data = airquality, mapping = aes(x = Ozone, y = Temp)) +
geom_point() + geom_smooth(method = loess)

