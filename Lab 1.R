#########################################################
# R script: studio1.R
# Project: Studio 1
#
# Date: 28/07/2022
# Author: William 
#
# Purpose: Introductory script for Studio 1
#########################################################

print(pi**2)
log10(1/2*sqrt(1+cos(pi/2)))
ls()
y <- 5
rm(y)
rm(list=ls()) # removes evrythin
help(ls)
apropos("med")
getwd()
setwd("D:/Desktop/R files")
heart <- read.csv("heart.csv", header = TRUE) #header	a logical value indicating whether the file contains the names of the variables as its first line. 
                                        # If missing, the value is determined from the file format: header is set to TRUE if and only if the first row 
                                        # contains one fewer field than the number of columns.
head(heart,10) # prints the first 10
View(heart) # graphical table
names(heart) # variable names
str(heart) # same like top but more info

heart$AGE # accesses only age
heart[,1]
heart[,"AGE"]

# first parameter is basically 9the condition
heart[,c("AGE","HD")] # the c is for combine to show both
heart[,c(1,14)]

heart[10:15,] # for 10 tro 15 dudes
heart[c(10,11,15),] # for 10 11 and 15 dudes

heart[c(3,88), c("SEX","CP")] # for 3 and 88 their sex and cp

heart[heart$SEX == 0, "AGE"] # all their sex is 0
heart[heart$CHOL < 150, c("SEX","CP")] # chol is less than 150

heart[heart$SEX == 0 & heart$CHOL < 150, ] # this is for and condition


heart$AC <- heart$AGE / heart$CHOL # makes new shit for the table
heart$AC <- NULL # removes the mofoking column

mean(heart$AGE) # sample mean
median(heart$AGE) # sample median
quantile(heart$AGE) # sample quantiles
min(heart$AGE) # minimum value
max(heart$AGE) # maximum value
range(heart$AGE) # sample range
var(heart$AGE) # sample variance
sd(heart$AGE) # sample standard deviation

summary(heart$AGE) # shows the min, 1st quartile, median, mean, 3rd quartile and the Max
summary(heart) # same but for all attributes

table(heart$SEX) # makes a table for this attribute with 0 and 1s
table(heart$SEX, heart$HD) # makes a  table for both this attributes

heart$SEX <- factor(heart$SEX, labels=c("MALE","FEMALE"), levels=c(0,1)) # basically makes all 0s to male and 1s to female

myMult <- function(a, b) # makes func bruv
{
  return(a*b)
}
print(myMult(10, 3))

retval =list() # declares a list
retval$a = 1
retval$b = "you can store strings too"
retval$c = list()
retval$c$g = "You can store lists inside lists"
retval$c$a = "You"

length(retval) # checks the length
length(retval$c)

data("ToothGrowth") # built in data set

# central tendencies is mean mode and median
# disperencies is variance, standard deviation, and interquartile range
summary(ToothGrowth)

for (num in 0:15){
  if ((num %% 3) == 0 && num != 0){
    cat(num)
  }
}

factorial <- function(n){
  if (n <= 1){
    return(1)
  }
  else {
    return(n*factorial(n-1))
  }
  
}


#make boxplot and histogram for len attribute
boxplot( len ~ dose, data = ToothGrowth, col = "black") # len will be on y while dose will be on X
boxplot(ToothGrowth$len)
len <- ToothGrowth$len
hist(ToothGrowth$len)

hist(Temperature,
     main="Maximum daily temperature at La Guardia Airport",
     xlab="Temperature in degrees Fahrenheit",
     xlim=c(50,100),
     col="darkmagenta",
     freq=FALSE
)


# Calculate the 5th, 25th, 50th ,75th and 95th quantiles of the len variable using the quantile()
quantile(ToothGrowth$len, probs = c(5,25,50,75,95)/100)

# plot and correlation coefficient
plot(ToothGrowth$len, ToothGrowth$dose)
cor(ToothGrowth$len, ToothGrowth$dose)

# appropriate names
plot(ToothGrowth$len, ToothGrowth$dose, xlab = "len", ylab = "dose")

mush = read.csv("mushroom.csv", header=T, stringsAsFactors=T)
tab = table(mush$cap.shape)
pie(tab)

table(mush$class, mush$cap.shape)
