rm(list = ls())
setwd("D:/Desktop/R files/3152 Files")
Dec21PedCount = read.csv("Ped_Count_December_2021.csv", header = TRUE)
Dec21PedCount = lapply(Dec21PedCount[3:83], as.numeric)
Dec21PedCount = as.data.frame(Dec21PedCount)
cols = ncol(Dec21PedCount)



# rows : 744, cols : 81
# cols
# rows 
# 

#5
pacific = c(209, 48, 169, 138, 64, 97, 161, 95, 145, 90, 121, 80, 56, 64, 209, 64, 72, 288, 322)
tasman = c(76, 64, 68, 64, 37, 32, 32, 51, 56, 40, 64, 56, 80, 121, 177, 56, 80, 35, 72, 72, 108, 48)
boxplot(tasman,pacific)
t.test(pacific,tasman, conf.level = 0.99) # p value chance no difference from observed data

#6
meta = c(45,51,39,41,48,49,46,43,47)
stature = c(171,178,157,163,172,183,173,175,173)
plot(stature~meta)
fitted = lm(formula = stature ~ meta)
abline(fitted)
summary(fitted)

#7
kiama = read.delim("kiama.txt")
mean(kiama$Interval)
sd(kiama$Interval)
hist(kiama$Interval)
hist(kiama$Interval, 
     breaks = 50,
     xlim = c(0,200),
     col = "darkmagenta")


#8
timber = read.table("http://www.statsci.org/data/oz/timber.txt", header = TRUE)
cor(timber$Elast,timber$Rigid)  
cor(timber$Dens,timber$Rigid) 


w <-c(9, 16, "monkey")
class(w)
