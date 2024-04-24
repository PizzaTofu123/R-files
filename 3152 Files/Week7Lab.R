#5
student = c('anne','bob','carl')
engS1 = c(50,NA,5)
engS2 = c(NA,52,30)
mathS1 = c(77,'-','-')
mathS2 = c(69,47,55)

Grades = data.frame(student,engS1,engS2,mathS1,mathS2)
Grades[Grades == '-'] = 0
class(Grades$engS1)

library(reshape2)
Grades = melt(Grades, id.vars = 'student', measure.vars = c('engS1','engS2','mathS1','mathS2'))
colnames(Grades) = c("student", 'subject', 'mark')

Grades

library(stringr)
Grades$semester = str_sub(Grades$subject,-2)
Grades$subject = str_sub(Grades$subject,1,-3)

Grades

#tidyverse method
library(tidyverse)
Grades %>% pivot_longer(!student, names_to = "subject", values_to = 'marks')

#7
setwd("D:/Desktop/R files/3152 Files")
data = read.csv("govhackelectricitytimeofusedataset.csv")
data = data[,c(1:3)]

colnames(data) = c("key","datetime","supply")
data$date = as.Date(data$datetime, format = "%d/%m/%Y")
data$datetime = NULL
data
data_1 = data[as.Date(data$date, "%Y-%m-%d") > as.Date("2012-12-31", "%Y-%m-%d"),]
data_1 = data[as.Date(data$date, "%Y-%m-%d") < as.Date("2013-02-01", "%Y-%m-%d"),]
data_1

#compute daily supply for each meter nd each day
attach(data_1)
day_cons = as.data.frame(as.table(by(supply, list(key,date), sum)))

data_day_count = as.data.frame(as.table(by(supply, list(key, date), length))) # if not nrow then dim

data_day = cbind(day_cons, data_day_count)
View(data_day)

#make data tidy
#remove dupes cols
data_day = data_day[,c(1,2,3,6)]
# rename
colnames(data_day) = c("Date", "ID", "Cons", "N")
# remove NA
data_day = data_day[complete.cases(data_day),]
# kee ponly 48 rows
data_day = data_day[data_day$N == 48,]
data_day

library(ggplot2)
ggplot(data_day, aes(data_day$ID, data_day$Cons)) + geom_boxplot()














