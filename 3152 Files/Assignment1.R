setwd("D:/Desktop/R files/3152 Files")
# Setting up data
rm(list = ls())
set.seed(32210213) # XXXXXXXX = your student ID
cvbase = read.csv("PsyCoronaBaselineExtract.csv")
cvbase <- cvbase[sample(nrow(cvbase), 40000), ] # 40000 rows

#Q 1.a and 1.b
# getting the dimension
dim(cvbase)

# checking how many columns are numerical attributes and how many are categorical
head(cvbase)


cvbase$employstatus <- NA
# combining employ_status attributes into one employstatus attribute
for (i in 1:nrow(cvbase)){
  for (j in 21:30){
    x = cvbase[i,j]
    if (!is.na(x)){
      cvbase[i,"employstatus"] = j-20
    }
  }
}
# creating a temporary table without employ_status
temp_drop = cvbase[-c(21:30)]
head(temp_drop)

# finding all the complete cases (no NAs)
comp = complete.cases(temp_drop)
comp

# filtering outr only leaving the completes
cvbase = cvbase[comp,]
dim(cvbase)

# analyzing numerical values
library(ggplot2)
library(tidyr)
# analyzing numerical values
par(mar=c(7,5,1,1))
par(mfrow = c(2,2))
boxplot(cvbase[1:20], las =2, ylab = "Value", main = "boxplot of first 20 variables") # the first 20 variables to stop before just employ_status
boxplot(cvbase[31:49], las =2, ylab = "Value", main = "boxplot of next 20 numerical variables") # the next 19 variables after employ_status but just before coded_country to skip it
par(mar=c(6,5,1,1))
boxplot(cvbase[51:54],las =2, ylab = "Value",main = "boxplot of last 4 numerical variables") # the rest of the numerical variables

# plotting the distribution of the employ_status by using our already created employstatus column
par(mfrow = c(2,2))
hist(cvbase$employstatus, xlim=c(1,10),breaks=40, main = "employstatus_ frequency", xlab = "employ_status indices")
# plotting coded_country
coded_ctry = data.frame(table(cvbase$coded_country))
barplot(coded_ctry$Freq, names.arg = coded_ctry$Var1,las = 2, main = "coded_country frequency")


#Q 2.a
serbia = cvbase[cvbase$coded_country == 'Republic of Serbia',]
cvbase = cvbase[!(cvbase$coded_country == 'Republic of Serbia'),]
dim(serbia)

par(mar=c(7,5,1,1))
par(mfrow = c(2,2))
boxplot(serbia[1:20], las =2, ylab = "Value", main = "serbia variables") 
boxplot(cvbase[1:20], las =2, ylab = "Value", main = "other countries variables") 
boxplot(serbia[31:49], las =2, ylab = "Value", main = "serbia variables") 
boxplot(cvbase[31:49], las =2, ylab = "Value", main = "other countries variables") 
par(mar=c(6,5,1,1))
boxplot(serbia[51:54],las =2, ylab = "Value",main = "serbia variables")
boxplot(cvbase[51:54],las =2, ylab = "Value",main = "other countries variables")
hist(serbia$employstatus, xlim=c(1,10),breaks=40, main = "employstatus_ frequency serbia", xlab = "employstatus_ indices")
hist(cvbase$employstatus, xlim=c(1,10),breaks=40, main = "employstatus_ frequency other countries", xlab = "employstatus_ indices")


#Q 2.b
#dropping the coded_country variable from serbia and previously created employstatus
serbia = serbia[-c(55)]
serbia = serbia[-c(50)]
#replacing NA values with 0 for employ_status
serbia[is.na(serbia)] = 0
# fitting the linear model for proso1-proso4
fit_proso1 = lm(serbia$c19ProSo01 ~ ., data = serbia)
summary(fit_proso1)
fit_proso2 = lm(serbia$c19ProSo02 ~ ., data = serbia)
summary(fit_proso2)
fit_proso3 = lm(serbia$c19ProSo03 ~ ., data = serbia)
summary(fit_proso3)$coefficients
fit_proso4 = lm(serbia$c19ProSo04 ~ ., data = serbia)
summary(fit_proso4)$coefficients

#Q 2.c
#dropping the coded_country variable
coded_country = cvbase[,50] # temporarily drop coded_country from cvbase
cvbase = cvbase[-c(55)]
cvbase = cvbase[-c(50)]
#replacing NA values with 0 for employ_status
cvbase[is.na(cvbase)] = 0
# fitting the linear model for proso1-proso4
fit_proso1_cv = lm(cvbase$c19ProSo01 ~ ., data = cvbase)
summary(fit_proso1_cv)$coefficients
fit_proso2_cv = lm(cvbase$c19ProSo02 ~ ., data = cvbase)
summary(fit_proso2_cv)$coefficients
fit_proso3_cv = lm(cvbase$c19ProSo03 ~ ., data = cvbase)
summary(fit_proso3_cv)$coefficients
fit_proso4_cv = lm(cvbase$c19ProSo04 ~ ., data = cvbase)
summary(fit_proso4_cv)$coefficients

#Q 3
#Q 3.a
# data taken from the world bank
# https://databank.worldbank.org/reports.aspx?source=2&series=NY.GDP.PCAP.CD&country=#
gdp = read.csv("gdp_per_capita_2020.csv") # extracting gdp per capita
life = read.csv("life_expec_2020.csv") # extracting life expectency
national_inc = read.csv("adj_net_national_2020.csv") # extracting national_income
# omitting uncontributing rows (Regions and World data)
gdp = gdp[-c(218:271),]
life = life[-c(218:271),]
national_inc = national_inc[-c(218:271),]

data = as.data.frame(gdp$Country.Name) # creating new data frame
colnames(data)[1] = "country_name"

data$gdp = as.numeric(gdp$X2020..YR2020.) 
data$life = as.numeric(life$X2020..YR2020.)
data$national_inc = as.numeric(national_inc$X2020..YR2020.)
data[data == ".."] = NA # changing the missing value from .. to NA
data = data[complete.cases(data),] # omitting NA values
data[,2:4] = scale(data[,2:4]) # scaling the dataframe
dim(data)0

# clustering
data_kfit = kmeans(data[,2:4], 10, nstart = 25)
t = as.data.frame.matrix(table(actual = data$country_name, fitted = data_kfit$cluster))
t

t["Serbia",] 
t[t[7]==1,]

#Visualizing the clustering result in a multivariate graph
data_kfit$cluster = as.factor(data_kfit$cluster)
attach(data)
ggplot(data, aes(life, gdp, size = national_inc ,color = data_kfit$cluster)) + geom_point()

#Q 3.b
#adding back coded_country to cvbase that was dropped at 2.c
cvbase$coded_country = coded_country
#getting the cluster group
cluster_group = cvbase[cvbase$coded_country == "Algeria" | cvbase$coded_country == "Brazil"
                       | cvbase$coded_country == "Peru" | cvbase$coded_country == "Romania"
                       | cvbase$coded_country == "Vietnam",]
dim(cluster_group)
cluster_group = cluster_group[,-c(55)] # removing coded_country from the cluster group
#replacing NA values with 0 for employ_status
cluster_group[is.na(cluster_group)] = 0
# fitting the linear model for proso1-proso4
fit_proso1_clust1 = lm(cluster_group$c19ProSo01 ~ ., data = cluster_group)
summary(fit_proso1_clust1)
fit_proso1_clust2 = lm(cluster_group$c19ProSo02 ~ ., data = cluster_group)
summary(fit_proso1_clust2)$coefficients
fit_proso1_clust3 = lm(cluster_group$c19ProSo03 ~ ., data = cluster_group)
summary(fit_proso1_clust3)$coefficients
fit_proso1_clust4 = lm(cluster_group$c19ProSo04 ~ ., data = cluster_group)
summary(fit_proso1_clust4)$coefficients







