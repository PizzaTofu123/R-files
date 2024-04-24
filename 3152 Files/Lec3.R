library(ggplot2)
rm(list=ls())
getwd()
setwd("D:/Desktop/R files/3152 files")


qplot(Sepal.Length, data = iris, geom = "histogram",facets = Species ~ .) + facet_wrap(~ Species, ncol = 3)
m = ggplot(iris, aes(x = Sepal.Length)) #data
m = m + geom_histogram(binwidth = 0.1) #graph type
m = m + facet_wrap(~Species, ncol = 3) #grouping var
m
ggsave("irissepallen1.jpg", m, width = 20, height = 8, units = "cm")
 
g = ggplot(data = mpg)
g = g + geom_point(mapping = aes(x = displ, y = hwy,
                                   color = class))
g 

d <- ggplot(mpg, aes(displ, hwy, color = class)) +
  geom_point()
d = d + stat_summary(mapping = aes(x = displ, y =
                                       hwy), fun.min = min, fun.max = max, fun = median,
                       orientation = "x", colour = "black")
d = d + geom_point(mapping = aes(x = displ, y = hwy,
                                   color = class)) # overplots original points
d
ggsave("hwyvdispl.jpg", d, width = 20, height = 12,
         units = "cm")

#sizes of text
d = d + theme(axis.text = element_text(size = 8))
d = d + theme(axis.title = element_text(size = 10))

#labels
# could by axis.text.x or .y etc. to adjust separately
d = d + xlab("Engine Displacement (litres)")
d = d + ylab("Highway Fuel Consumption (mpg)")

#limits
d = d + ylim(10,50) + xlim(1,7)

#doing title
d = d + theme(plot.title = element_text(size = 14))
d = d + theme(plot.title = element_text(hjust = 0.5))
d = d + ggtitle("Highway … and Class")

#legend
d = d + theme(legend.position = c(0.91, 0.76),
                legend.key.height= unit(0.5, 'cm'))
d

#This function converts “coerces” the output of a
#table into a data frame
Sepal.cor <- as.data.frame(as.table(by(iris, iris[5],
                                         function(df) cor(df[1], df[2]))))
Sepal.cor

#This function assigns new column names to a data
#frame
colnames(Sepal.cor) <- c("Species", "Sepal.cor")
Sepal.cor

Petal.cor <- as.data.frame(as.table(by(iris, iris[5],
                                         function(df) cor(df[3], df[4]))))
colnames(Petal.cor) <- c("Species", "Petal.cor")
Petal.cor

iris.cor <- merge(Sepal.cor, Petal.cor, by = "Species")
iris.cor[,2] = round(iris.cor[,2], digits = 3)
iris.cor[,3] = round(iris.cor[,3], digits = 3)
write.csv(iris.cor, file = "Iris.cor.csv",
            row.names=FALSE)
