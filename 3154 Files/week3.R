setwd("D:/Desktop/R files/3154 Files")
fibers = read.csv("fibers.csv")
n = 100
sigma = sqrt(1/n * sum(fibers - 20)**2)
sigma
