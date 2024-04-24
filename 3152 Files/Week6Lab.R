setwd("D:/Desktop/R files/3152 Files")

#4
set.seed(9999)
zoo = read.csv("zoo.data.csv")
zkfit = kmeans(zoo[,2:17], 7, nstart = 20) 
zkfit
table(actual = zoo$type, fitted = zkfit$cluster)
zooN = zoo
zooN[,2:17] = scale(zooN[,2:17])
zkfit_N = kmeans(zooN[,2:17], 7, nstart = 20)
zkfit_N$cluster
table(actual = zoo$type, fitted = zkfit_N$cluster)
table(actual = zoo$type, fitted = zkfit$cluster)

?hclust
set.seed(9999)
zhfit = hclust(dist(zoo[,2:17]), 'average')

plot(zhfit, hang = -1)

cut.zhfit = cutree(zhfit, k=7)
cut.zhfit
rect.hclust(zhfit, k = 7, border = "red")
rect.hclust(zhfit, k = 3, border = "purple")
rect.hclust(zhfit, k = 4, border = "blue")
T3 = table(actual = zoo$type, fitted = cut.zhfit)
T3
31 + 13 + 20 + 4 + 8 + 0 + 0 /101

library(cluster)
i_sil_score = function(k){
  km = kmeans(zoo[,2:17], centers = k, nstart = 50)
  ss = silhouette(km$cluster, dist(zoo[,2:17]))
  mean(ss[,3])
}
k = 2:20
avg_sil = sapply(k,i_sil_score)
plot(k, avg_sil, type = 'b', xlab = 'Number of clusters', ylab = 'Average sil score')
df = data.frame(k,avg_sil)
df[]


