install.packages("cluster")
install.packages("fpc")
require(xlsx)
library(ggplot2)
library(fpc)
library(cluster)


setwd("E:/klastering");
data=read.xlsx("datamiskin.xlsx", sheetName = "table")
data
data$LuasLahan
hasil<-kmeans(data[,2,3],3)
hasil
hasil$cluster
hasil$centers
hasil$size


table(hasil$cluster, data$Jumlah)
hasil$cluster <- as.factor(hasil$cluster)
ggplot(data, aes(data$Provinsi, data$Jumlah, color = hasil$cluster)) + geom_point()
dataSample <-data[,2,3]
hc <- hclust(dist(dataSample), method="ave")
hc
plot(hc, hang = -1, labels=data$Provinsi)

d <- dist(dataSample, method="euclidian")
pamCluster <- pam(d, 3)
clusplot(as.matrix(d), pamCluster$cluster, color=T, shade=T, labels=3, lines=0) 
pamCluster
