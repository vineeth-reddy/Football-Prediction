library(stats)
library(NbClust)
library(cluster)
library(mclust)
library(amap)
library(factoextra)
library(purrr)
#library(stylo)
library(philentropy)
library(SnowballC)
#library(caTools)
library(dplyr)
library(proxy)


setwd("/Users/vineethreddy/Documents/projects")
(df<-read.csv("clustering.csv"))

distMatrix_E <- dist(df, method="euclidean")
distMatrix_Mi <- dist(df, method="minkowski", p =3)
distMatrix_M <- dist(df, method="manhattan")


scaled_df<- scale(df)
distMatrix_c <- dist(scaled_df, method = "cosine")

groups_c <- hclust(as.dist(distMatrix_c),method="ward.D")
plot(groups_c, cex=0.9, hang=-1, main = "Cosine")
rect.hclust(groups_c, k=3)
