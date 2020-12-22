library(readxl)
library(factoextra)
library(clustertend)
library(ggplot2)
library(NbClust)
library(cluster)
library(mclust)
paises <- read_excel("C:/Users/franc/DataSets/paises.xlsx", 
                     col_types = c("text", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric","numeric"))

paises<-data.frame(paises, row.names =paises$Country)
paises<-paises[,-c(1)]
paises
# PCA
paises.PC<-princomp(~. ,data=paises, cor=TRUE, scores=TRUE)
plot(paises.PC$scores[,1:2])
#Data Scale
paises.sc<-scale(paises)
hopkins(paises.sc, n=nrow(paises.sc)-1)
#K-Medias Country
km.res<-kmeans(paises.sc, 4, nstart=10)
plot(paises.PC$scores[,1:2], col=km.res$cluster)

fviz_cluster(km.res, data = paises.sc,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
             ellipse.type = "euclid", # Concentration ellipse
             # star.plot = TRUE, # Add segments from centroids to items
             #repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)
# Maybe looks great with 3-4 clusters
#Codo
fviz_nbclust(paises.sc, kmeans, method = "wss") +
  labs(subtitle = "Elbow method")
#Shilouette
fviz_nbclust(paises.sc, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")
# 2
fviz_nbclust(paises.sc, kmeans, nstart = 5, method = "gap_stat", nboot = 500)+
  labs(subtitle = "Gap statistic method")
# 7



# K medoides
pam.res <- pam(paises.sc, 7)
print(pam.res)
fviz_nbclust(paises.sc, pam, method = "silhouette")+
  theme_classic()
# okey 7
fviz_cluster(pam.res, # color palette
             ellipse.type = "t", # Concentration ellipse
             #repel = TRUE, # Avoid label overplotting (slow)
             show.clust.cent=TRUE,
             ggtheme = theme_classic()
)
# doesn't look like a great  cluster

#library mclust
mc.res <- Mclust(paises.sc) 
print(mc.res)
fviz_mclust(mc.res, "BIC", palette = "jco") # 4 Best cluster

#utility of this script ---->  MOB.R


