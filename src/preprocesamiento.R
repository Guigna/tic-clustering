

# Cargado de datos desde el directorio local... 
library(readr)
#OriginalData <- read_delim("C:/Users/Lenovoo/Dropbox/Alejandro Cataldo/git project/data/data.csv",";", escape_double = FALSE, trim_ws = TRUE) 
OriginalData <- read_csv("../data/data.csv")

#names(OriginalData)
#str(OriginalData)
#dim(OriginalData)
# 8084   26

# Creaci?n de nueva base de datos (binaryData) sin las columnas no-numericas.
binaryData <- OriginalData[,-c(1,2,3,4)] 

#remover instancias que contienen solo ceros
# Identificación y almacenamiento de las instancias que poseen ceros en todos los campos
indremoved = which(apply(binaryData, 1, function(x) all(x == 0)) )
paste("se removeran ",length(indremoved)," instancias que contienen solo ceros")
#no parece haber un patron en las encuestas con ceros

#Removimiendo instancias que poseen ceros en todos los campos de ambas bases de datos
binaryData = binaryData[ -indremoved, ]
OriginalData = OriginalData[ -indremoved, ]

#dim(binaryData)
#[1] 7861   22

barplot(sapply(binaryData, var), horiz=T, las=1, cex.names=0.8)

#No existen datos faltantes! :)

#Calculo de componentes principales
pc <- prcomp(binaryData)
plot(pc)
plot(pc, type='l')
summary(pc) 

outliers_1<-identify(qqnorm(pc$x[,1],pch = 20))

#Con el fin de representar al menos el 85% de la informaci?n áéíóú de los datos, se trabaja sobre la base de los 12 primeros componentes principales
comp <- data.frame(pc$x[,1:12])
plot(comp[,1:3], pch=16, col=rgb(0,0,0,0.5))


################## Apply k-means ##############

#install.packages("cluster")
library(cluster)
#install.packages("fpc")
library(fpc)

#KMEANS K = 3
k<-2
model <- kmeans(binaryData, k, nstart=25, iter.max=1000)     
#Calulo de silhouette Para cada cluster formado
dissE <- daisy(comp) 
dE2   <- dissE^2
sk2   <- silhouette(model$cl, dE2)
paste("k: ",k," silhouette: ",mean(sk2[,3]),"")

silhouette_kmeans<-c(0,0.372096819507349,0.293974180144305,0.274455229227329,0.249989801452009,0.246038468119303,0.252897798753004,0.244953331962789,0.247261680502461)
plot(silhouette_kmeans)


############## Apply hierarchical clustering, "Complete linkage" algorithm  ##########

install.packages("ape")
library(ape)
k<-2
h.complete <- hclust(dist(comp), method = "complete")
plot(as.phylo(h.complete), type = "fan")
plot(h.complete, main= "Complete linkage", xlab = "", sub = "")

dissE <- daisy(comp) 
dE2   <- dissE^2
sk2 <- silhouette(cutree(h.complete,k),dE2)
plot(sk2)
paste("k: ",k," silhouette: ",mean(sk2[,3]),"")



###################################################
###PAM
###################################################
#install.packages("ape")
library(ape)
library(cluster)

# install.packages("fpc")
library(fpc)

mydist <- as.dist(1-cor(t(binaryData), method="pearson")) # Generates distance matrix using Pearson correlation as distance method.
pamy <- pam(mydist, 4); 
#pamy$clustering # Same a above, but uses provided distance matrix.

dissE <- daisy(binaryData) 
dE2   <- dissE^2
sk2 <- silhouette(pamy$clustering,dE2)
plot(sk2, col = c("red", "green", "blue","black"))


####################################################
### DBSCAN
####################################################

dbscanResult <-  dbscan(comp,0.7,MinPts = 500)
dbscanResult
clusplot(comp, dbscanResult$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)
dissE <- daisy(comp) 
dE2   <- dissE^2
sk2   <- silhouette(dbscanResult$cl, dE2)

plot(sk2, col = c("red", "green", "blue","black"))


