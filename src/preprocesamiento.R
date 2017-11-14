

# Cargado de datos desde el directorio local... 
library(readr)
#OriginalData <- read_delim("C:/Users/Lenovoo/Dropbox/Alejandro Cataldo/git project/data/data.csv",";", escape_double = FALSE, trim_ws = TRUE) 
OriginalData <- read_csv("./data/data.csv")
OriginalData <- data
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

# No es necesario escalar los datos, debido a que son binarios

# EVALUANDO LA EXISTENCIA DE CLUSTER:

# La funcion "get_clust_tendency" utiliza el estadistico de Hopskin 
# para evaluar la tendencia a formar cluster que los datos poseen.
# Este estadistico posee un rango de 0 a 1, donde valor cercano a 1 permite concluir 
# una alta probabilidad de existencia de cluster. En general, un valor sobre 0.5
# indica que existe tendencia a formar clusters.

# install.packages(c("factoextra", "clustertend"))
# install.packages("devtools")
# require("devtools")
# devtools::install_github("kassambara/factoextra")

library("factoextra")
res <- get_clust_tendency(binaryData, n= 40 , graph = TRUE)
res$hopkins_stat
#[1] 0.6503583, en este caso, el valor es mayor que 0.5 por lo tanto los datos poseen 
# tendencia a formar clusters.

# Otra forma de evaluar la tendencia a formar cluster, es utilizar el metodo visual,
# sin embargo, necesita muchos recursos computacionales cuando la base de datos estudiada
# es relativamente grande.
print(res$plot) 

#DETERMINANDO EL NUMERO OPTIMO DE CLUSTERS:
# se han implementado los siguiente metodos:
# - Elbow
# - Silhouette method
# - Gap statistic

install.packages("NbClust")
library(factoextra)
library(NbClust)

fviz_nbclust(binaryData, kmeans, method = "wss") + geom_vline(xintercept = 4, linetype = 2) +
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(binaryData, kmeans, method = "silhouette") + labs(subtitle = "Silhouette method")

# Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(binaryData, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+ 
  labs(subtitle = "Gap statistic method")

# Gap statistic:
library("cluster")
set.seed(123)
# Compute the gap statistic
gap_stat <- clusGap(binaryData, FUN = kmeans, nstart = 25, 
                    K.max = 10, B = 500) 
# Plot the result
library(factoextra)
fviz_gap_stat(gap_stat)


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

gap_stat <- clusGap(comp, FUN = kmeans, nstart = 25, 
                    K.max = 10, B = 50) 


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


