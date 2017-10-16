

# Cargado de datos desde el directorio local... 
library(readr)
OriginalData <- read_delim("C:/Users/Lenovoo/Dropbox/Alejandro Cataldo/git project/data/data.csv",";", escape_double = FALSE, trim_ws = TRUE) 

# Creación de nueva base de datos (binaryData) sin las columnas no-numericas.
binaryData <- OriginalData[,-c(1,2,3,4)] 

# Identificación y almacenamiento de las instancias que poseen ceros en todos los campos
indremoved = which(apply(binaryData, 1, function(x) all(x == 0)) )

#Removimientolas de instancias que poseen ceros en todos los campos de ambas bases de datos
binaryData = binaryData[ -indremoved, ]
OriginalData = OriginalData[ -indremoved, ]

#No existen datos faltantes!

#Calculo de componentes principales
pc <- prcomp(binaryData)
plot(pc)
plot(pc, type='l')
summary(pc) 

#Con el fin de representar al menos el 85% de la información de los datos, se trabaja sobre la base de los 12 primeros componentes principales
comp <- data.frame(pc$x[,1:12])
plot(comp, pch=16, col=rgb(0,0,0,0.5))


# Apply k-means with k=3

install.packages("cluster")
library(cluster)
install.packages("fpc")
library(fpc)

#KMEANS K = 3
k <- kmeans(comp, 3, nstart=25, iter.max=1000)     

#Calulo de silhouette Para cada cluster formado
dissE <- daisy(comp) 
dE2   <- dissE^2
sk2   <- silhouette(k$cl, dE2)
plot(sk2, col = c("red", "green", "blue","black"))

plotcluster(comp, k$cluster)
clusplot(comp, k$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)







