#install.packages("factoextra")
library(factoextra)
library(ggplot2)
library(readxl)
library(gridExtra)

#Load Raw Data
setwd("..")
setwd("Clustering/")
ListadoPromedios <- read_excel("ListadoPromedios.xlsx")
Pensum11001 <- read_excel("Pensum11001.xls")
Pensum13001 <- read_excel("Pensum13001.xls")
Pensum18001 <- read_excel("Pensum18001.xls")


# Listado de primedios unicamente con valores numericos
ListadoPromediosNumeric <- read_excel("ListadoPromedios.xlsx", 
                               col_types = c("numeric", "numeric", "skip", 
                                             "skip", "skip", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric"))
View(ListadoPromediosNumeric)


#Promedio_Simple_Acumulado VS cursos_acumulados

mainDataframe <-  read_excel("ListadoPromedios.xlsx", 
                             col_types = c("skip", "skip", "skip", 
                                           "skip", "skip", "skip", "skip", "skip", 
                                           "numeric", "skip", "numeric"))

rawdata<-ggplot(mainDataframe, aes(x = Promedio_Simple_Acumulado, y = cursos_acumulados)) + geom_point()
grid.arrange(rawdata)

df<- scale(mainDataframe)
head(df)

set.seed(123)
cluster3 <- kmeans(df, 3, nstart = 25)
cluster4 <- kmeans(df, 4, nstart = 25)
cluster5 <- kmeans(df, 5, nstart = 25)
cluster6 <- kmeans(df, 6, nstart = 25)
cluster7 <- kmeans(df, 7, nstart = 25)


grafica1 <- fviz_cluster(cluster3, geom = "point", data = mainDataframe,show.clust.cent = TRUE, ellipse = TRUE)
grafica2 <- fviz_cluster(cluster4, geom = "point", data = mainDataframe,show.clust.cent = TRUE, ellipse = TRUE)
grafica3 <- fviz_cluster(cluster5, geom = "point", data = mainDataframe,show.clust.cent = TRUE, ellipse = TRUE)
grafica4 <- fviz_cluster(cluster6, geom = "point", data = mainDataframe,show.clust.cent = TRUE, ellipse = TRUE)
grafica5 <- fviz_cluster(cluster7, geom = "point", data = mainDataframe,show.clust.cent = TRUE, ellipse = TRUE)
pdf("Promedio_Simple_AcumuladoVScursos_acumulados.pdf") 
grid.arrange(rawdata, grafica1, grafica2, grafica3, grafica4,grafica5)
dev.off()

###################################################################################################################
#Promedio_Simple_Acumulado VS año

mainDataframe <- read_excel("ListadoPromedios.xlsx", 
                               col_types = c("numeric", "skip", "skip", 
                                             "skip", "skip", "skip", "skip", "skip", 
                                             "numeric", "skip", "skip"))
mainDataframe<-mainDataframe[!mainDataframe$año == "0", ]
mainDataframe<-mainDataframe[!mainDataframe$año == "5", ]

rawdata<-ggplot(mainDataframe, aes(x = año , y =  Promedio_Simple_Acumulado)) + geom_point()
grid.arrange(rawdata)

df<- scale(mainDataframe)
head(df)

set.seed(123)
cluster3 <- kmeans(df, 3, nstart = 25)
cluster4 <- kmeans(df, 4, nstart = 25)
cluster5 <- kmeans(df, 5, nstart = 25)
cluster6 <- kmeans(df, 6, nstart = 25)
cluster7 <- kmeans(df, 7, nstart = 25)


grafica1 <- fviz_cluster(cluster3, geom = "point", data = mainDataframe,show.clust.cent = TRUE, ellipse = TRUE)
grafica2 <- fviz_cluster(cluster4, geom = "point", data = mainDataframe,show.clust.cent = TRUE, ellipse = TRUE)
grafica3 <- fviz_cluster(cluster5, geom = "point", data = mainDataframe,show.clust.cent = TRUE, ellipse = TRUE)
grafica4 <- fviz_cluster(cluster6, geom = "point", data = mainDataframe,show.clust.cent = TRUE, ellipse = TRUE)
grafica5 <- fviz_cluster(cluster7, geom = "point", data = mainDataframe,show.clust.cent = TRUE, ellipse = TRUE)
pdf("Promedio_Simple_AcumuladoVSaño.pdf") 
grid.arrange(rawdata, grafica1, grafica2, grafica3, grafica4,grafica5)
dev.off()

##################################################################################################################
#Promedio_Simple_Acumulado VS cursos_x_ciclo

mainDataframe <- read_excel("ListadoPromedios.xlsx", 
                               col_types = c("skip", "skip", "skip", 
                                             "skip", "skip", "skip", "skip", "numeric", 
                                             "numeric", "skip", "skip"))

rawdata<-ggplot(mainDataframe, aes(x = cursos_x_ciclo , y =  Promedio_Simple_Acumulado)) + geom_point()
grid.arrange(rawdata)

df<- scale(mainDataframe)
head(df)

set.seed(123)
cluster3 <- kmeans(df, 3, nstart = 25)
cluster4 <- kmeans(df, 4, nstart = 25)
cluster5 <- kmeans(df, 5, nstart = 25)
cluster6 <- kmeans(df, 6, nstart = 25)
cluster7 <- kmeans(df, 2, nstart = 25)


grafica1 <- fviz_cluster(cluster3, geom = "point", data = mainDataframe,show.clust.cent = TRUE, ellipse = TRUE)
grafica2 <- fviz_cluster(cluster4, geom = "point", data = mainDataframe,show.clust.cent = TRUE, ellipse = TRUE)
grafica3 <- fviz_cluster(cluster5, geom = "point", data = mainDataframe,show.clust.cent = TRUE, ellipse = TRUE)
grafica4 <- fviz_cluster(cluster6, geom = "point", data = mainDataframe,show.clust.cent = TRUE, ellipse = TRUE)
grafica5 <- fviz_cluster(cluster7, geom = "point", data = mainDataframe,show.clust.cent = TRUE, ellipse = TRUE)
pdf("Promedio_Simple_AcumuladoVScursos_x_ciclo.pdf") 
grid.arrange(rawdata, grafica1, grafica2, grafica3, grafica4,grafica5)
dev.off()

##################################################################################################################

