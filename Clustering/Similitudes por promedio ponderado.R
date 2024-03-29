#install.packages("factoextra")
library(factoextra)
library(ggplot2)
library(readxl)
library(gridExtra)



# Listado de primedios unicamente con valores numericos
ListadoPromediosNumeric <- read_excel("ListadoPromedios.xlsx", 
                                      col_types = c("numeric", "numeric", "skip", 
                                                    "skip", "skip", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", 
                                                    "numeric"))
#View(ListadoPromediosNumeric)


#Promedio_Ponderado_Acumulado VS cursos_acumulados

mainDataframe <-  read_excel("ListadoPromedios.xlsx", 
                             col_types = c("skip", "skip", "skip", 
                                           "skip", "skip", "skip", "skip", "skip", 
                                           "skip", "numeric", "numeric"))

rawdata<-ggplot(mainDataframe, aes(x = Promedio_Ponderado_Acumulado, y = cursos_acumulados)) + geom_point()
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
pdf("Promedio_Ponderado_AcumuladoVScursos_acumulados.pdf") 
grid.arrange(rawdata, grafica1, grafica2, grafica3, grafica4,grafica5)


rng<-2:10 #K from 2 to 20
tries <-100 #Run the K Means algorithm 100 times
avg.totw.ss <-integer(length(rng)) #Set up an empty vector to hold all of points
for(v in rng){ # For each value of the range variable
  v.totw.ss <-integer(tries) #Set up an empty vector to hold the 100 tries
  for(i in 1:tries){
    k.temp <-kmeans(df,centers=v) #Run kmeans
    v.totw.ss[i] <-k.temp$tot.withinss#Store the total withinss
  }
  avg.totw.ss[v-1] <-mean(v.totw.ss) #Average the 100 total withinss
}
TotalWithin <- plot(rng,avg.totw.ss,type="b", main="Total Within SS by Various K",
                    ylab="Average Total Within Sum of Squares",
                    xlab="Value of K")
dev.off()

###################################################################################################################
#Promedio_Ponderado_Acumulado VS año

mainDataframe <- read_excel("ListadoPromedios.xlsx", 
                            col_types = c("numeric", "skip", "skip", 
                                          "skip", "skip", "skip", "skip", "skip", 
                                          "skip", "numeric", "skip"))
mainDataframe<-mainDataframe[!mainDataframe$año == "0", ]
mainDataframe<-mainDataframe[!mainDataframe$año == "5", ]

rawdata<-ggplot(mainDataframe, aes(x = año , y =  Promedio_Ponderado_Acumulado)) + geom_point()
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
pdf("Promedio_Ponderado_AcumuladoVSaño.pdf") 
grid.arrange(rawdata, grafica1, grafica2, grafica3, grafica4,grafica5)


rng<-2:10 #K from 2 to 20
tries <-100 #Run the K Means algorithm 100 times
avg.totw.ss <-integer(length(rng)) #Set up an empty vector to hold all of points
for(v in rng){ # For each value of the range variable
  v.totw.ss <-integer(tries) #Set up an empty vector to hold the 100 tries
  for(i in 1:tries){
    k.temp <-kmeans(df,centers=v) #Run kmeans
    v.totw.ss[i] <-k.temp$tot.withinss#Store the total withinss
  }
  avg.totw.ss[v-1] <-mean(v.totw.ss) #Average the 100 total withinss
}
TotalWithin <- plot(rng,avg.totw.ss,type="b", main="Total Within SS by Various K",
                    ylab="Average Total Within Sum of Squares",
                    xlab="Value of K")
dev.off()

##################################################################################################################
#Promedio_Ponderado_Acumulado VS cursos_x_ciclo

mainDataframe <- read_excel("ListadoPromedios.xlsx", 
                            col_types = c("skip", "skip", "skip", 
                                          "skip", "skip", "skip", "skip", "numeric", 
                                          "skip", "numeric", "skip"))

rawdata<-ggplot(mainDataframe, aes(x = cursos_x_ciclo , y =  Promedio_Ponderado_Acumulado)) + geom_point()
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
pdf("Promedio_Ponderado_AcumuladoVScursos_x_ciclo.pdf") 
grid.arrange(rawdata, grafica1, grafica2, grafica3, grafica4,grafica5)


rng<-2:10 #K from 2 to 20
tries <-100 #Run the K Means algorithm 100 times
avg.totw.ss <-integer(length(rng)) #Set up an empty vector to hold all of points
for(v in rng){ # For each value of the range variable
  v.totw.ss <-integer(tries) #Set up an empty vector to hold the 100 tries
  for(i in 1:tries){
    k.temp <-kmeans(df,centers=v) #Run kmeans
    v.totw.ss[i] <-k.temp$tot.withinss#Store the total withinss
  }
  avg.totw.ss[v-1] <-mean(v.totw.ss) #Average the 100 total withinss
}
TotalWithin <- plot(rng,avg.totw.ss,type="b", main="Total Within SS by Various K",
                    ylab="Average Total Within Sum of Squares",
                    xlab="Value of K")
dev.off()

##################################################################################################################

