install.packages("factoextra")
install.packages("tidyverse")
library(factoextra)
library(ggplot2)
library(readxl)
library(gridExtra)
library(tidyverse)
library(dplyr)

setwd("Clustering/")
ListadoPromedios <- read_excel("ListadoPromedios.xlsx")
Pensum11001 <- read_excel("Pensum11001.xls")
Pensum13001 <- read_excel("Pensum13001.xls")
Pensum18001 <- read_excel("Pensum18001.xls")

#Load and merge all grades
setwd("Notas/")
file_list <- list.files()
gradesDataset=data.frame()
for (file in file_list){
  names<-tools::file_path_sans_ext(basename(file))
  #print(names[1])
  # if the merged gradesDataset doesn't exist, create it
  if (!exists("gradesDataset")){
    gradesDataset <- read.table(file, header=TRUE, sep=",")
    gradesDataset$ID <- rep(names,nrow(gradesDataset))
    
  }
  
  # if the merged gradesDataset does exist, append to it
  if (exists("gradesDataset")){
    temp_dataset=data.frame()
    temp_dataset <-read.table(file, header=TRUE, sep=",")
    temp_dataset$ID <- rep(names,nrow(temp_dataset))
    gradesDataset<-rbind(gradesDataset, temp_dataset)
    rm(temp_dataset)
  }
}

setwd("..")
rm(file_list)
rm(file)
rm(names)
names(gradesDataset)[names(gradesDataset) == "No_pensum"] <- "No_Pensum"

#Outer join: merge(x = df1, y = df2, by = "CustomerId", all = TRUE)

#Left outer: merge(x = df1, y = df2, by = "CustomerId", all.x = TRUE)

#Right outer: merge(x = df1, y = df2, by = "CustomerId", all.y = TRUE)

#Cross join: merge(x = df1, y = df2, by = NULL)

df<-gradesDataset

NombresEje <- function(df){
  df <- df %>% mutate_if(is.factor, as.character) %>% glimpse()
  df$Eje[df$Eje == "GESTIÓN (OTROS)"] <- "OTROS"
  df$Eje[df$Eje == "INGENIERÍA PRIMERO (OTROS)"] <- "OTROS"
  df$Eje[df$Eje == "INGENIERÍA APLICADA (OTROS)"] <- "OTROS"
  df$Eje[df$Eje == "CIENCIAS DE LA COMPUTACIÓN (CIENCIAS DE INGENIERIA)"] <- "CIENCIAS DE INGENIERIA"
  df$Eje[df$Eje == "PROGRAMACIÓN (CIENCIAS DE INGENIERIA)"] <- "CIENCIAS DE INGENIERIA"
  df$Eje[df$Eje == "CIENCIAS DE LA COMPUTACIÓN (CIENCIAS DE INGENIERIA)"] <- "CIENCIAS DE INGENIERIA"
  df$Eje[df$Eje == "BASES DE DATOS (CIENCIAS DE INGENIERIA)"] <- "CIENCIAS DE INGENIERIA"
  df$Eje[df$Eje == "INGENIERIA DE SOFTWARE (CIENCIAS DE INGENIERIA)"] <- "CIENCIAS DE INGENIERIA"
  df$Eje[df$Eje == "(CIENCIAS DE INGENIERIA)"] <- "CIENCIAS DE INGENIERIA"
  df$Eje[df$Eje == "QUIMICA (CIENCIAS BASICAS)"] <- "CIENCIAS BASICAS"
  df$Eje[df$Eje == "MATEMATICA (CIENCIAS BASICAS)"] <- "CIENCIAS BASICAS"
  df$Eje[df$Eje == "MATEMÁTICA (CIENCIAS BASICAS)"] <- "CIENCIAS BASICAS"
  df$Eje[df$Eje == "QUÍMICA (CIENCIAS BASICAS)"] <- "CIENCIAS BASICAS"
  df$Eje[df$Eje == "ESTADÍSTICA (CIENCIAS BASICAS)"] <- "CIENCIAS BASICAS"
  df$Eje[df$Eje == "FÍSICA (CIENCIAS BASICAS)"] <- "CIENCIAS BASICAS"
  df$Eje[df$Eje == "FISICA (CIENCIAS BASICAS)"] <- "CIENCIAS BASICAS"
  df$Eje[df$Eje == "ADMINISTRACIÓN DE INFORMACIÓN ( PROFESIONAL)"] <- "PROFESIONAL"
  df$Eje[df$Eje == "ESTADÍSTICA E INVESTIGACIÓN DE OPERACIONES ( PROFESIONAL)"] <- "PROFESIONAL"
  df$Eje[df$Eje == "GERENCIAL EMPRESARIAL ( PROFESIONAL)"] <- "PROFESIONAL"
  df$Eje[df$Eje == "NACIONAL Y SOCIAL (CIENCIAS SOCIALES Y HUMANIDADE)"] <- "CIENCIAS SOCIALES Y HUMANIDADES"
  df$Eje[df$Eje == "PERSONAL Y ESTÉTICA (CIENCIAS SOCIALES Y HUMANIDADE)"] <- "CIENCIAS SOCIALES Y HUMANIDADES"
  df$Eje[df$Eje == "SISTEMAS (INGENIERÍA APLICADA)"] <- "INGENIERÍA APLICADA"
  df$Eje[df$Eje == "INFORMÁTICA (INGENIERÍA APLICADA)"] <- "INGENIERÍA APLICADA"
  df <- within(df, Eje <- ifelse(is.na(Eje), "CFI", Eje))
  return(df)
}




PromediosEstudiantes <- merge(x = ListadoPromedios, y = gradesDataset, by = "ID", all = TRUE)

PromediosEstudiantes <-NombresEje(PromediosEstudiantes)

mainDataframe = data.frame(PromediosEstudiantes$Eje,PromediosEstudiantes$Promedio_Simple_Acumulado)
names(mainDataframe)[names(mainDataframe) == "PromediosEstudiantes.Eje"] <- "Eje"
names(mainDataframe)[names(mainDataframe) == "PromediosEstudiantes.Promedio_Simple_Acumulado"] <- "Promedio_Simple_Acumulado"
with(mainDataframe, by(Promedio_Simple_Acumulado, Eje, mean))



rawdata<-ggplot(mainDataframe, aes(x = Eje, y = Promedio_Simple_Acumulado)) + geom_point()
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

grid.arrange(rawdata, grafica1, grafica2, grafica3, grafica4,grafica5)


