#install.packages("factoextra")
#install.packages("sqldf")
#install.packages("dplyr")
library(factoextra)
library(ggplot2)
library(readxl)
library(gridExtra)
library(dplyr)

#Load Raw Data
setwd("..")
setwd("Clustering/")
ListadoPromedios <- read_excel("ListadoPromedios.xlsx")
ListadoPromedios$cfi <- 0
ListadoPromedios$promedio_cfi <- 0 

Pensum11001 <- read_excel("Pensum11001.xls")
Pensum13001 <- read_excel("Pensum13001.xls")
Pensum18001 <- read_excel("Pensum18001.xls")
#View(ListadoPromedios)
#View(Pensum11001)
#View(Pensum13001)
#View(Pensum18001)

#Load and merge all grades
setwd("Notas/")
gradesDataset=data.frame()
file_list <- list.files()
for (file in file_list){
  names<-strsplit(file[1], ".csv")
  # if the merged gradesDataset doesn't exist, create it
  if (!exists("gradesDataset")){
    gradesDataset <- read.table(file, header=TRUE, sep=",")
    gradesDataset$ID <- rep(names[1],nrow(gradesDataset))
  }
  
  # if the merged gradesDataset does exist, append to it
  if (exists("gradesDataset")){
    temp_dataset=data.frame()
    temp_dataset <-read.table(file, header=TRUE, sep=",")
    temp_dataset$ID <- rep(names[1],nrow(temp_dataset))
    gradesDataset<-rbind(gradesDataset, temp_dataset)
    #grades<- gradesDataset[which(gradesDataset$Nota != 'A' & gradesDataset$Nota_Calculo_Prom != 'NA'),]
    ListadoPromedios$cfi[which(ListadoPromedios$ID == rep(names[1]))] <- temp_dataset %>% filter((grepl("(CFI)",  Nombre_Curso) | grepl("(EDP)",  Nombre_Curso)) & Nota != 'A' & Nota != 'R') %>% summarise(cursos = n_distinct(No_curso)) 
    #temp_dataset$Nota <- as.numeric(temp_dataset$Nota)
    #test <- temp_dataset$Nota
    ListadoPromedios$promedio_cfi[which(ListadoPromedios$ID == rep(names[1]))] <- temp_dataset %>% filter((grepl("(CFI)", Nombre_Curso ) | grepl("(EDP)", Nombre_Curso)) & Nota != 'A' & Nota != 'R') %>% summarise(promedio = mean(strtoi(Nota, base= 0L)))
    #test <- temp_dataset %>% filter(grepl("(CIENCIAS BASICAS)", Eje )) %>% summarise(cursos = mean(Nota))
    #TEST <- temp_dataset %>% filter(grepl("(CIENCIAS BASICAS)", Eje)) %>% group_by(No_ciclo) %>% summarise(cursos = n_distinct(No_curso)) 
    #TEST
    rm(temp_dataset)
  }
}



#rawdata<-ggplot(ListadoPromedios, aes(x = cursos_acumulados, y = Promedio_Simple_Acumulado)) + geom_point()
#grid.arrange(rawdata)

#cursos <-gradesDataset[, c('Nota', 'Nota_Calculo_Prom')]
ListadoPromedios <- subset(ListadoPromedios, cfi != 0 )
ListadoPromedios$cfi <- unlist(ListadoPromedios$cfi)
ListadoPromedios$promedio_cfi <- unlist(ListadoPromedios$promedio_cfi)
cursos <- ListadoPromedios[, c('cfi', 'promedio_cfi')]


df<- scale(cursos)
head(df)

set.seed(123)
clusterk7 <- kmeans(df, 7, nstart = 25)
clusterk2 <- kmeans(df, 2, nstart = 25)
clusterk3 <- kmeans(df, 3, nstart = 25)
clusterk4 <- kmeans(df, 4, nstart = 25)
clusterk5 <- kmeans(df, 5, nstart = 25)
clusterk6 <- kmeans(df, 6, nstart = 25)
grafica7 <- fviz_cluster(clusterk7, geom = "point", data = df, show.clust.cent = TRUE, ellipse = TRUE) + ggtitle("k = 7")
grafica2 <- fviz_cluster(clusterk2, geom = "point", data = df, show.clust.cent = TRUE, ellipse = TRUE) + ggtitle("k = 2")
grafica3 <- fviz_cluster(clusterk3, geom = "point", data = df, show.clust.cent = TRUE, ellipse = TRUE) + ggtitle("k = 3")
grafica4 <- fviz_cluster(clusterk4, geom = "point", data = df, show.clust.cent = TRUE, ellipse = TRUE) + ggtitle("k = 4")
grafica5 <- fviz_cluster(clusterk5, geom = "point", data = df, show.clust.cent = TRUE, ellipse = TRUE) + ggtitle("k = 5")
grafica6 <- fviz_cluster(clusterk6, geom = "point", data = df, show.clust.cent = TRUE, ellipse = TRUE) + ggtitle("k = 6")
grid.arrange(grafica2, grafica3, grafica4,  grafica5,  grafica6,grafica7,   nrow = 2)



fviz_nbclust(df, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)
