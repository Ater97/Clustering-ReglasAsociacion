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
ListadoPromedios$cursos_numericos <- 0


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
    ListadoPromedios$cursos_numericos[which(ListadoPromedios$ID == rep(names[1]) )] <- temp_dataset %>% filter(grepl("(CIENCIAS BASICAS)", Eje)) %>% summarise(cursos = n_distinct(No_curso)) 
    #TEST <- temp_dataset %>% filter(grepl("(CIENCIAS BASICAS)", Eje)) %>% summarise(cursos = n_distinct(No_curso)) 
    rm(temp_dataset)
  }
}

#rawdata<-ggplot(ListadoPromedios, aes(x = cursos_acumulados, y = Promedio_Simple_Acumulado)) + geom_point()
#grid.arrange(rawdata)

cursos <-gradesDataset[, c('Nota', 'Nota_Calculo_Prom')]

df<- scale(cursos)
head(df)

set.seed(123)
clusterk <- kmeans(df, 4, nstart = 25)
grafica1 <- fviz_cluster(clusterk, geom = "point", data = df, show.clust.cent = TRUE, ellipse = TRUE) + ggtitle("k = 4")
grid.arrange(grafica1, nrow = 2)
