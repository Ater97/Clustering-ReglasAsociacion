install.packages("factoextra")
library(factoextra)
library(ggplot2)
library(readxl)

#Load Raw Data
setwd("..")
setwd("Analisis Proyecto 2/")
ListadoPromedios <- read_excel("ListadoPromedios.xlsx")
Pensum11001 <- read_excel("Pensum11001.xls")
Pensum13001 <- read_excel("Pensum13001.xls")
Pensum18001 <- read_excel("Pensum18001.xls")
View(ListadoPromedios)
View(Pensum11001)
View(Pensum13001)
View(Pensum18001)


#Load and merge all grades
setwd("Notas/")
file_list <- list.files()
for (file in file_list){
  
  # if the merged gradesDataset doesn't exist, create it
  if (!exists("gradesDataset")){
    gradesDataset <- read.table(file, header=TRUE, sep=",")
  }
  
  # if the merged gradesDataset does exist, append to it
  if (exists("gradesDataset")){
    temp_dataset <-read.table(file, header=TRUE, sep=",")
    gradesDataset<-rbind(gradesDataset, temp_dataset)
    rm(temp_dataset)
  }
  
}


rawdata<-ggplot(ListadoPromedios, aes(x = cursos_acumulados, y = Promedio_Simple_Acumulado)) + geom_point()
grid.arrange(rawdata)

df<- scale(gradesDataset)
head(df)



