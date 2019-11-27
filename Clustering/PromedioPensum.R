install.packages("factoextra")
library(factoextra)
library(ggplot2)
library(readxl)
library(gridExtra)

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
  #Quitarle a los nobres la extencion
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
names(gradesDataset)[names(gradesDataset) == "No_pensum"] <- "No_Pensum"

#Outer join: merge(x = df1, y = df2, by = "CustomerId", all = TRUE)

#Left outer: merge(x = df1, y = df2, by = "CustomerId", all.x = TRUE)

#Right outer: merge(x = df1, y = df2, by = "CustomerId", all.y = TRUE)

#Cross join: merge(x = df1, y = df2, by = NULL)


PromediosEstudiantes <- merge(x = ListadoPromedios, y = gradesDataset, by = "ID", all = TRUE)

test1 <- merge(x = PromediosEstudiantes, y = Pensum11001, by = NULL)

NULLtest1 <- merge(x = test1, y = Pensum13001, by = NULL)

test1 <- merge(x = test1, y = Pensum18001, by = "No_Pensum", all = TRUE)





