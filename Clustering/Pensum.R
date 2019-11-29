install.packages("factoextra")
install.packages("tidyverse")
library(factoextra)
library(ggplot2)
library(readxl)
library(gridExtra)
library(tidyverse)
library(cluster)
library(dplyr)
library(readr)
library(Rtsne)

loadData <- function(){
  #setwd("Clustering/")
  ListadoPromedios <<- read_excel("ListadoPromedios.xlsx")
  Pensum11001 <- read_excel("Pensum11001.xls")
  Pensum13001 <- read_excel("Pensum13001.xls")
  Pensum18001 <- read_excel("Pensum18001.xls")
  Pensum <<- rbind(Pensum11001, Pensum13001, Pensum18001)
  #Load and merge all grades
  setwd("Notas/")
  file_list <- list.files()
  gradesDataset<<-data.frame()
  for (file in file_list){
    names<-tools::file_path_sans_ext(basename(file))
    #print(names[1])
    # if the merged gradesDataset doesn't exist, create it
    if (!exists("gradesDataset")){
      gradesDataset<<-read.table(file, header=TRUE, sep=",")
      gradesDataset$ID<<-rep(names,nrow(gradesDataset))
      
    }
    
    # if the merged gradesDataset does exist, append to it
    if (exists("gradesDataset")){
      temp_dataset=data.frame()
      temp_dataset <-read.table(file, header=TRUE, sep=",")
      temp_dataset$ID <- rep(names,nrow(temp_dataset))
      gradesDataset<<-rbind(gradesDataset, temp_dataset)
      rm(temp_dataset)
    }
  }
  
  setwd("..")
  rm(file_list)
  rm(file)
  rm(names)
}
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

loadData()
names(gradesDataset)[names(gradesDataset) == "No_pensum"] <- "No_Pensum"

Pensum$No_Curso <- strtoi(Pensum$No_Curso, base= 0L)

PensumNotas <- merge(x = Pensum, y = gradesDataset, by = "No_Curso", all.x = TRUE)

PensumNotas[complete.cases(PensumNotas), ]



dt1 <- data.frame(Pensum$No_Curso, Pensum$No_Pensum)
dt2 <- data.frame(gradesDataset$No_Curso, gradesDataset$Nota)
### Creditos Practicos vs Teoricos de todos los pensums ######

df <-data.frame(gradesDataset$No_Pensum, gradesDataset$Nota)
names(df)[names(df) == "gradesDataset.No_Pensum"] <- "No_Pensum"
names(df)[names(df) == "gradesDataset.Nota"] <- "Nota"
df$Nota <- strtoi(df$Nota, base= 0L)

rawdata<-ggplot(df, aes(x = Nota, y = No_Pensum)) + geom_point()
grid.arrange(rawdata)

df<- scale(df)
head(df)

set.seed(123)
cluster3 <- kmeans(df, 3, nstart = 25)
cluster4 <- kmeans(df, 4, nstart = 25)
cluster5 <- kmeans(df, 5, nstart = 25)
cluster6 <- kmeans(df, 6, nstart = 25)
cluster7 <- kmeans(df, 7, nstart = 25)


grafica1 <- fviz_cluster(cluster3, geom = "point", data = df,show.clust.cent = TRUE, ellipse = TRUE)
grafica2 <- fviz_cluster(cluster4, geom = "point", data = df,show.clust.cent = TRUE, ellipse = TRUE)
grafica3 <- fviz_cluster(cluster5, geom = "point", data = df,show.clust.cent = TRUE, ellipse = TRUE)
grafica4 <- fviz_cluster(cluster6, geom = "point", data = df,show.clust.cent = TRUE, ellipse = TRUE)
grafica5 <- fviz_cluster(cluster7, geom = "point", data = df,show.clust.cent = TRUE, ellipse = TRUE)
#pdf("Promedio_Simple_AcumuladoVScursos_acumulados.pdf") 
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

#dev.off()


df <- data.frame(lapply(Pensum, as.numeric))

#“euclidean”, “manhattan”, “gower”
gower_dist <- daisy(df, metric = "euclidean")
gower_mat <- as.matrix(gower_dist)
df[which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]), arr.ind = TRUE)[1, ], ]
df[which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]), arr.ind = TRUE)[1, ], ]

sil_width <- c(NA)
for(i in 2:8){  
  pam_fit <- pam(gower_dist, diss = TRUE, k = i)  
  sil_width[i] <- pam_fit$silinfo$avg.width  
}
plot(1:8, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:8, sil_width)

k <- 3
pam_fit <- pam(gower_dist, diss = TRUE, k)
pam_results <- df %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
pam_results$the_summary


tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering))

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))
