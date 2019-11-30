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


setwd("..")
ListadoPromedios <- read_excel("ListadoPromedios.xlsx")
ListadoPromedios$promedio_cursos_numericos<-0
ListadoPromedios$promedio_cfi<-0
ListadoPromedios$promedio_cursos_area<-0

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
      #ListadoPromedios$cursos_numericos[which(ListadoPromedios$ID == rep(names[1]))] <- temp_dataset %>% filter(grepl("(CIENCIAS BASICAS)", Eje)) %>% summarise(cursos = n_distinct(No_curso))
      ListadoPromedios$promedio_cursos_numericos[which(ListadoPromedios$ID == rep(names[1]))] <- temp_dataset %>% filter(grepl("(CIENCIAS BASICAS)", Eje ) & Nota != 'A') %>% summarise(promedio = mean(strtoi(Nota, base= 0L)))
      ListadoPromedios$promedio_cfi[which(ListadoPromedios$ID == rep(names[1]))] <- temp_dataset %>% filter((grepl("(CFI)", Nombre_Curso ) | grepl("(EDP)", Nombre_Curso)) & Nota != 'A' & Nota != 'R') %>% summarise(promedio = mean(strtoi(Nota, base= 0L)))
      #ListadoPromedios$cursos_area[which(ListadoPromedios$ID == rep(names[1]))] <- temp_dataset %>% filter(grepl("(CIENCIAS DE INGENIERIA)", Eje) & Nota != 'A' & Nota != 'R') %>% summarise(cursos = n_distinct(No_curso))
      ListadoPromedios$promedio_cursos_area[which(ListadoPromedios$ID == rep(names[1]))] <- temp_dataset %>% filter(grepl("(CIENCIAS DE INGENIERIA)", Eje ) & Nota != 'A' & Nota != 'R') %>% summarise(promedio = mean(strtoi(Nota, base= 0L)))
      
       rm(temp_dataset)
    }
  }
  
  setwd("..")
  rm(file_list)
  rm(file)
  rm(names)

  
  ListadoPromedios$promedio_cursos_area <- unlist(ListadoPromedios$promedio_cursos_area)  
  ListadoPromedios$promedio_cfi <- unlist(ListadoPromedios$promedio_cfi)
  ListadoPromedios$promedio_cursos_numericos <- unlist(ListadoPromedios$promedio_cursos_numericos)

  
  
df<- ListadoPromedios[ , c("promedio_cursos_numericos","promedio_cfi","promedio_cursos_area","Promedio_Simple_Acumulado")]    

df<- na.omit(df)
df<- subset(df, promedio_cursos_numericos != 0 )
df<- subset(df, promedio_cfi != 0 )
df<- subset(df, promedio_cursos_area != 0 )
pdf("Promedio por Area euclidean.pdf") 
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

k <- 4
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

dev.off()
