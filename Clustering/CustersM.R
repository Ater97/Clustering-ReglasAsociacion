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
  Pensum11001 <<- read_excel("Pensum11001.xls")
  Pensum13001 <<- read_excel("Pensum13001.xls")
  Pensum18001 <<- read_excel("Pensum18001.xls")
  
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

loadData()
names(gradesDataset)[names(gradesDataset) == "No_pensum"] <- "No_Pensum"


PromediosEstudiantes <- merge(x = ListadoPromedios, y = gradesDataset, by = "ID", all = TRUE, stringsAsFactors=TRUE)
df<-subset(PromediosEstudiantes, select=-c(ID,sede, año, facultad, carrera, No_seccion_fac))



######## ListadoPromedios gower ###################################################################################

df <- read_excel("ListadoPromedios.xlsx", 
                 col_types = c("numeric", "skip", "skip", 
                               "skip", "skip", "numeric", "numeric", 
                               "skip", "numeric", "numeric", "skip"))

pdf("ListadoPromedios gower.pdf") 
#“euclidean”, “manhattan”, “gower”
gower_dist <- daisy(df, metric = "gower")
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

dev.off()
