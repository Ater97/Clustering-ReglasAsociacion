install.packages("factoextra")
install.packages("tidyverse")
install.packages("scales")
install.packages("rlang")

library(factoextra)
library(ggplot2)
library(readxl)
library(gridExtra)
library(arules)
library(tidyverse)

#Load Raw Data
setwd("..")
setwd("Clustering/")
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
gradesDataset=data.frame()
file_list <- list.files()0
for (file in file_list){
  #Quitarle a los nobres la extencion
  names<-tools::file_path_sans_ext(basename(file))
  #print(names[1])
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
    rm(temp_dataset)
  }
}
dir.create(path = "tmp", showWarnings = FALSE)
gradesDataset[order(gradesDataset$No_ciclo),]
gradesDataset<-gradesDataset[filter(gradesDataset$Nota > 65),]
gradesDataset%>% filter(gradesDataset.Nota > 65)
gradesDataset<-gradesDataset[order(gradesDataset$No_ciclo),]
newgradesDataset<- data.frame()
newgradesDataset <- data.frame(gradesDataset$No_ciclo, gradesDataset$Nombre_Curso)
names(newgradesDataset) <- c("No_ciclo", "Nombre_Curso" )
View(newgradesDataset)
write.table(newgradesDataset, './tmp/output.csv', sep = ',', row.names = FALSE, append = FALSE, fileEncoding = 'UTF-8')
order_trans<-read.table('./tmp/output.csv', header=TRUE, sep=",")

datos_split <- split(x = newgradesDataset$No_ciclo, f = newgradesDataset$Nombre_Curso)
order_trans <- as(datos_split, Class = "transactions")

order_trans <- read.transactions(
  file = "./tmp/output.csv",
  format = "single",
  sep = ",",
  header=TRUE,
  cols=c(1,2),
  rm.duplicates = TRUE
)
order_trans@itemInfo
inspect(order_trans)
image(order_trans)


datos_split <- split(x = gradesDataset$Nombre_Curso, f = gradesDataset$No_curso)
datos_split<- gradesDataset %>% as.data.frame() %>% mutate(valor = 1) %>%spread(key = Nombre_Curso, value = valor, fill = 0) %>%
  column_to_rownames(var = "No_curso") %>%
  as.matrix()
order_trans <- as(datos_split, Class = "transactions")


rawdata<-ggplot(ListadoPromedios, aes(x = cursos_acumulados, y = Promedio_Simple_Acumulado)) + geom_point()
grid.arrange(rawdata)

df<- scale(gradesDataset)
head(df)

set.seed(123)
cluster4 <- kmeans(df, 3, nstart = 25)


#:eemos otro dataset
data(Groceries)
Groceries
Groceries@itemInfo

#definimos reglas
rules <- apriori(Groceries, parameter=list(support=0.001, confidence=0.5))
inspect(rules)

#extraemos reglas con confianza =0.8 o mas
subrules <- rules[quality(rules)$confidence > 0.8]
inspect(subrules)

#vemos solo el top 3 con mayor lift
rules_high_lift <- head(sort(rules, by="lift"), 3)
inspect(rules_high_lift)

image(Groceries)
a<-as(subrules,"data.frame")

