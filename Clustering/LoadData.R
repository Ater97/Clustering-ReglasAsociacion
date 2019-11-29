install.packages("factoextra")
install.packages("tidyverse")
install.packages("scales")
install.packages("rlang")
install.packages('grid')
install.packages('arules')

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

datos_split=data.frame()
#Load and merge all grades
myRules<-as(datos_split, Class = "rules")
myRules=new("rules", ...)
myRules<- apriori(myRules, parameter=list(support=0.02, confidence=0.65,maxlen=5,maxtime=60))
myRules
setwd("Notas/")
gradesDataset=data.frame()
ReglasCursoxCursosReprobados=data.frame()
file_list <- list.files()
for (file in file_list){
  #Quitarle a los nobres la extencion
  names<-tools::file_path_sans_ext(basename(file))
  #print(names[1])
  # if the merged gradesDataset doesn't exist, create it
  #if (!exists("gradesDataset")){
    #gradesDataset <- read.table(file, header=TRUE, sep=",")
    #gradesDataset$ID <- rep(names[1],nrow(gradesDataset))
    
  #}
  
  # if the merged gradesDataset does exist, append to it
  if (exists("gradesDataset")){
    temp_dataset=data.frame()
    temp_dataset <-read.table(file, header=TRUE, sep=",")
    gradesDataset<-rbind(gradesDataset, temp_dataset)
    temp_dataset<-temp_dataset[order(temp_dataset$No_ciclo),]
    temp_dataset$Nota <- as.numeric(as.character(temp_dataset$Nota))
    temp_dataset<-temp_dataset%>% filter(Nota < 65)
    temp_dataset <- data.frame(temp_dataset$No_ciclo, temp_dataset$Nombre_Curso)
    names(temp_dataset) <- c("No_ciclo", "Nombre_Curso" )
    #datosEstudiantes[[names[1]]]<-temp_dataset
    write.table(temp_dataset, './tmp/output.csv', sep = ',', row.names = FALSE, append = FALSE, fileEncoding = 'UTF-8')
    order_trans <- read.transactions(
      file = "./tmp/output.csv",
      format = "single",
      sep = ",",
      header=TRUE,
      cols=c(1,2),
      rm.duplicates = TRUE
    )
    if(nrow(order_trans)>0){
      if (!exists("rules")){
        rules <- apriori(order_trans, parameter=list(support=0.02, confidence=0.65,maxlen=5,maxtime=60))
        a<-as(rules,"data.frame")
      }
      else{
        myRules <- apriori(order_trans, parameter=list(support=0.02, confidence=0.65,maxlen=5,maxtime=60))
        #rrules <- union(rules,myRules)
        rUnion <- union(r1,r2)
        #rules<-rrules
        a<-as(myRules,"data.frame")
      }
      #a<-as(rules,"data.frame")
      ReglasCursoxCursosReprobados<-rbind(ReglasCursoxCursosReprobados, a)
      
    }
    #nrow(dataset)
    #inspect(rules)
    #temp_dataset$ID <- rep(names[1],nrow(temp_dataset))
    rm(temp_dataset)
    
  }
}
rm(myRules)
rm(rules)
top_20_itemsets <- sort(gradesDataset, by = "support", decreasing = TRUE)[1:20]
nrow(dataset)
dir.create(path = "tmp", showWarnings = FALSE)
gradesDataset[order(gradesDataset$No_ciclo),]
gradesDataset<-gradesDataset[filter(gradesDataset$Nota > 65),]
gradesDataset <- as_tibble(gradesDataset)
gradesDataset$Nota <- as.numeric(as.character(gradesDataset$Nota))
gradesDataset <-gradesDataset%>% filter(Nota < 65)
newgradesDataset<-gradesDataset%>% filter(Nota < 65)
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

my_data <- as_tibble(iris)
my_data
my_data %>% filter(Sepal.Length > 7)
my_data <-my_data %>% filter(Sepal.Length > 7)
write.table(my_data, './tmp/Prueba.csv', sep = ',', row.names = FALSE, append = FALSE, fileEncoding = 'UTF-8')
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
order_trans@itemInfo
inspect(order_trans)
order_trans
#definimos reglas
rules <- apriori(order_trans, parameter=list(support=0.000000000000000001, confidence=0.65,maxlen=5,maxtime=60))
inspect(rules)
a<-as(rules,"data.frame")

#extraemos reglas con confianza =0.8 o mas
subrules <- rules[quality(rules)$confidence > 0.8]
inspect(subrules)

#vemos solo el top 3 con mayor lift
rules_high_lift <- head(sort(rules, by="lift"), 3)
inspect(rules_high_lift)

image(Groceries)
a<-as(subrules,"data.frame")

