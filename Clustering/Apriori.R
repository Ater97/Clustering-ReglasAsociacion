install.packages("factoextra")
install.packages("tidyverse")
install.packages("scales")
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

##reprobar un curso (o más cursos) implica también reprobar otro
setwd("Notas/")
rm(myRules)
rm(rules)
rm(rrules)
rm(temp_tra)
gradesDataset=data.frame()
trans=data.frame()
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
        rules <- apriori(order_trans, parameter=list(support=0.00002, confidence=0.35,maxlen=5,maxtime=60))
        a<-as(rules,"data.frame")
        temp_tra<-order_trans
      }
      else{
        myRules <- apriori(order_trans, parameter=list(support=0.00002, confidence=0.35,maxlen=5,maxtime=60))
        rrules <- c(rules,myRules)
        trans <- c(temp_tra, order_trans)
        #rUnion <- union(r1,r2)
        rules<-rrules
        temp_tra<-trans
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
image(trans)
trans
rm(myRules)
rm(rules)
rm(rrules)

#vemos solo el top 3 con mayor lift
rules_high_lift <- head(sort(rrules, by="lift"), 30)
summary(rrules)
as(rules_high_lift, Class = "data.frame") %>%
  ggplot(aes(x = reorder(rules, lift), y = lift)) +
  geom_col() +
  coord_flip() +
  labs(title = "reprobar un curso (o más cursos) implica también reprobar otro", x = "itemsets") +
  theme_bw()
nrow(dataset)




##
setwd("Notas/")
rm(myRules)
rm(rules)
rm(rrules)
rm(temp_tra)
gradesDataset=data.frame()
trans=data.frame()
ReglasCursoxAprobados=data.frame()
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
    temp_dataset<-temp_dataset%>% filter(Nota > 65)
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
        rules <- apriori(order_trans, parameter=list(support=0.25, confidence=0.65,maxlen=5,minlen=2,maxtime=60))
        a<-as(rules,"data.frame")
        temp_tra<-order_trans
      }
      else{
        myRules <- apriori(order_trans, parameter=list(support=0.25, confidence=0.65,maxlen=5,minlen=2,maxtime=60))
        rrules <- c(rules,myRules)
        trans <- c(temp_tra, order_trans)
        #rUnion <- union(r1,r2)
        rules<-rrules
        temp_tra<-trans
        a<-as(myRules,"data.frame")
      }
      #a<-as(rules,"data.frame")
      ReglasCursoxAprobados<-rbind(ReglasCursoxAprobados, a)
      
    }
    #nrow(dataset)
    #inspect(rules)
    #temp_dataset$ID <- rep(names[1],nrow(temp_dataset))
    rm(temp_dataset)
    
  }
}

image(trans)
trans
#vemos solo el top 3 con mayor lift
rules_high_lift <- head(sort(rrules, by="lift"), 30)
summary(rrules)
as(rules_high_lift, Class = "data.frame") %>%
  ggplot(aes(x = reorder(rules, lift), y = lift)) +
  geom_col() +
  coord_flip() +
  labs(title = "aprobar un
curso (o más cursos) implica también aprobar otro", x = "itemsets") +
  theme_bw()