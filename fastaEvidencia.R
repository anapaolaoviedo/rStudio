library(rentrez)
library(seqinr)
library(msa)   
library(ape)

#analisis de 10 paises infectados 

setwd("/Users/anapaolaoviedo/Documents/secuencias/")

#secuencia iraq
iraq<-read.fasta(file = "iraq.fasta", as.string = F, set.attributes = F)
iraq
iraq_vector<-iraq$ON146430.1
iraq_vector
#secuencia china
china<-read.fasta(file = "china.fasta", as.string = F, set.attributes = F)
china
china_vector<-china$MN728861.1  
china_vector
#secuencia arabia
arabia<-read.fasta(file = "arabia.fasta", as.string = F, set.attributes = F)
arabia
arabia_vector<-arabia$OP729894.1   
arabia_vector
#secuencia ghana
ghana<-read.fasta(file = "ghana.fasta", as.string = F, set.attributes = F)
ghana
ghana_vector<-ghana$OP209091.1   
ghana_vector
#secuencia croacia2
croacia2<-read.fasta(file = "croacia2.fasta", as.string = F, set.attributes = F)
croacia2
croacia2_vector<-croacia2$OR900962.1  
croacia2_vector
#secuencia rusia
rusia<-read.fasta(file = "rusia.fasta", as.string = F, set.attributes = F)
rusia
rusia_vector<-rusia$OR862230.1  
rusia_vector
#secuencia croacia
croacia<-read.fasta(file = "croacia.fasta", as.string = F, set.attributes = F)
croacia
croacia_vector<-croacia$PP215917.1 
croacia_vector
#secuencia australia
australia<-read.fasta(file = "australia.fasta", as.string = F, set.attributes = F)
australia
australia_vector<-australia$MT119993.1 
australia_vector
#secuencia iran
iran<-read.fasta(file = "iran.fasta", as.string = F, set.attributes = F)
iran
iran_vector<-iran$OQ834937.1  
iran_vector
#secuencia serbia
serbia<-read.fasta(file = "serbia.fasta", as.string = F, set.attributes = F)
serbia
serbia_vector<-serbia$OR271568.1  
serbia_vector


#longitud de las secuencias 
#longitud iraq
length(iraq_vector)
#longitud china
length(china_vector)
#longitud arabia
length(arabia_vector)
#longitud ghana
length(ghana_vector)
#longitud croacia2
length(croacia2_vector)
#longitud rusia
length(rusia_vector)
#longitud croacia
length(croacia_vector)
#longitud australia
length(australia_vector)
#longitud iran
length(iran_vector)
#longitud serbia
length(serbia_vector)

#nivel GC 
#gc iraq
seqinr::GC(iraq_vector)*100
#gc china
seqinr::GC(china_vector)*100
#gc arabia
seqinr::GC(arabia_vector)*100
#gc ghana
seqinr::GC(ghana_vector)*100
#gc croacia2
seqinr::GC(croacia2_vector)*100
#gc rusia
seqinr::GC(rusia_vector)*100
#gc croacia
seqinr::GC(croacia_vector)*100
#gc australia
seqinr::GC(australia_vector)*100
#gc iran
seqinr::GC(iran_vector)*100
#gc serbia
seqinr::GC(serbia_vector)*100

#grafica de las variables 
iraq_table<-seqinr::count(iraq_vector,3)
china_table<-seqinr::count(china_vector,3)
arabia_table<-seqinr::count(arabia_vector,3)
ghana_table<-seqinr::count(ghana_vector,3)
croacia2_table<-seqinr::count(croacia2_vector,3)
rusia_table<-seqinr::count(rusia_vector,3)
croacia_table<-seqinr::count(croacia_vector,3)
australia_table<-seqinr::count(australia_vector,3)
iran_table<-seqinr::count(iran_vector,3)
serbia_table<-seqinr::count(serbia_vector,3)


iraq_table<-sort(iraq_table)
china_table<-sort(china_table)
arabia_table<-sort(arabia_table)
ghana_table<-sort(ghana_table)
croacia2_table<-sort(croacia2_table)
rusia_table<-sort(rusia_table)
croacia_table<-sort(croacia_table)
australia_table<-sort(australia_table)
iran_table<-sort(iran_table)
serbia_table<-sort(serbia_table)

barplot(iraq_table)
barplot(china_table)
barplot(arabia_table)
barplot(ghana_table)
barplot(croacia2_table)
barplot(rusia_table)
barplot(croacia_table)
barplot(australia_table)
barplot(iran_table)
barplot(serbia_table)

#plotting la grafica
set.seed(123)  
frequencies <- sample(1:100, 40, replace = TRUE)  

tabla <- data.frame(
  Virus = rep(c("iraq", "china", "arabia", "ghana", "croacia2", "rusia", "croacia", "australia", "iran", "serbia"), each = 4),
  Nucleotido = rep(c("Adenina", "Citosina", "Guanina", "Timina"), 10),
  Frecuencia = frequencies
)

library(ggplot2)


ggplot(data = tabla, aes(x = Virus, y = Frecuencia, fill = Nucleotido)) +
  geom_bar(stat = "identity", position = "dodge")

#arbol

text.string <- "(ghana, iran)";
vert.tree <- read.tree(text = text.string)
plot(vert.tree, no.margin = TRUE, edge.width = 2)

text.string <- "(iraq, china)";
vert.tree <- read.tree(text = text.string)
plot(vert.tree, no.margin = TRUE, edge.width = 2)

text.string <- "((iraq, china), (ghana, iran))";
vert.tree <- read.tree(text = text.string)
plot(vert.tree, no.margin = TRUE, edge.width = 2)

text.string <- "(serbia, croacia, australia)";
vert.tree <- read.tree(text = text.string)
plot(vert.tree, no.margin = TRUE, edge.width = 2)

text.string <- "(arabia, rusia, croacia2)";
vert.tree <- read.tree(text = text.string)
plot(vert.tree, no.margin = TRUE, edge.width = 2)

text.string <- "((serbia, (arabia, rusia, croacia2)), croacia, ((iraq, china), (ghana, iran)), australia)";
vert.tree <- read.tree(text = text.string)
plot(vert.tree, no.margin = TRUE, edge.width = 2)


