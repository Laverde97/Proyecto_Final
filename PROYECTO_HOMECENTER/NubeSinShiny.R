library(tidyverse)
library(tm)
library(wordcloud)
library(wesanderson)
Homecenter_Encuesta <- read_excel("~/SEBASTIAN LAVERDE/MAESTRIA/PRIMER SEMESTRE/Introduccion a la estadistica y probabilidad/BASES DE DATOS STALYN/PROYECTO SHINY/data/Homecenter.xlsx", 
                         sheet = "Encuesta")


library(tm)
library(wordcloud)
library(memoise)

FAMI_PROD=c("ACCESORIOS AUTOMOVILES", "AIRE LIBRE", "ALQUILER DE HERRAMIENTAS", "ASEO", "BANOS Y COCINAS",
            "CASA INTELIGENTE", "DECORACION", "ELECTRICIDAD", "ELECTROHOGAR", "FERRETERIA",
            "FIERRO/HIERRO", "HERRAMIENTAS Y MAQUINARIAS", "ILUMINACION Y VENTILADORES", "JARDIN", "MADERA Y TABLEROS",
            "MENAJE", "MUEBLES", "OBRA GRUESA", "ORGANIZACION", "PINTURA Y ACCESORIOS",
            "PISOS", "PLOMERIA / GASFITERIA", "PUERTAS/VENTANAS/MOLDURAS", "TABIQUERIA/TECHUMBRE/AISLACION", "TEMPORADA")


palabra<- as.character(Homecenter_Encuesta$RAZON)
palabra_corpus<- tm::Corpus(VectorSource(palabra))

palabra_corpus<- palabra_corpus %>% 
  tm_map(removePunctuation) %>%  # Remueve puntuaciones
  tm_map(removeNumbers) %>% 
  tm_map(stripWhitespace) %>% # Espacios y oraciones
  tm_map(tolower) %>% # Minusculas agrupaciones y frecuencias
  tm_map(removeWords,stopwords("Spanish")) 

palabra_corpus<- tm_map(palabra_corpus, stemDocument) # Estemizar palabras

palabra_count<-as.matrix(TermDocumentMatrix(palabra_corpus))
palabra_freq<-sort(rowSums(palabra_count), decreasing=TRUE )


wordcloud(words = names(palabra_freq),freq = palabra_freq,scale=c(3,0.5),max.words = 100,random.order = FALSE,colors = colors() )

table(Homecenter_Encuesta$`FAMILIA PRODUCTO`)
names(Homecenter_Encuesta$`FAMILIA PRODUCTO`)


