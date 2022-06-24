library(tm)
library(wordcloud)
library(memoise)
library(readxl)
FAMI_PROD=list("ACCESORIOS AUTOMOVILES", "AIRE LIBRE", "ALQUILER DE HERRAMIENTAS", "ASEO", "BANOS Y COCINAS",
            "CASA INTELIGENTE", "DECORACION", "ELECTRICIDAD", "ELECTROHOGAR", "FERRETERIA",
            "FIERRO/HIERRO", "HERRAMIENTAS Y MAQUINARIAS", "ILUMINACION Y VENTILADORES", "JARDIN", "MADERA Y TABLEROS",
            "MENAJE", "MUEBLES", "OBRA GRUESA", "ORGANIZACION", "PINTURA Y ACCESORIOS",
            "PISOS", "PLOMERIA / GASFITERIA", "PUERTAS/VENTANAS/MOLDURAS", "TABIQUERIA/TECHUMBRE/AISLACION", "TEMPORADA")


# Using "memoise" to automatically cache the results
getTermMatrix <- memoise(function(FAMI_PROD) {
  # Careful not to let just any name slip in here; a
  # malicious user could manipulate this value.
  
  Homecenter_Encuesta <- read_excel("C:/Users/Admin/Documents/SEBASTIAN LAVERDE/MAESTRIA/PRIMER SEMESTRE/Introduccion a la estadistica y probabilidad/BASES DE DATOS STALYN/Proyecto_Final/PROYECTO_BDD/data/2022 (1).xlsx", 
                                            sheet = "Encuesta")
  
  
  palabra<- as.character(Homecenter_Encuesta$RAZON)
  palabra_corpus<- tm::Corpus(VectorSource(palabra))
  
  palabra_corpus<- palabra_corpus %>% 
    tm_map(removePunctuation) %>%  # Remueve puntuaciones
    tm_map(removeNumbers) %>% 
    tm_map(stripWhitespace) %>% # Espacios y oraciones
    tm_map(tolower) %>% # Minusculas agrupaciones y frecuencias
    tm_map(removeWords,stopwords("Spanish")) 
  
  palabra_corpus<- tm_map(palabra_corpus, stemDocument) # Estemizar palabras
  
  palabra_count<-as.matrix(TermDocumentMatrix(palabra_corpus,control = list(minWordLength = 1)))
  palabra_freq<-sort(rowSums(palabra_count), decreasing=TRUE )
  palabra_freq
})


library(datasets)
library(ggplot2) 

# Panel Descriptivo
#helper function (convert vector to named list)
# namel<-function (vec){
#   tmp<-as.list(vec)
#   names(tmp)<-as.character(unlist(vec))
#   tmp
# }
