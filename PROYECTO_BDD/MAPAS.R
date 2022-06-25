library(tmap)
library(tmaptools)
library(colmaps)
library(ggplot2)
library(sp)
library(tidyverse)
library(sf)


library(GADMTools)
COL <- gadm_sf_loadCountries(c("COL"), level=0, basefile="./")

DEPTOS <- gadm_sf_loadCountries(c("COL"), level = 1, basefile = "./")
# gadm_plot(DEPTOS)
# suppressPackageStartupMessages(library(mapview))
# DEPTOS$sf %>% mapview(zcol = "NAME_1", legend = TRUE, col.regions = sf.colors)


library(readxl)
MAPA <- read_excel("C:/Users/Admin/Downloads/MAPA.xlsx")
MAPA$DEPTO<- c("Quindio",
               "Atlantico",
               "Cundinamarca",
               "Santander",
               "Valle del Cauca",
               "Bolívar",
               "Norte de Santander",
               "Cundinamarca",
               "Cundinamarca",
               "Tolima",
               "Caldas",
               "Antioquia",	
               "Córdoba",
               "Huila"	,
               "Risaralda",
               "Magdalena",
               "Boyacá",	
               "Cesar",
               "Meta",
               "Casanare")

# MAPA
# MUNICIPIOS <- gadm_sf_loadCountries(c("COL"), level = 1, basefile = "./")
# gadm_plot(MUNICIPIOS)


DEPTOS <- gadm_sf_loadCountries(c("COL"), level = 1, basefile = "./")

map_colo=DEPTOS$sf
colnames(map_colo)=c("ISO","PAIS","DEPTO","TYPE_1",   "ENGTYPE_1","geometry" )


BASE_MAPA<-merge(x =map_colo ,y = MAPA, by="DEPTO")[,-c(2,4,5)]

library(leafpop)

# colnames(unique(BASE_MAPA))
library(mapview)
colnames(BASE_MAPA)<- c("DEPTO","PAIS", "CIUDAD" ,"TOTAL_CONTEO_DESPACHOS", "PROMEDIO_ENTREGAS" ,"PROMEDIO_DESPACHOS_DIA", "geometry")
mapview(BASE_MAPA,zcol = "DEPTO", legend = TRUE, col.regions = sf.colors,  popup = popupTable(
  BASE_MAPA,
  zcol = c("DEPTO","PAIS", "CIUDAD" ,"TOTAL_CONTEO_DESPACHOS", "PROMEDIO_ENTREGAS" ,"PROMEDIO_DESPACHOS_DIA"
  )
))



