###############################################################################
#                                                                             #
#                            DOCUMENTAR EL SCRIPT                             #
#                                                                             #
###############################################################################

# Juan Laverde
# Version 0.0.1
# Curso de Bases de datos.
# 27 - 06 - 2022
# Script de la solución del segundo taller

###############################################################################
#                                                                             #
#                              COMANDOS BASICOS                               #
#                                                                             #
###############################################################################

# CTRL + SHIF + R  INSERTAR UNA SESIÓN
# CTRL + SHIF + A ARREGLA EL CÓDIGO
# Addins -----> Buscador -----> Style active file    "ARREGLA EL CÓDIGO"



###############################################################################
#                                                                             #
#                            CARGAR LAS LIBRERIAS                             #
#                                                                             #
###############################################################################
library(tidyverse)
library(readxl)
library(psych)
library(knitr)
library(readxl)
###############################################################################
#                                                                             #
#                              CARGAR LAS BASES DE                            #
#                               DATOS A UTILIZAR                              #
#                                                                             #
###############################################################################


SALUD_15_mas <- read_excel("TALLER_2/Taller/SALUD/BD_15 y más.xlsx")
ENCUESTA_15_mas <- read_excel("TALLER_2/Taller/ENCUESTA/BD_15 y más.xlsx")

###############################################################################
#                                                                             #
#                              CAMBIAMOS LOS NOMBRES                          #
#                                A LAS VARAIBLES                              #
#                                                                             #
###############################################################################

colnames(ENCUESTA_15_mas) <-
  c(
    "Identificador",
    "p6edad",
    "p7sexo",
    "p14sgsss",
    "p31usaamalgama",
    "p32amalgamacasa",
    "p33almacenahg",
    "p34distancia",
    "p35guardatoxicas" ,
    "p36manipulatoxicas",
    "p37cercafumigaciones",
    "p38zonaproduccion"
  )
# DATOS<-list(SALUD_15_mas,ENCUESTA_15_mas)

# colnames(unique(SALUD_15_mas))
#
#
# colnames(unique(ENCUESTA_15_mas))

# Hacemos un Left_Join para unir dos bases de datos por la variable "Identificador"


BASE_EXPLO1 <- left_join(x = ENCUESTA_15_mas, y = SALUD_15_mas, by = "Identificador")



###############################################################################
#                                                                             #
#                           REALIZAMOS LOS INDICADORES                        #
#                           DE Encuesta (BD_15 y más)                         #
#                                   Niños                                     #
#                                                                             #
###############################################################################


Ninos <- BASE_EXPLO1 %>%
  filter(p6edad < 18)

# Indicador 25 ------------------------------------------------------------


Indi25 <- Ninos %>%
  filter(p31usaamalgama == 1) %>%
  summarise("id25" = n() / dim(Ninos)[1]) * 100

# Indicador 26 ------------------------------------------------------------


Indi26 <- Ninos %>%
  filter(p33almacenahg == 1) %>%
  summarise("id26" = n() / dim(Ninos)[1]) * 100

# Indicador 27 ------------------------------------------------------------


Indi27 <- Ninos %>%
  filter(p35guardatoxicas == 1) %>%
  summarise("id27" = n() / dim(Ninos)[1]) * 100


# Indicador 28 ------------------------------------------------------------


Indi28 <- Ninos %>%
  filter(p36manipulatoxicas == 1) %>%
  summarise("id28" = n() / dim(Ninos)[1]) * 100

# Indicador 29 ------------------------------------------------------------


Indi29 <- Ninos %>%
  filter(p37cercafumigaciones == 1) %>%
  summarise("id29" = n() / dim(Ninos)[1]) * 100


# Indicador 30 ------------------------------------------------------------


Indi30 <- Ninos %>%
  filter(p38zonaproduccion == 1) %>%
  summarise("id30" = n() / dim(Ninos)[1]) * 100


###############################################################################
#                                                                             #
#                           REALIZAMOS LOS INDICADORES                        #
#                             DE SALUD (BD_15 y más)                          #
#                                   ADULTOS                                   #
#                                                                             #
###############################################################################

Adultos <- BASE_EXPLO1 %>%
  filter(p6edad >= 18)




# Indicador 35 ------------------------------------------------------------

Indi35 <- Adultos %>%
  filter(
    p12cancerpulmon == 1 |
      p13cancervejiga == 1 | p14cancerpiel == 1 | p15cancerriñon == 1 |
      p16cancerprostata == 1 | p17cancerhigado == 1
  ) %>%
  summarise("id35" = n() / dim(Adultos)[1]) * 100


# Indicador 36 ------------------------------------------------------------

Indi36 <- Adultos %>%
  filter(p20fractura == 1) %>%
  summarise("id36" = n() / dim(Adultos)[1]) * 100

# Indicador 37 ------------------------------------------------------------


Indi37 <- Adultos %>%
  filter(p21osteoporosis == 1) %>%
  summarise("id37" = n() / dim(Adultos)[1]) * 100

# Indicador 38 ------------------------------------------------------------

Indi38 <- Adultos %>%
  filter(p22conciencia == 1) %>%
  summarise("id38" = n() / dim(Adultos)[1]) * 100


# Indicador 39 ------------------------------------------------------------

Embaraza <- BASE_EXPLO1 %>%
  filter(p7sexo == 2)

Indi39 <- Embaraza %>%
  filter(p23embarazo == 1) %>%
  summarise("id39" = n() / dim(Embaraza)[1]) * 100


# Indicador Encuesta ------------------------------------------------------

Indica_Encuesta <-
  round(
    data.frame(
      "I25" = Indi25,
      "I26" = Indi26,
      "I27" = Indi27,
      "I28" = Indi28,
      "I29" = Indi29,
      "I30" = Indi30
    ),
    2
  )
Indica_Encuesta


# Indicador Salud ---------------------------------------------------------
Indica_Salud <-
  round(data.frame(
    "I35" = Indi35,
    "I36" = Indi36,
    "I37" = Indi37,
    "I37" = Indi38,
    "I39" = Indi39
  ),
  2)
Indica_Salud


