###############################################################################
#                                                                             #
#                            DOCUMENTAR EL SCRIPT                             #
#                                                                             #
###############################################################################

# Juan Laverde
# Rodrigo Gomez
# Sergio Castellanos
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
library(dplyr)
library(webr)
library(dplyr)
###############################################################################
#                                                                             #
#                              CARGAR LAS BASES DE                            #
#                               DATOS A UTILIZAR                              #
#                                                                             #
###############################################################################


SALUD_15_mas <- read_excel("TALLER_2/Taller/SALUD/BD_15 y más.xlsx")
ENCUESTA_15_mas <-
  read_excel("TALLER_2/Taller/ENCUESTA/BD_15 y más.xlsx")


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



BASE_EXPLO1 <-
  left_join(x = ENCUESTA_15_mas, y = SALUD_15_mas, by = "Identificador")

colnames(unique(BASE_EXPLO1))
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
      p13cancervejiga == 1 |
      p14cancerpiel == 1 | p15cancerriñon == 1 |
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

###############################################################################
#                                                                             #
#                              REALIZAMOS ANÁLISIS                            #
#                                  DESCRIPTIVO                                #
#                                                                             #
###############################################################################

# Eliminamos la columna "Identificador"
Base_Descri <- select(BASE_EXPLO1,-"Identificador")

# Cambiamos las etiquetas de las variables (ENCUESTA) -------------------------

# p7sexo

Base_Descri$p7sexo <-
  factor(Base_Descri$p7sexo, labels = c("Masculino", "Femenino"))

#p14sgsss

Base_Descri$p14sgsss <-
  factor(
    Base_Descri$p14sgsss,
    labels = c("Contributivo", "Subsidiado", "Vinculado", "Especial", "NS/NR")
  )

# p31usaamalgama

Base_Descri$p31usaamalgama <-
  factor(Base_Descri$p31usaamalgama, labels = c("Si", "No",  "NS/NR"))

# p32amalgamacasa

Base_Descri$p32amalgamacasa <-
  factor(Base_Descri$p32amalgamacasa, labels = c("Si", "No",  "NS/NR"))

# p33almacenahg

Base_Descri$p33almacenahg <-
  factor(Base_Descri$p33almacenahg, labels = c("Si", "No",  "NS/NR"))

# p35guardatoxicas

Base_Descri$p35guardatoxicas <-
  factor(Base_Descri$p35guardatoxicas, labels = c("Si", "No",  "NS/NR"))

# p36manipulatoxicas

Base_Descri$p36manipulatoxicas <-
  factor(Base_Descri$p36manipulatoxicas, labels = c("Si", "No",  "NS/NR"))

# p37cercafumigaciones

Base_Descri$p37cercafumigaciones <-
  factor(Base_Descri$p37cercafumigaciones,
         labels = c("Si", "No",  "NS/NR"))

# p38zonaproduccion
Base_Descri$p38zonaproduccion <-
  factor(Base_Descri$p38zonaproduccion, labels = c("Si", "No",  "NS/NR"))


# Cambiamos las etiquetas de las variables (SALUD) --------------------------------

# p12cancerpulmon

Base_Descri$p12cancerpulmon <-
  factor(Base_Descri$p12cancerpulmon, labels = c("Si", "No",  "NS/NR"))

# p13cancervejiga

Base_Descri$p13cancervejiga <-
  factor(Base_Descri$p13cancervejiga, labels = c("No",  "NS/NR"))

# p14cancerpiel

Base_Descri$p14cancerpiel <-
  factor(Base_Descri$p14cancerpiel, labels = c("Si", "No",  "NS/NR"))

# p15cancerriñon

Base_Descri$p15cancerriñon <-
  factor(Base_Descri$p15cancerriñon, labels = c("No",  "NS/NR"))

# p16cancerprostata

Base_Descri$p16cancerprostata <-
  factor(Base_Descri$p16cancerprostata, labels = c("Si", "No",  "NS/NR"))

# p17cancerhigado

Base_Descri$p17cancerhigado <-
  factor(Base_Descri$p17cancerhigado, labels = c("Si", "No",  "NS/NR"))

# p20fractura

Base_Descri$p20fractura <-
  factor(Base_Descri$p20fractura,
         labels = c("Traumática", "No_Traumática"))

# p21osteoporosis

Base_Descri$p21osteoporosis <-
  factor(Base_Descri$p21osteoporosis, labels = c("Si", "No",  "NS/NR"))

# p22conciencia

Base_Descri$p22conciencia <-
  factor(Base_Descri$p22conciencia, labels = c("Si", "No",  "NS/NR"))

# p23embarazo
Base_Descri$p23embarazo <-
  factor(Base_Descri$p23embarazo, labels = c("Si", "No"))


# BoxPlot Edad Vs (Encuesta) ---------------------------------------------------


# Boxplot de la variable Edad

Conf3x2 = matrix(c(1:4), nrow =  2, byrow = TRUE)

layout(Conf3x2)

# Boxplot de  Edad

boxplot(
  Base_Descri$p6edad,
  horizontal = FALSE,
  ylab = "Edad",
  col = "aliceblue",
  main = "BoxPlot de Edad"
)

# Boxplot de p7sexo distribuidos por Edad

boxplot(
  Base_Descri$p6edad ~ Base_Descri$p7sexo,
  xlab = "Género",
  ylab = "Edad",
  main = "BoxPlot de Edad vs Género",
  col = rainbow(2)
)

# Boxplot de p14sgsss distribuidos por Edad

boxplot(
  Base_Descri$p6edad ~ Base_Descri$p14sgsss,
  xlab = "Tipo de usuario en el SGSSS",
  ylab = "Edad",
  main = "BoxPlot de Edad vs Tipo de usuario en el SGSSS",
  col = rainbow(2)
)

# Boxplot de p32amalgamacasa distribuidos por Edad

boxplot(
  Base_Descri$p6edad ~ Base_Descri$p32amalgamacasa,
  xlab = "Usted o algún miembro de su familia que viva con usted realiza quema de amalgama en casa",
  ylab = "Edad",
  main = "BoxPlot de Edad vs Quema de amalgama",
  col = rainbow(2)
)

# Dividimos el panel de gráficas 2x2
Conf3x2 = matrix(c(1:4), nrow =  2, byrow = TRUE)

layout(Conf3x2)

# Boxplot de p33almacenahg distribuidos por Edad

boxplot(
  Base_Descri$p6edad ~ Base_Descri$p33almacenahg,
  xlab = "Usted o algún miembro de su familia que viva con usted  guarda mercurio o azogue en casa",
  ylab = "Edad",
  main = "BoxPlot de Edad vs Mercurio",
  col = rainbow(2)
)

# Boxplot de p35guardatoxicas distribuidos por Edad

boxplot(
  Base_Descri$p6edad ~ Base_Descri$p35guardatoxicas,
  xlab = "Usted o algún miembro de su familia que viva con usted  guarda mercurio o azogue en casa",
  ylab = "Edad",
  main = "BoxPlot de Edad vs Guarda sustancias Tóxicas",
  col = rainbow(2)
)

# Boxplot de p36manipulatoxicas distribuidos por Edad

boxplot(
  Base_Descri$p6edad ~ Base_Descri$p36manipulatoxicas,
  xlab = "Usted está en contacto o manipula alguna sustancia tóxica o peligrosa (cianuro, plaguicidas  o venenos, u otra)",
  ylab = "Edad",
  main = "BoxPlot de Edad vs Manipula sustancias Tóxicas",
  col = rainbow(2)
)

# Boxplot de p37cercafumigaciones distribuidos por Edad

boxplot(
  Base_Descri$p6edad ~ Base_Descri$p37cercafumigaciones,
  xlab = "Su casa está cerca de zonas de cultivo o donde se realicen fumigaciones?",
  ylab = "Edad",
  main = "BoxPlot de Edad vs ¿Su casa está cerca de zonas de cultivo o donde se realicen fumigaciones?",
  col = rainbow(2)
)

# Boxplot de p38zonaproduccion distribuidos por Edad

boxplot(
  Base_Descri$p6edad ~ Base_Descri$p38zonaproduccion,
  xlab = "Su casa está cerca de zonas o industrias de procesamiento o producción de sustancias tóxicas",
  ylab = "Edad",
  main = "BoxPlot de Edad vs Procesamiento sustancias Tóxicas",
  col = rainbow(2)
)

# BoxPlot Edad Vs (Cancer - SALUD) ---------------------------------------------------

Conf3x2 = matrix(c(1:4), nrow =  2, byrow = TRUE)

layout(Conf3x2)

# Boxplot de p12cancerpulmon distribuidos por Edad

boxplot(
  Base_Descri$p6edad ~ Base_Descri$p12cancerpulmon,
  xlab = " ¿Le han diagnosticado cáncer de pulmón?",
  ylab = "Edad",
  main = "BoxPlot de Edad vs  ¿Le han diagnosticado cáncer de pulmón?",
  col = rainbow(3)
)

# Boxplot de p14cancerpiel distribuidos por Edad

boxplot(
  Base_Descri$p6edad ~ Base_Descri$p14cancerpiel,
  xlab = "¿Le han diagnosticado cáncer  en la piel?",
  ylab = "Edad",
  main = "BoxPlot de Edad vs  ¿Le han diagnosticado cáncer  en la piel? ",
  col = rainbow(3)
)

# Boxplot de p16cancerprostata distribuidos por Edad

boxplot(
  Base_Descri$p6edad ~ Base_Descri$p16cancerprostata,
  xlab = "¿Le han diagnosticado cáncer de próstata?",
  ylab = "Edad",
  main = "BoxPlot de Edad vs  ¿Le han diagnosticado cáncer de próstata?",
  col = rainbow(3)
)

# Boxplot de p17cancerhigado distribuidos por Edad

boxplot(
  Base_Descri$p6edad ~ Base_Descri$p17cancerhigado,
  xlab = " ¿Le han diagnosticado cáncer de hígado?",
  ylab = "Edad",
  main = "BoxPlot de Edad vs   ¿Le han diagnosticado cáncer de hígado?",
  col = rainbow(3)
)

# Estos BoxPlot no van en el trabajo
boxplot(
  Base_Descri$p6edad ~ Base_Descri$p13cancervejiga,
  xlab = "¿Le han diagnosticado cáncer de vejiga?",
  ylab = "Edad",
  main = "BoxPlot de Edad vs  ¿Le han diagnosticado cáncer de vejiga?",
  col = rainbow(3)
)

# Estos BoxPlot no van en el trabajo

boxplot(
  Base_Descri$p6edad ~ Base_Descri$p15cancerriñon,
  xlab = "¿Le han diagnosticado cáncer de riñón?",
  ylab = "Edad",
  main = "BoxPlot de Edad vs  ¿Le han diagnosticado cáncer de riñón?",
  col = rainbow(3)
)



Conf2x2 = matrix(c(1:4), nrow =  2, byrow = TRUE)

layout(Conf2x2)

# Boxplot de p20fractura distribuidos por Edad

boxplot(
  Base_Descri$p6edad ~ Base_Descri$p20fractura,
  xlab = "Si ha sufrido fracturas, la fractura fue:",
  ylab = "Edad",
  main = "BoxPlot de Edad vs   Si ha sufrido fracturas",
  col = rainbow(3)
)

# Boxplot de p21osteoporosis distribuidos por Edad

boxplot(
  Base_Descri$p6edad ~ Base_Descri$p21osteoporosis,
  xlab = "¿Le han diagnosticado osteoporosis?",
  ylab = "Edad",
  main = "BoxPlot de Edad vs ¿Le han diagnosticado osteoporosis?",
  col = rainbow(3)
)

# Boxplot de p22conciencia distribuidos por Edad

boxplot(
  Base_Descri$p6edad ~ Base_Descri$p22conciencia,
  xlab = "¿Ha presentado algún trauma severo con perdida de la conciencia?",
  ylab = "Edad",
  main = "BoxPlot de Edad vs ¿Ha presentado algún trauma severo con perdida de la conciencia?",
  col = rainbow(3)
)

# Boxplot de p23embarazo distribuidos por Edad

boxplot(
  Base_Descri$p6edad ~ Base_Descri$p23embarazo,
  xlab = "¿Actualmente se encuentra embarazada?",
  ylab = "Edad",
  main = "BoxPlot de Edad vs ¿Actualmente se encuentra embarazada?",
  col = rainbow(3)
)


# Boxplot de Osteoporosis distribuidos por Género y Edad

Base_oste = filter(Base_Descri, p21osteoporosis == "Si" | p21osteoporosis == "No")

p <- ggplot(Base_oste, aes(p7sexo, p6edad, fill = p21osteoporosis))
p + geom_boxplot(outlier.color = "red") +
  guides(fill = guide_legend(title = "Osteoporosis")) +
  xlab("Género") + ylab("Edad") + ggtitle("Boxplot de Osteoporosis \n distribuidos por Género y Edad")



# DETECCION DE OUTLIERS
g_caja = boxplot(Base_Descri$p6edad ~ Base_Descri$p21osteoporosis)
g_caja$out

# SELECCION DE VARIABLES
# 1. Edad
# 2. Genero
# 3. SGSSS
# TODAS las variables de Encuesta

# Gráfico de comparaciones entre las variables Fractura, Género y SGSSS Vs Edad


Base_Descri1 = Base_Descri %>%
  select(p7sexo, p20fractura, p6edad, p14sgsss) %>%
  na.omit(p20fractura)
colnames(Base_Descri1) = c("Género", "Fractura", "Edad", "SGSSS")
p1 <-
  ggplot(Base_Descri1, aes(x = SGSSS, y = Edad, fill = Fractura))
p1 + geom_boxplot() + facet_wrap( ~ Género, scales = "free")

Dona <- Base_Descri %>%
  select(p7sexo, p14sgsss) %>%
  group_by()
colnames(Dona) = c("Género", "SGSSS")
PieDonut(Dona, aes(Género, SGSSS), title = "Género por Tipo de usuario en el SGSSS")



###############################################################################
#                                                                             #
#                               ANÁLISIS CALIDAD                              #
#                                  INFORMACION                                #
#                                                                             #
###############################################################################


# Duplicidad variable Identificador --------------------------------------------                          


#Conteo repetidos 15 o más edad
length(which(table(SALUD_15_mas$Identificador) > 1))
length(which(table(ENCUESTA_15_mas$Identificador) > 1))

# No se encontraron repetidos en de la variable Identificador

#Variables con N/A en la base de SALUD -----------------------------------------

na_SALUD_15_mas <-
  apply(X = is.na(SALUD_15_mas),
        MARGIN = 2,
        FUN = sum)

# p20fractura con 2096 registros N/A que equivale a 90,11% del total de 
#personas  que respondieron la encuesta de SALUD de 15 o mas 

Mujeres_Conteo <-
  BASE_EXPLO1 %>% filter(p7sexo == 2) %>% summarise("Conteo_Mujeres" = n())

Mujeres <- BASE_EXPLO1 %>% filter(p7sexo==2)

na_SALUD_15_mas_mujeres <-
  apply(X = is.na(Mujeres),
        MARGIN = 2,
        FUN = sum)

# p23embarazo con 22 registros N/A que equivale a 1,48 % del total de mujeres 
# de 15 o mas que respondieron la encuesta de SALUD


# Variables con N/A en la base de ENCUESTA ------------------------------------

na_ENCUESTA_15_mas <-
  apply(X = is.na(ENCUESTA_15_mas),
        MARGIN = 2,
        FUN = sum)
na_ENCUESTA_15_mas

# p31usaamalgama con 601 registros N/A que equivale a 25,69% del total de 
# encuestados de 15 o mas años


# Personas sin responder la encuesta de Salud ----------------------------------

na_Base_Agrupada <-
  apply(X = is.na(BASE_EXPLO1),
        MARGIN = 2,
        FUN = sum)

# Al realizar la union de la base de ENCUESTA con la de SALUD por el 
# Identificador encontramos que 22 persoanas que contestaron la ENCUESTA no se 
# encuentran en la base de SALUD, esas 22 personas equivalen al 0,98 % de 
# personas de 15 o mas que se encontraban en la base de ENCUESTA.

# Personas sin responder la ENCUESTA -------------------------------------------

BASE_EXPLO_SALUD_ENCUESTA <-
  left_join(x = SALUD_15_mas, y = ENCUESTA_15_mas, by = "Identificador")
na_Base_Agrupada_SALUD_ENCUESTA <-
  apply(
    X = is.na(BASE_EXPLO_SALUD_ENCUESTA),
    MARGIN = 2,
    FUN = sum
  )

# Al realizar la union de la base de SALUD con ENCUESTA por el Identificador 
# encontramos que 9 personas que contestaron la de SALUD  no se encuentran en 
# la base de ENCUESTA, esas 9 personas equivalen al 0,38 % de personas de 15 o 
# mas que se encontraban en la base de SALUD.

# Inconsistencias datos --------------------------------------------------------

Genero_Embarazos_Hombres <-
  BASE_EXPLO1 %>% group_by (p23embarazo) %>% filter(p7sexo == 1)%>% 
  summarise("Conteo_Embarazos" = n())

# Para la variable p23embarazo tenemos una incosistencia en la respuesta 
# que dieron los hombres donde aparece 2 hombres que marcaron "1" , lo cual no 
# tiene logica que un hombre este embarazado.

conteo_men_15_annos <-
  BASE_EXPLO1 %>%  filter(p6edad<15) %>% summarise("Conteo_<_15_annos" = n())

# Se confirma que en la base de 15 o mas no se tienen edades menores a 15. 




