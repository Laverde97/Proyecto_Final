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

###############################################################################
#                                                                             #
#                              REALIZAMOS ANÁLISIS                            #
#                                  DESCRIPTIVO                                #
#                                                                             #
###############################################################################

# Eliminamos la columna "Identificador"
Base_Descri <- select(BASE_EXPLO1, -"Identificador")

# Cambiamos las etiquetas de las variables (ENCUESTA) --------------------------------

# p7sexo

Base_Descri$p7sexo <- factor(Base_Descri$p7sexo, labels = c("Masculino", "Femenino"))

#p14sgsss

Base_Descri$p14sgsss <- factor(Base_Descri$p14sgsss, labels = c("Contributivo", "Subsidiado", "Vinculado", "Especial", "NS/NR"))

# p31usaamalgama

Base_Descri$p31usaamalgama <- factor(Base_Descri$p31usaamalgama, labels = c("Si", "No",  "NS/NR"))

# p32amalgamacasa

Base_Descri$p32amalgamacasa <- factor(Base_Descri$p32amalgamacasa, labels = c("Si", "No",  "NS/NR"))

# p33almacenahg

Base_Descri$p33almacenahg <- factor(Base_Descri$p33almacenahg, labels = c("Si", "No",  "NS/NR"))

# p35guardatoxicas

Base_Descri$p35guardatoxicas <- factor(Base_Descri$p35guardatoxicas, labels = c("Si", "No",  "NS/NR"))

# p36manipulatoxicas

Base_Descri$p36manipulatoxicas <- factor(Base_Descri$p36manipulatoxicas, labels = c("Si", "No",  "NS/NR"))

# p37cercafumigaciones

Base_Descri$p37cercafumigaciones <- factor(Base_Descri$p37cercafumigaciones, labels = c("Si", "No",  "NS/NR"))

# p38zonaproduccion
Base_Descri$p38zonaproduccion <- factor(Base_Descri$p38zonaproduccion, labels = c("Si", "No",  "NS/NR"))


# Cambiamos las etiquetas de las variables (SALUD) --------------------------------

# p12cancerpulmon

Base_Descri$p12cancerpulmon <- factor(Base_Descri$p12cancerpulmon, labels = c("Si", "No",  "NS/NR"))

# p13cancervejiga

Base_Descri$p13cancervejiga <- factor(Base_Descri$p13cancervejiga, labels = c( "No",  "NS/NR"))

# p14cancerpiel

Base_Descri$p14cancerpiel <- factor(Base_Descri$p14cancerpiel, labels = c("Si", "No",  "NS/NR"))

# p15cancerriñon

Base_Descri$p15cancerriñon <- factor(Base_Descri$p15cancerriñon, labels = c("No",  "NS/NR"))

# p16cancerprostata

Base_Descri$p16cancerprostata <- factor(Base_Descri$p16cancerprostata, labels = c("Si", "No",  "NS/NR"))

# p17cancerhigado

Base_Descri$p17cancerhigado <- factor(Base_Descri$p17cancerhigado, labels = c("Si", "No",  "NS/NR"))

# p20fractura

Base_Descri$p20fractura <- factor(Base_Descri$p20fractura, labels = c("Traumática", "No_Traumática"))

# p21osteoporosis 

Base_Descri$p21osteoporosis <- factor(Base_Descri$p21osteoporosis, labels = c("Si", "No",  "NS/NR"))

# p22conciencia

Base_Descri$p22conciencia <- factor(Base_Descri$p22conciencia, labels = c("Si", "No",  "NS/NR"))

# p23embarazo
Base_Descri$p23embarazo <- factor(Base_Descri$p23embarazo, labels = c("Si", "No"))



# Identificar datos atipicos  ---------------------------------------------

# BoxPlot Edad Vs (Encuesta) ---------------------------------------------------


# Boxplot de la variable Edad

Conf3x2 = matrix(c(1:9), nrow =  3, byrow=TRUE)

layout(Conf3x2)

boxplot(
  Base_Descri$p6edad,
  horizontal = FALSE,
  ylab = "Edad",
  col = "aliceblue",
  main = "BoxPlot de Edad"
)


boxplot(
  Base_Descri$p6edad ~ Base_Descri$p7sexo,
  xlab = "Género",
  ylab = "Edad",
  main = "BoxPlot de Edad vs Género",
  col = rainbow(2)
)


boxplot(
  Base_Descri$p6edad ~ Base_Descri$p14sgsss,
  xlab = "Tipo de usuario en el SGSSS",
  ylab = "Edad",
  main = "BoxPlot de Edad vs Tipo de usuario en el SGSSS",
  col = rainbow(2)
)


boxplot(
  Base_Descri$p6edad ~ Base_Descri$p32amalgamacasa,
  xlab = "Usted o algún miembro de su familia que viva con usted realiza quema de amalgama en casa",
  ylab = "Edad",
  main = "BoxPlot de Edad vs Quema de amalgama",
  col = rainbow(2)
)


boxplot(
  Base_Descri$p6edad ~ Base_Descri$p33almacenahg,
  xlab = "Usted o algún miembro de su familia que viva con usted  guarda mercurio o azogue en casa",
  ylab = "Edad",
  main = "BoxPlot de Edad vs Mercurio",
  col = rainbow(2)
)


boxplot(
  Base_Descri$p6edad ~ Base_Descri$p35guardatoxicas,
  xlab = "Usted o algún miembro de su familia que viva con usted  guarda mercurio o azogue en casa",
  ylab = "Edad",
  main = "BoxPlot de Edad vs Guarda sustancias Tóxicas",
  col = rainbow(2)
)


boxplot(
  Base_Descri$p6edad ~ Base_Descri$p36manipulatoxicas,
  xlab = "Usted está en contacto o manipula alguna sustancia tóxica o peligrosa (cianuro, plaguicidas  o venenos, u otra)",
  ylab = "Edad",
  main = "BoxPlot de Edad vs Manipula sustancias Tóxicas",
  col = rainbow(2)
)


boxplot(
  Base_Descri$p6edad ~ Base_Descri$p37cercafumigaciones,
  xlab = "Su casa está cerca de zonas de cultivo o donde se realicen fumigaciones?",
  ylab = "Edad",
  main = "BoxPlot de Edad vs ¿Su casa está cerca de zonas de cultivo o donde se realicen fumigaciones?",
  col = rainbow(2)
)


boxplot(
  Base_Descri$p6edad ~ Base_Descri$p38zonaproduccion,
  xlab = "Su casa está cerca de zonas o industrias de procesamiento o producción de sustancias tóxicas",
  ylab = "Edad",
  main = "BoxPlot de Edad vs Procesamiento sustancias Tóxicas",
  col = rainbow(2)
)

# BoxPlot Edad Vs (salud) ---------------------------------------------------

Conf3x2 = matrix(c(1:4), nrow =  2, byrow=TRUE)

layout(Conf3x2)

boxplot(
  Base_Descri$p6edad ~ Base_Descri$p12cancerpulmon,
  xlab = " ¿Le han diagnosticado cáncer de pulmón?",
  ylab = "Edad",
  main = "BoxPlot de Edad vs  ¿Le han diagnosticado cáncer de pulmón?",
  col = rainbow(3)
)

boxplot(
  Base_Descri$p6edad ~ Base_Descri$p14cancerpiel,
  xlab = "¿Le han diagnosticado cáncer  en la piel?",
  ylab = "Edad",
  main = "BoxPlot de Edad vs  ¿Le han diagnosticado cáncer  en la piel? ",
  col = rainbow(3)
)

boxplot(
  Base_Descri$p6edad ~ Base_Descri$p16cancerprostata,
  xlab = "¿Le han diagnosticado cáncer de próstata?",
  ylab = "Edad",
  main = "BoxPlot de Edad vs  ¿Le han diagnosticado cáncer de próstata?",
  col = rainbow(3)
)


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



Conf2x2 = matrix(c(1:4), nrow =  2, byrow=TRUE)

layout(Conf2x2)

boxplot(
  Base_Descri$p6edad ~ Base_Descri$p20fractura,
  xlab = "Si ha sufrido fracturas, la fractura fue:",
  ylab = "Edad",
  main = "BoxPlot de Edad vs   Si ha sufrido fracturas",
  col = rainbow(3)
)


boxplot(
  Base_Descri$p6edad ~ Base_Descri$p21osteoporosis,
  xlab = "¿Le han diagnosticado osteoporosis?",
  ylab = "Edad",
  main = "BoxPlot de Edad vs ¿Le han diagnosticado osteoporosis?",
  col = rainbow(3)
)


boxplot(
  Base_Descri$p6edad ~ Base_Descri$p22conciencia,
  xlab = "¿Ha presentado algún trauma severo con perdida de la conciencia?",
  ylab = "Edad",
  main = "BoxPlot de Edad vs ¿Ha presentado algún trauma severo con perdida de la conciencia?",
  col = rainbow(3)
)

boxplot(
  Base_Descri$p6edad ~ Base_Descri$p23embarazo,
  xlab = "¿Actualmente se encuentra embarazada?",
  ylab = "Edad",
  main = "BoxPlot de Edad vs ¿Actualmente se encuentra embarazada?",
  col = rainbow(3)
)


library(ggplot2)
p <- ggplot(Base_Descri, aes(p7sexo, p6edad, fill = p21osteoporosis))
p + geom_boxplot()



ftable(Base_Descri$p7sexo,Base_Descri$p23embarazo)

ftable(Base_Descri$p7sexo,Base_Descri$p21osteoporosis)

