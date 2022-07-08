library(tidyverse)
library(readxl)
library(psych)
library(knitr)
library(factoextra)

require(dplyr)
require(tibble)
require(FactoMineR)
require(factoextra)
require(gridExtra)
require(mice)
require(corrplot)
require(ggplot2)
require(readxl)
  
library(readxl)
Medicina_Legal <- read_excel("Medicina Legal.xlsx")
# Se realizara con un gráfico para observar si existen datos 
# faltantes en la base de datos con la función (missmap)


library(Amelia)
library(ggplot2)
library(DataExplorer)
# Datos faltantes

options(digits=3)


missmap(Medicina_Legal,col=c("red","yellow")) #Individuos

par(mar=c(0,0,0,0))
md.pattern(Medicina_Legal, rotate.names = T)


# Gráfica con proporción de valores faltantes por variable 
plot_missing(data=Medicina_Legal,title = "Porcentaje de valores perdidos por variables")

# No hay datos faltantes 

Depto_Taller1 <- read_excel("Depto_Taller1.xlsx")

Base_Datos=merge(Medicina_Legal,Depto_Taller1,by = "Departamento")

unique(Base_Datos$Siglas)


class(Base_Datos)
View(Base_Datos)

dim(Medicina_Legal)
dim(Depto_Taller1)


Base_Datos = Base_Datos %>%
  column_to_rownames(var = "Siglas")  


View(Base_Datos)

str(Base_Datos)

Base_Datos$Código= as.character(Base_Datos$Código)
# Quantitative and qualitative variables
Quanti_Var= select_if(Base_Datos, is.numeric)


Quali_var = select_if(Base_Datos, is.character)
head(Quali_var)

table(Quali_var$Regiones)


# DESCRIPTIVA -------------------------------------------------------------
# Cambiar a numero enteros
options(scipen=1)
Descrip<-round(describe(Quanti_Var[,c(2:16)]),0)[,-c(1,2,6,7,13)]
Descrip

colnames(Descrip)<- c('Media','sd','Mediana', 'Min','Max', 'Rango','Asimetria',' Kurtosis')
Descrip

Prueba=rnorm(n = 100,mean = 0,sd = 1)

describe(Prueba)

ggpairs(Quanti_Var[,c(2:16)], aes(alpha = 0.1), lower = list(continuous = wrap("smooth", method = "loess", col = "#5499C7")))





# GRÁFICAS DENSIDADES MULTIPLES -------------------------------------------

nombres=colnames(unique(Quanti_Var[,c(2:16)]))
length(Densi)
for(i in 1:length(Quanti_Var[,c(2:16)])){
  assign(paste0("Gra_",i),
         
         ggplot(Quanti_Var[,c(2:16)], aes_string(x=nombres[i])) +
           geom_histogram(aes(y = ..density..), bins = 7, fill="#5b9ac7", color = "#4b80a6") +
           geom_density())
  
}
windows()
library(gridExtra)
g = grid.arrange(Gra_1, Gra_2, Gra_3, Gra_4, Gra_5, Gra_6,
                 Gra_7, Gra_8, Gra_9, Gra_10, Gra_11, Gra_12,
                 Gra_13, Gra_14, Gra_15, ncol=3, nrow = 5)

# DISTRIBUCIÓN NORMAL MULTIVARIADA ----------------------------------------



# Multivariate normality test + qqnorm
windows()
par(mfrow=c(1,2))
mardia = mvn(Quanti_Var, mvnTest = c("mardia"),
             multivariatePlot = "qq")$multivariateNormality 
Outlier = mvn(Quanti_Var, 
              multivariateOutlierMethod = "quan",
              showOutliers = T) ; Outlier
mardia

royston = mvn(Quanti_Var, mvnTest = c("royston"))$multivariateNormality ; royston
energy = mvn(Quanti_Var, mvnTest = c("energy"))$multivariateNormality ; energy

# Los datos no se distribuyen de Forma Normal


# DETECCIÓN DE DATOS ATIPICOS ---------------------------------------------

Outlier = mvn(Quanti_Var, 
              multivariateOutlierMethod = "quan",
              showOutliers = T) ; Outlier

colnames(unique(Quanti_Var))

library(gridExtra)
grid.arrange(mardia,Outlier, ncol=2, nrow = 1)

# Tasa por variables (por cada 100.000 habitantes) 



Quanti_Var_Tasa=mutate(Quanti_Var,
                       
       Densi_Pobl = Quanti_Var$Población/Quanti_Var$Superficie,
                       
       Tasa_Homi = (Quanti_Var$Homicidios/sum(Quanti_Var$Población))*100000,
       
       Tasa_Suici = (Quanti_Var$Suicidios/sum(Quanti_Var$Población))*100000,
       
       Tasa_MTra = (Quanti_Var$MTransporte/sum(Quanti_Var$Población))*100000,
       
       Tasa_MAcci = (Quanti_Var$MAccidentales/sum(Quanti_Var$Población))*100000,
       
       Tasa_Interper = (Quanti_Var$Interpersonal/sum(Quanti_Var$Población))*100000,
       
       Tasa_Exame = (Quanti_Var$Exámenes/sum(Quanti_Var$Población))*100000,
       
       Tasa_Intrafa = (Quanti_Var$Intrafamiliar/sum(Quanti_Var$Población))*100000,
       
       Tasa_LTrans = (Quanti_Var$LTransporte/sum(Quanti_Var$Población))*100000,
       
       Tasa_LAccide = (Quanti_Var$LAccidentales/sum(Quanti_Var$Población))*100000,
       
       Tasa_Desap = (Quanti_Var$Desaparecidos/sum(Quanti_Var$Población))*100000,
       
       Tasa_Nacimi = (Quanti_Var$Nacimientos/sum(Quanti_Var$Población))*100000,
       
       Tasa_Defun = (Quanti_Var$Defunciones/sum(Quanti_Var$Población))*100000,
       
       Tasa_Feta = (Quanti_Var$Fetales/sum(Quanti_Var$Población))*100000,
       
       Tasa_Cabe = (Quanti_Var$Cabecera/sum(Quanti_Var$Población))*100000
       

)




Quanti_Var_Tasa=Quanti_Var_Tasa[, -c(1:16)]


# Densidad empirica

Quanti_Var_Tasa$Regiones= Base_Datos$Regiones

ggplot(Quanti_Var_Tasa, aes(x=Tasa_Homi, fill=Regiones)) +
  geom_density(alpha=0.6) +
  scale_fill_brewer(palette = 1)


# GRÁFICAS DENSIDADES MULTIPLES -------------------------------------------

nombres=colnames(unique(Quanti_Var_Tasa[,c(4:17)]))
length(Densi)
for(i in 1:length(Quanti_Var_Tasa[,c(4:7)])){
  assign(paste0("Gra_",i),ggplot(Quanti_Var_Tasa[,c(4:18)], aes_string(x=nombres[i], fill="Regiones")) +
           geom_density(alpha=0.6) +
           scale_fill_brewer(palette = 1))
  
}
windows()
library(gridExtra)
g = grid.arrange(Gra_1, Gra_2, Gra_3, Gra_4, ncol=2, nrow = 2)



nombres=colnames(unique(Quanti_Var_Tasa[,c(8:12)]))
length(Densi)
for(i in 1:length(Quanti_Var_Tasa[,c(8:11)])){
  assign(paste0("Gra_",i),ggplot(Quanti_Var_Tasa[,c(4:18)], aes_string(x=nombres[i], fill="Regiones")) +
           geom_density(alpha=0.6) +
           scale_fill_brewer(palette = 1))
  
}
windows()
library(gridExtra)
g = grid.arrange(Gra_1, Gra_2, Gra_3, Gra_4, ncol=2, nrow = 2)

windows()
nombres=colnames(unique(Quanti_Var_Tasa[,c(12:15)]))
for(i in 1:length(Quanti_Var_Tasa[,c(12:15)])){
  assign(paste0("Gra_",i),ggplot(Quanti_Var_Tasa[,c(4:18)], aes_string(x=nombres[i], fill="Regiones")) +
           geom_density(alpha=0.6) +
           scale_fill_brewer(palette = 1))
  
}
windows()
library(gridExtra)
g = grid.arrange(Gra_1, Gra_2, Gra_3, Gra_4, ncol=2, nrow = 2)

nombres=colnames(unique(Quanti_Var_Tasa[,c(16:17)]))
for(i in 1:length(Quanti_Var_Tasa[,c(16:17)])){
  assign(paste0("Gra_",i),ggplot(Quanti_Var_Tasa[,c(4:18)], aes_string(x=nombres[i], fill="Regiones")) +
           geom_density(alpha=0.6) +
           scale_fill_brewer(palette = 1))
  
}
windows()
library(gridExtra)
g = grid.arrange(Gra_1, Gra_2, ncol=2, nrow = 1)

# Diagrama de dispersión --------------------------------------------------



ggpairs(Quanti_Var_Tasa[,-c(1,2)], aes(alpha = 0.1), lower = list(continuous = wrap("smooth", method = "loess", col = "#5499C7")))


# Histograma y densidad empirica ------------------------------------------

Densi=Quanti_Var_Tasa[,-c(1,2)]
dim(Densi)
nombres=colnames(unique(Densi))
length(Densi)
for(i in 1:length(Densi)){
  assign(paste0("Gra_",i),
         
ggplot(Densi, aes_string(x=nombres[i])) +
geom_histogram(aes(y = ..density..), bins = 7, fill="#5b9ac7", color = "#4b80a6") +
geom_density())

}
windows()
library(gridExtra)
g = grid.arrange(Gra_1, Gra_2, Gra_3, Gra_4, Gra_5, Gra_6,
                 Gra_7, Gra_8, Gra_9, Gra_10, Gra_11, Gra_12,
                 Gra_13, Gra_14, Gra_15, ncol=3, nrow = 5)

###############################################################################
#                                                                             #
#                         ANÁLISIS DE COMPONENTES                             #
#                               PRINCIPALES                                   #
#                                                                             #
###############################################################################


# CORRELACIONES -----------------------------------------------------------


R = cor(Quanti_Var_Tasa) ; R

# Correlations
corrplot(R, method = "ellipse", type="lower",  
         diag = F, tl.col = 1, order = "original", addCoef.col = T, number.cex = .5)

# Principal Component Analysis

Quanti_Var_Tasa$Regiones= as.factor(Quanti_Var_Tasa$Regiones)
class(Quanti_Var_Tasa$Regiones)
pc1 = PCA(Quanti_Var_Tasa, quali.sup = 18, ncp = 4,  graph = T)
pc$eig


# Scree plot
windows()
nc = 6
fviz_screeplot(pc1, ncp = nc, main = "", linetype = "dashed") +
  geom_text(aes(y = pc1$eig[1:nc,2] + 3),
            label = round(pc1$eig[1:nc,2], 1))+
  geom_text(aes(y = 3),label = paste("(",round(pc1$eig[1:nc,1], 1),")", sep = ""), col = "white", size = 2.5)+
  geom_line(aes(y = pc1$eig[1:nc,3]), linetype = "dashed", col = 2) +
  geom_point(aes(y = pc1$eig[1:nc,3]), pch = 20, col = 2, size = 2) +
  geom_text(aes(y = pc1$eig[1:nc,3] + 3),
            label = round(pc1$eig[1:nc,3], 1), col = 2)

# First Principal Plane
windows()
fviz_pca_var(pc1, axes = c(1,2), repel = T)

# Principal Planes
axes = combn(4, 2) ; axes
for(i in 1:ncol(axes)){
  assign(paste0("CP_",i),
         fviz_pca_var(pc1, axes = axes[,i], repel = T))
}
windows()
g = grid.arrange(CP_1, CP_2, CP_3, CP_4, CP_5, CP_6, ncol=3, nrow = 2)

# Correlations circle (Principal Planes)
round(pc1$var$coord, 2)

# Square Cosines
windows()
cos2 = round(pc1$var$cos2, 2) ; cos2
fviz_pca_var(pc1, axes = 1:2, col.var = "cos2", repel = T) +
  scale_color_gradient2(low="red", mid="blue", high="black", midpoint=.5)

# Contributions
windows()
contr = round(pc1$var$contrib,1) ; contr
fviz_contrib(pc1, axes = 1:2, choice = "var")


# Contributions
windows()

for(i in 1:4){
  assign(paste0("c",i), fviz_contrib(pc1, axes = i, choice = "var"))
}

grid.arrange(c1, c2, c3, c4, ncol=2, nrow = 2)

windows()

fviz_pca_var(pc1, axes = 1:2, col.var = "contrib", repel = T)+
  scale_color_gradient2(low="red", mid="blue", high="black", midpoint=10)

for(i in 1:ncol(axes)){
  assign(paste0("cc",i),
         fviz_pca_var(pc1, axes = axes[,i], col.var = "contrib", repel = T)+
           scale_color_gradient2(low="red", mid="blue", high="black", midpoint=10))
}
windows()
grid.arrange(cc1, cc2, cc3, cc4, cc5, cc6, ncol=3, nrow = 2)

windows()
fviz_pca_ind(pc1, labelsize = 4, repel = T, pointsize = 1,habillage=Quanti_Var_Tasa$Regiones,addEllipses=TRUE) +
  scale_color_brewer(palette = "Set1")

# Simultaneous representation of individuals and variables

windows()
fviz_pca_biplot(pc1, labelsize = 4, repel = T, label =c("ind","var"),col.var = 1, pointsize = 1, habillage=Quanti_Var_Tasa$Regiones,addEllipses=TRUE) +
  scale_color_brewer(palette = "Set1")
