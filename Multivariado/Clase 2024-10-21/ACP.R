#### COMPONENTES PRINCIPALES ####
setwd("D:/dev/Estadistica/Multivariado/Clase 2024-10-21/")
###################
#Ejemplo 1 
# Posicionamiento de países de la UE frente al cumplimiento de las condiciones de Maastricht
pais<-readxl::read_excel("datosM.xlsx",sheet="EU97")

# Objetivo:
# Evaluar la situación de cada país de la UE en cuanto a las previsiones de entrada en el Mercado de la UE 
# y la similitud o disimilitud entre ellos. 
# Para lo anterior, consideramos las cuatro variables relacionadas con el cumplimiento de las condiciones de
# Maastricht: Inflación, deuda, déficit y crecimiento

cor(pais[,2:5])  # correlacón (no incluir la primera columna) , la primera columna es el nombre de pais
ajuste <- princomp(pais[,2:5], cor=TRUE) #genera los componentes principales
summary(ajuste) #muestra los componentes principales
loadings(ajuste) #vector de cargas
ajuste$scores
biplot(ajuste)
text(ajuste$scores[,1],ajuste$scores[,2], pais$País, cex=.7)


###################
# Ejemplo 2 
notas <- readxl::read_excel("datosM.xlsx",sheet="NOTAS")
head(notas)

notas <- data.frame(notas[, 2:9], row.names = notas$Alumno)
head(notas)

# quitamos la 1era y a 10, y asignamos "row.names"
notas <- data.frame(notas[, c(-1,-10)], row.names = notas$Alumno)
head(notas)

summary(notas)

# ACP: prcomp
acp_notas <- prcomp(notas, scale = TRUE)
summary(acp_notas)
plot(acp_notas, type = "lines")

# vemos las cargas
acp_notas$rotation 

## Componente 1: Rama humanista -> positivo + alto -> buena nota    
## Componente 2: Rama cientifica -> negativo + alto -> buena nota

acp_notas$x # scores
biplot(acp_notas)

# otros gráficos
#install.packages("factoextra")
library(factoextra)
# Contribucion de variables en componentes
fviz_pca_var(acp_notas, col.var = "contrib",
             axes = c(1,2))

# Scores de alumnos en las dimensiones
fviz_pca_ind(acp_notas, col.ind = "cos2",
             axes = c(1,2))


###################
# Ejemplo 3

records <- readxl::read_excel("datosM.xlsx",sheet="records")
# como tenemos nombres de los países, filtramos
records[ , -1] # el análisis se requieren "datos numéricos"

records <- data.frame(records[, -1], row.names = records$Países)

summary(records) 

# graficos multiples para describir
par(mfrow = c(2,5), bty = "n")
for(i in 1:ncol(records)){
  densidad <- density(records[,i])
  plot(densidad, main = paste0("Distribucion ", names(records)[i])) 
}
par(mfrow=c(1,1))  #volvemos a un gráfico por pantalla

cor_records <- cor(records)#correlaciones
round(cor_records, 2)

# ACP: princomp
acp_record <- princomp(records, cor = TRUE)

summary(acp_record)
plot(acp_record, type = "lines") 

loadings(acp_record) #cargas

## Componente 1: evalua ambas carreras
## Componente 2: carreras cortas, "rapidez", 
##               entre mal alto y positivo son mas lentos
## Componente 3: carreras largas, "resistencia", 
##               entre mas positivo menos resistente en maratones
##               entre mas negativo menos resistente en carreras largas

## scores de los paises por dimension 1, 2 y 3
round(acp_record$scores[,c(1,2,3)], 2) # Scores 
biplot(acp_record) # es imposible de interpretar claramente

## grafico bonito de contribucion var
fviz_screeplot(acp_record, 
               addlabels = TRUE, 
               ylim = c(0,100)) 

## score de de las columnas por dimension 2 y 3
fviz_pca_var(pca, 
             col.var = "contrib", 
             axes = c(2,3),
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             ggtheme = theme_minimal())

## score de de los paises por dimension 2 y 3
fviz_pca_ind(acp_record, col.ind = "cos2", 
             axes = c(2,3),
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             ggtheme = theme_minimal())

