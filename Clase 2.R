# Repaso ####
library(readxl) # Carga el paquete readxl

# Antes de importar una tabla, debemos definir nuestro directorio
setwd("d:/dev/Estadistica")
# Otra opcion; pestaña Session, Set Working Directory, Choose Directory.

# Importamos una base de datos de excel a R
base <- read_excel("Bases de datos/Base_de_datos_1.xlsx")

head(base) # Muestra las primeras filas
tail(base) # Muestra las ultimas filas
names(base) # Entrega los nombres de cada columna (variables) de la base de datos
summary(base) # Realiza un resumen estadistico rapido por cada columna (variable)

# Estadisticos de centro ####

# Promedio

mean(base$Ingreso_Anual) # Promedio del ingreso anual

# Mediana

median(base$Ingreso_Anual)

# Moda
# Para cacular la moda en necesario el paquete modeest
# install.packages("modeest")
library(modeest)

mfv(base$Edad)

# Estadisticos de posicion ####

# Cuartiles y percentiles
quantile(base$Ingreso_Anual)

quantile(base$Ingreso_Anual, probs = 0.28)
quantile(base$Ingreso_Anual, probs = c(0.39, 0.5))

quantile(base$Ingreso_Anual, prob = seq(0, 1, length = 101))
# En prob = podemos especificar que percentiles especificos queremos
# Con seq(0, 1, length = 101) generamos una lista con los numeros del 0 al 100 con espacio de 1 unidad

# Estadisticos de variabilidad ####

# Rango
range(base$Ingreso_Anual) # me entrega min y max

# RIC
IQR(base$Ingreso_Anual) # Q3 - Q1

# Desviacion estandar
sd(base$Ingreso_Anual)

# Varianza
var(base$Ingreso_Anual)

# Coeficiente de variacion
sd(base$Ingreso_Anual)/mean(base$Ingreso_Anual) # interpretación relativa del grado de variabilidad
# cuanto es en relacion la dispersion de la media

# Estadisticos de forma ####

# Para estos es necesario el paquete moments
# install.packages("moments")
library(moments)

# Coeficiente de asimetria
skewness(base$Ingreso_Anual)

# Curtosis
kurtosis(base$Ingreso_Anual)

#  Tablas con resumen  ####
library(dplyr)

tabla3 <- base %>% 
    group_by(Sucursal) %>% 
    summarise(Promedio_ingreso = mean(Ingreso_Anual)) 
#el operador  %>% permite concatenar operaciones, summarise crea una nueva columna
#group_by() agrupa un conjunto de filas seleccionado en un conjunto de filas de resumen.

# Graficos ####

# Histogramas
hist(base$Edad) # Genera el histograma de la Edad

# Grafico de cajas
boxplot(base$Ingreso_Anual) # Entrega el grafico de caja del Ingreso_Anual
boxplot(base$Ingreso_Anual ~ base$Sexo_recodificado) # Entrega el grafico de caja del ingreso por sexo

# Violin
# install.packages("vioplot")
library(vioplot)
vioplot(base$Ingreso_Anual ~ base$Bienes)

# Grafico circular
pie(table(base$Sucursal)) # Entrega el grafico circular de personas por sucursal

# Grafico de barras
barplot(table(base$Bienes, base$Sucursal), legend.text = TRUE)

# Grafico de barras apilado en proporcion
tabla_bienes <- table(base$Bienes, base$Sucursal)
barplot(prop.table(tabla_bienes, margin = 2))

# Graficos de dispercion
plot(base$Edad, base$Ingreso_Anual)
