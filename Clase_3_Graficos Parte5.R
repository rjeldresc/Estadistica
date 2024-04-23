##############  PAQUETES  ############## 
#install.packages("readxl")
#install.packages("ggplot2") 
library(ggplot2) 
#install.packages("ggthemes")
library(ggthemes) 
#install.packages("MASS") #lineas paralelas

#install.packages("aplpack") #tallo y hoja
library(aplpack)


setwd("C:/Users/caroh/Documents/RStudio/Clases/Clase 3") #Definición de directorio de trabajo

base <- readxl::read_excel("Base_Taller3.xlsx", sheet = "Parte5") #Importar y definir base de datos
attach(base) #para reconocer por el nombre las columanas importadas

base$I1=as.numeric(base$I1) #para cambiar naturaleza delobjeto
base$I3=as.numeric(base$I3)  # no incluye datos no numéricos
attach(base) #para reconocer por el nombre las columanas importadas


##############  LINEAS PARALELAS  ############## 
datos1 <- base[, 3:6]
library(MASS)
parcoord(datos1)
parcoord(datos1, var.label = TRUE)    #agrega mínimos y máximos 
cols <- as.factor(Sexo) #transforma a una nueva categoria para poder clasificar   
parcoord(datos1, var.label = TRUE, col=cols)   # solo? dos colores (sexo)
legend("topleft", legend = levels(as.factor(Sexo)))

datos2 <- base[, 3:6]
parcoord(datos2)
parcoord(datos2, var.label = TRUE)    #agrega mínimos y máximos 
cols <- as.factor(Num_alumno)   
parcoord(datos2, var.label = TRUE, col=cols) # un color por alumno


##############  DE PUNTO APILADOS  ############## 
ggplot(base, aes(x =I2)) +geom_dotplot()
ggplot(base, aes(x =I2)) +geom_dotplot(fill = "steelblue")

