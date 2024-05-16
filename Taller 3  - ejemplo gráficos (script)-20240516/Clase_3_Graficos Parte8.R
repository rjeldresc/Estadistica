##############  PAQUETES  ############## 
#install.packages("readxl")
#install.packages("ggplot2") 
library(ggplot2) 
#install.packages("ggmosaic")
library(ggmosaic)

setwd("d:/dev/Estadistica/")
#setwd("C:/Users/caroh/Documents/RStudio/Clases/Clase 3") #Definición de directorio de trabajo

base <- readxl::read_excel("Base_Taller3.xlsx", sheet = "Parte8a") #Importar y definir base de datos
attach(base) #para reconocer por el nombre las columanas importadas


##############  GRÁFICO MARIMEKKO (MOSAICO)  ##############
ggplot(base) +
  geom_mosaic(aes(x = product(Area,Sexo)), fill = "lightblue",color = "black")

ggplot(base) +
  geom_mosaic(aes(x = product(Area, Sexo), fill = Area))

#otro ejemplo
base2 <- readxl::read_excel("Base_Taller3.xlsx", sheet = "Parte8b")
attach(base2) 

ggplot(base2) +
  geom_mosaic(aes(x = product(Afecta, Región), fill = Afecta))

