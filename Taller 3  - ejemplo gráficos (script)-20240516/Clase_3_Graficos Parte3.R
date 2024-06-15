##############  PAQUETES  ############## 
#install.packages("readxl") 
#install.packages("ggplot2") 
#install.packages("ggridges")#densidad diferentes niveles


setwd("d:/dev/Estadistica/")
#setwd("C:/Users/caroh/Documents/RStudio/Clases/Clase 3") #Definición de directorio de trabajo

base <- readxl::read_excel("Base_taller3.xlsx", sheet = "Parte3") #Importar y definir base de datos
attach(base) #para reconocer por el nombre las columanas importadas


##############  DENSIDADES EN DIFERENTES NIVELES  ##############
library(ggplot2)
library(ggridges)
ggplot(base, aes(x = Densidad, y = Octanaje)) +
  geom_density_ridges()

ggplot(base, aes(x = Densidad, y = Octanaje, fill = Octanaje)) +
  geom_density_ridges(scale = 3)    # scale separa las densidades
# fill agrega color a cada densidad

ggplot(base, aes(x = Densidad, y = Octanaje, fill= Octanaje)) +
  geom_density_ridges(scale = 0.5, quantile_lines = TRUE,quantiles = 2) # agrega mediana

ggplot(base, aes(x = Densidad, y = Octanaje, fill= Octanaje)) +
  geom_density_ridges(scale = 0.5, quantile_lines = TRUE) # agrega cuartiles

