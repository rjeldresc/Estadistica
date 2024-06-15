##############  PAQUETES  ############## 
#install.packages("readxl")
#install.packages("ggplot2") 
library(ggplot2) 
#install.packages("ggalluvial") #Stankey

setwd("d:/dev/Estadistica/")
#setwd("C:/Users/caroh/Documents/RStudio/Clases/Clase 3") #Definición de directorio de trabajo

base <- readxl::read_excel("Base_Taller3.xlsx", sheet = "Parte7") #Importar y definir base de datos
attach(base) #para reconocer por el nombre las columanas importadas


##############  DIAGRAMA DE STANKEY  ##############
library(ggalluvial)
ggplot(base,aes(axis1 = Sexo, axis2 = Person, axis3 = Area, y = frec)) +
  geom_alluvium(aes(fill = Person)) +
  geom_stratum() +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Encuesta", "Respuesta"),
                   expand = c(0.10, 0.05)) +
  theme_void()

