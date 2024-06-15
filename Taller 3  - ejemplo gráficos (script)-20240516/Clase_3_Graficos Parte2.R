##############  PAQUETES  ############## 
#install.packages("readxl") 
#install.packages("GGally")
setwd("d:/dev/Estadistica/")
#setwd("C:/Users/caroh/Documents/RStudio/Clases/Clase 3") #Definición de directorio de trabajo

base <- readxl::read_excel("Base_Taller3.xlsx", sheet = "Parte2") #Importar y definir base de datos
attach(base) #para reconocer por el nombre las columanas importadas



##############  MATRIZ DE DISPERSIONES  ##############
pairs(base[,c(5,3:4)], pch=19,gap = 1, upper.panel= panel.smooth, lower.panel = NULL) 
# grafico de correlacion con multiples variables
#base , pch simbolo, gap = distancia entre graficos.
#puedes agregar líneas de regresión suavizadas pasando la función panel.smooth al argumento lower.panel 



##############  MATRIZ DE CORRELACIONES  ##############
correlaciones <- cor(base[c(5,3:4)]) #permite calcular el coeficiente de correlación de Pearson, Kendall o Spearman para dos variables cuantitativas.

corrplot::corrplot(correlaciones, type="lower")
corrplot::corrplot(correlaciones, type="lower", method = "square")
corrplot::corrplot(correlaciones, type="lower", method = "number")


##############  TODAS LAS VARIABLES  ##############
GGally::ggpairs(base) 
GGally::ggpairs(base[,c(5,3:4)]) 
