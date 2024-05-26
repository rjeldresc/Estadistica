##############  PAQUETES  ############## 
#install.packages("readxl") 
#install.packages("fmsb") #RADAL
library(fmsb)

setwd("d:/dev/Estadistica/")
#setwd("C:/Users/caroh/Documents/RStudio/Clases/Clase 3") #Definición de directorio de trabajo

base <- readxl::read_excel("Base_Taller3.xlsx", sheet = "Parte4") #Importar y definir base de datos
attach(base) #para reconocer por el nombre las columanas importadas


##############  RADAL  ############## 
Grupo <- c("Mat", "Leng","Hist","Ingl")

radarchart(base,title="Notas", plwd = 1, col=1:4)
legend("topright",legend = paste(Grupo),bty = "n", pch = 4,
       text.col = "grey25", pt.cex = 4, col=1:4)    # pt.cex = es el tamaño de circulo en la legenda

radarchart(base, vlcex= 2, title="Notas", col=1:4)   # vlcex cambia tamaño de etiqueta circulo
legend("topright",legend = paste(Grupo), bty = "n", pch = 20,
       text.col = "grey25", pt.cex = 1,col=1:4)  

