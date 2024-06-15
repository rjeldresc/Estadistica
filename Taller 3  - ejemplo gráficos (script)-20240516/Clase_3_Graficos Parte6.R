##############  TALLO HOJA  ############## 

install.packages("aplpack")
library(aplpack)
datos1 <- c(12, 15, 16, 21, 24, 29, 30, 31, 32, 33, 
            45, 46, 49, 50, 52, 58, 60, 63, 64, 68)
datos2 <- c(12, 15, 16, 22, 24, 29, 29, 31, 32, 33, 
            45, 46, 49, 50, 52, 58, 59, 63, 64, 65,73,75)

#Tallo y hoja simple
stem(datos1, scale = 2)  
stem(datos2, scale = 2)     

#Tallo y hoja comparativo
stem.leaf.backback(datos1, datos2)  # tallo y hoja comparativo # simétricos
stem.leaf.backback(datos1, datos2, back.to.back = FALSE) # uno al lado del otro
