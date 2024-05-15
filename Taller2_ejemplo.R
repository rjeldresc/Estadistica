#Clase 17-abril

setwd("d:/dev/Estadistica")

# Crear una tabla de datos
datos <- data.frame(
  Tarea = c("A", "B", "C", "D"),
  Puntuacion = c(75, 92, 88, 65)
)

View(datos)
#rm(datos)
attach(datos)

# Mostrar la tabla de datos en la consola
datos
print(datos)
class(datos$Puntuacion)
mean(datos$Puntuacion)

# Crear un gráfico 
barplot(Puntuacion)



#guardar datos txt
write.table(datos, file = "Ejemplo.txt")

#guardar datos excel
install.packages("openxlsx") 
library(openxlsx)
write.xlsx(datos, "Ejemplo.xlsx") 

#para guardar plot

# Configurar el archivo PNG
png(filename = "Ejemplo.png")

# Crear el gráfico de barras
bp <- barplot(Puntuacion, 
        names.arg = Tarea,
        col= "lightblue", 
        main="Grafico de barras para Tarea vs Puntuacion", 
        xlab = "Tareas", 
        ylab="Puntuacion",
        legend.text=rownames(Tarea),
        ylim=c(0,100))

# Añadir los valores sobre cada barra
text(x = bp, y = datos$Puntuacion, label = datos$Puntuacion, pos = 3, cex = 0.8, col = "black")

# Cerrar el dispositivo gráfico
dev.off()


