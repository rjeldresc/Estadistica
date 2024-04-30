
1+1
2*2

a = 1
b<- 1

d<-"1"

pi_value <- pi
print(pi_value)

x<-1
print(x)

complejo <- 2+4i
print(class(complejo))


#crear una tabla de datos
datos<-data.frame(
  Tarea = c("A", "B", "C", "D"),
  Puntuacion = c(75, 92, 88, 65)
)
attach(datos)
View(datos$Tarea)

barplot(Puntuacion)
barplot(Puntuacion, col = "lightblue", main="Puntuacion", xlab = "Tareas", ylab =  "Puntos" , legend.text = row.names(Tarea))

#guardar los datos en un txt
write.table(datos, file = "datos.txt")


#para conexion con sql server
install.packages("odbc")
library(odbc)
# Establecer la conexión
con <- dbConnect(odbc(),
                 Driver = "ODBC Driver 17 for SQL Server",
                 Server = "DESKTOP-NKC9QNV",
                 Database = "bbdd_conversion",
                 UID = "rodrigo",
                 PWD = "enter")

result <- dbGetQuery(con, "SELECT * FROM CampoParametros")

print(result)
dbDisconnect(con)

?cor

# Crear un vector de números
numeros <- c(10, 20, 30, 40, 50)

# Mostrar el vector
print(numeros)

# Crear un vector de caracteres
colores <- c("rojo", "verde", "azul")

# Mostrar el vector de caracteres
print(colores)

# Crear un vector lógico
booleanos <- c(TRUE, FALSE, TRUE, TRUE, FALSE)

# Mostrar el vector lógico
print(booleanos)


# Crear una lista en R
mi_lista <- list(
  nombre = "Juan",
  edad = 30,
  ciudad = "Ciudad de México",
  intereses = c("programación", "estadística", "visualización"),
  puntajes = c(80, 90, 75, 85)
)

# Mostrar la lista
print(mi_lista)

# Crear una matriz en R
matriz <- matrix(
  c(1, 2, 3, 4, 5, 6),  # Datos para la matriz
  nrow = 2,             # Número de filas
  ncol = 3,             # Número de columnas
  byrow = TRUE          # Rellenar por filas (TRUE) o por columnas (FALSE)
)

# Mostrar la matriz
print(matriz)

# Crear un data frame en R
data_frame <- data.frame(
  Nombre = c("Juan", "María", "Carlos"),
  Edad = c(25, 30, 28),
  Ciudad = c("Ciudad de México", "Madrid", "Buenos Aires"),
  Puntaje = c(80, 75, 90)
)

# Mostrar el data frame
print(data_frame)


