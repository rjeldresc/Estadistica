
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

setwd("d:/dev/Estadistica")
getwd()

##Clase 23/abril
library(readxl)
library(openxlsx)

readxl::read_excel()
openxlsx::read.xlsx()

?read_excel

## Clase día 23/abril ####
#base <- readxl::read_excel(file.choose()) #aparece un cuadro de dialogo para buscar el archivo
base <- readxl::read_excel(path = "Base_de_datos_1.xlsx") #Importar y definir base de datos
attach(base) #para reconocer por el nombre las columnas importadas
base$Bienes <- as.factor(base$Bienes)
base$Solicitud <- as.factor(base$Solicitud)
base$Sucursal <- as.factor(base$Sucursal)
base$Sexo <- as.factor(base$Sexo)
head(base)
tail(base)

names(base)
colnames(base)
summary(base$Ingreso_Anual)

quintiles <- quantile(base$Ingreso_Anual, probs = seq(0, 1, by = 0.2))
quintiles
boxplot(base$Ingreso_Anual, horizontal = T)

# Calcular los valores de los indicadores
stats <- fivenum(base$Ingreso_Anual)
min <- stats[1]
q1 <- stats[2]
median <- stats[3]
q3 <- stats[4]
max <- stats[5]

# Añadir los valores de los indicadores al gráfico
text(min, 1, labels = round(min, 2), pos = 3)
text(q1, 1, labels = round(q1, 2), pos = 3)
text(median, 1, labels = round(median, 2), pos = 3)
text(q3, 1, labels = round(q3, 2), pos = 3)
text(max, 1, labels = round(max, 2), pos = 3)


hist(x = base$Ingreso_Anual)

Ingreso <- base$Ingreso_Anual

## exportar datos
write.table(x = base, file = "base.csv", sep = ";" )
write.csv(x = base, file = "base_csv.csv")
write.csv2(x = base, file = "base_csv2.csv")

?write.table
?write.csv2


## exportar en excel
library(openxlsx)
write.xlsx(base, "base.xlsx")

base$Genero <- ifelse(base$Sexo == 1, "Hombre", "Mujer")

#tablas de frecuencia
#funcion table()

#tablas ####

table(base$Bienes)

#tabla de doble entrada
tabla_1 <- table(base$Bienes, base$Sucursal) #por columna, luego filas
tabla_1
addmargins(tabla_1) # agrega totales hacia abajo, y hacia el lado derecho (es una suma , como un total por fila y columna)
addmargins(tabla_1, margin = 1) # totales solo por columna
addmargins(tabla_1, margin = 2) # totales solo por fila

prop.table(tabla_1) #entrega las proporciones totales, respecto a los 598
prop.table(tabla_1 , margin = 1) #entrega las proporciones por filas
prop.table(tabla_1 , margin = 2) #entrega las proporciones por columnas


?prop.table
?addmargins

round(prop.table(tabla_1)*100,1)
