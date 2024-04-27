
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

