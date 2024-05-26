#script clase 29-04

setwd("D:/dev/Estadistica/Clase 29-04")

base <- readxl::read_excel("Sucursales.xlsx", sheet = "Datos") #Importar y definir base de datos

head(base)
attach(base) #para reconocer por el nombre las columanas importadas

table(Sucursal)->t1 
round(prop.table(t1)*100,1)-> t2 # redondear con 1 decimal
cbind(t1,t2) -> tabla1   # muestra las dos columnas
colnames(tabla1) <- c("Frecuencia", "Porcentaje")
addmargins(tabla1,margin=1)


# ¿cuantos clientes hay por sucursal y sexo?
table(Sucursal,Sexo) -> t3
addmargins(t3,margin=1)  # suma vertical
addmargins(t3, margin=2) # suma horizontal
addmargins(t3)


# ¿cuál es la distribución porcentual del sexo, por sucursal?
round(prop.table(t3,2)*100,1) ->  tc # % 100% por columna
addmargins(tc,1)

