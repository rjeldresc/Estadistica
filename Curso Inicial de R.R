
# curso de R 

setwd("d:/dev/Estadistica")

x<-50
y<-10

x*y

#1000 valores de una distribucion normal
muestra <- rnorm(1000)
muestra

hist(x = muestra)

#para obtener ayuda
?hist

#para cargar el environment desde disco
load(file = "D:/dev/Estadistica/CursoInicial.RData")

sin(180)
sqrt(4)

#tipos de datos
integer
double
logical
character
factor
date

caracter <- "a"
cadena <- "hola que tal"

verdad <- TRUE
falso <- FALSE

verdad & falso
verdad | falso

5 == 4+1

5 != 4

#asignar en los dos sentidos
aux <- codigo....
codigo... ->  aux

#valor infinito
infinito <- Inf * 0 
infinito

class(caracter)
class(NaN)
class(cadena)


typeof(caracter)
typeof(NaN)
typeof(cadena)

no_fecha <- "2024-05-10"
#para dejarlo como objeto fecha
fecha <- as.Date(no_fecha)
class(fecha) #clase Date
typeof(fecha) #sigue siendo un numero double

otra_fecha <- "10-05-2024"
fecha2 <- as.Date(otra_fecha, tryFormats = c("%d-%m-%Y"))
fecha2
class(fecha2)

?as.Date

#inmutabilidad, son asignacion por valor, no por referencia
x<-10
y<-x+1
y

x<-11


#vectores c("", "", "")

?NA  #Not Available
NaN  #not a number

0/0

var1 <- 10
var1 == 5
var1 == NA

NA == NA

#PARA DETECTAR LOS NA

is.na(NA)
is.nan(NaN)
is.na(NaN)
is.nan(NA)

#si se diferencia entre mayusculas y minusculas

b <- 3
B

#vectores

#vector de numeros

v1 <- c( 1,1,2,3,5,8)
v1
class(v1)
v2 <- c(11,12)
v2
class(v2)
pl <- c("RODRIGO", "JELDRES" , "CARRASCO")
pl
class(pl)

vector_logico <- c(T,F,T,T)
class(vector_logico)

#pasa el vector a caracter
x <- "a"
class(c(2, x, 3))

#puede darle nombres a las entradas de un vector
names(pl) #como pl no tiene nombres, retorna NULL

#genero un vector de nombre
Mi_Nombre <- c("RODRIGO", "JELDRES" , "CARRASCO")
names(Mi_Nombre) <- c("nombre", "primer apellido", "segundo apellido") #se le asigna nombre a las columnas
names(Mi_Nombre)

#combinar dos vectores
v3 <- c(v1,v2)
v3
#se le asignan nombres a v1
names(v1) <- c("n1", "n2", "n3", "n4", "n5", "n6")
names(v2) <- c("x1", "x2")

#accediendo a los elementos de un vector
v3[8] #usando la posicion

#para varias posiciones
v3[c(1,4)]

#para 3:5 , retorna la secuencia 3,4,5

v3[c(1:7)]

#accediendo a un elemento con el nombre de la columna
v3["x1"]

#accediendo a varios elementos con el nombre
v3[c("x1", "n1")]

#cambiando un valor de una posicion especifica
v3[4] <- 99
v3

#para cambiar uno de los nombres por "nombre_nuevo"
names(v3[6])
names(v3)[6] <- "nombre_nuevo"

#para cambiar por varias posiciones
names(v3)[c(1,8)] <- c("otro nombre", "mas nombres")

#rescatar por posiciones logicas

v3[c(F,F,T,F,T,F,F,F)] #para acceder a las posiciones 3 y 5

#para traer datos que sean mayores a 10
condicion <- v3 > 10
v3[condicion]


edades <- sample(10:40, 20, replace = TRUE)
edades
esMayordeEdad <- edades >= 18
edades[esMayordeEdad]

?which.max
which.max(v3) #retorna el nombre de la columna y la posicion
v3[which.max(v3)]
max(v3) #retorna solo el valor

#para omitir un elemento en especifico
condicion <- !v3 == 99
condicion
v3[condicion]

#largo de un vector
length(v3)

#para omitir el de la posicion 4
v3[-4]
v3[-c(1,7)] #omite todos los elementos excepto posicion 1 y 7
v3[-which.max(v3)] #consultar el vector sin el máximo valor

#operador %in%
#vector_a %in% vector_b retorna vector del largo del vector a
c(1,2,5,8,1,0) %in% c(3,4,2,8)

#all para ver si todos los elementos de un vector son true
all(c(T,T,F))
?all

all(c(3,2,8) %in% c(3,4,2,8))

#suma de vectores de distinto largo
c <- c(1,2,3) + c(5,4,8,10)
c

d <- c(1,2,3) + 5
d

rep(4,5)
rep(c(1,2,3), 7)

rep(c(1,2,3), length.out = 19)


?seq
seq(from = 1 , to = 10, by = 0.5)
seq( 1 ,  10,  0.5)
seq( 1 ,  10,  length.out = 40)


#distribucion normal
rnorm(1000)
#distribucion uniforme
runif(200)


hist(runif(200))

#R interpreta cada elemento como una ejecucion aparte
sqrt(c(3,4,5,6,7,8))

#para manipular texto
nchar(c("hola", "soy", "yo"))

#substring substr
?substr

substr(c("hola", "soy", "yo"), 2 , 3)

length(c("hola", "soy", "yo")) #retorna la cantidad de "cosas" que tiene el vector


grepl("ala", "palabra") #buscar parte de una palabra si esta contenida en otra


gsub("ala", "olo", "palabra") #reemplaza una cosa por otra

gsub("hola", "xx", c("hola buen día", "como estas", "que tal"))

#https://www.tidytextmining.com/

library(topicmodels)
library(tidytext)
library(ggplot2)
library(dplyr)
library(tidyr)

#matrices

M <- matrix(c(9,10,4,6,7,8), ncol = 3 , nrow = 2 , byrow = T)
M
class(M)

dim(M)[2] #retorna un vector

dim(M)[2] #cantidad de columnas
nrow(M) #cantidad de filas
ncol(M) #Cantidad de columnas

v1 <- 1:5
v2 <- seq(1,2,length.out = 5)
v1;v2

#para unir vectores se usa cbind por columna

C <- cbind(v1,v2)
C

#para unir vectores se usa cbind por fila
D <- rbind(v1,v2)
D

cbind(c(3,5,7,6,7,8), c(10,11,NA, NA,NA, NA))

diag(c(1,2,6,9)) #genera una matriz diagonal

M[2,1] #para obtener un elemento de la matriz

M[2,] #obtiene la segunda fila

M[,3] #obtiene la tercera columna


M[2,2] <- 99

#acceso mediante names
colnames(M) <- c("Columna A", "Columna b", "Columna C")
rownames(M) <- c("FILA 1", "FILA 2")
M

rownames(M) <- paste0("Fila_", 1:2)
colnames(M) <- paste0("Columna_", 1:3)
M

#para acceder mediante nombres
M["Fila_2","Columna_2"]


#quiero obtener dos elementos de una matriz
indices <- rbind(c(F,T,F), c(F,F,T))
indices

M[indices]

#Operatoria con matrices
t(M)

M1 <- matrix(c(1,1,2,5), ncol=2)
M2 <- matrix(c(1,3,5,7), ncol=2)

sumaM <- M1 + M2
sumaM

multiM <- M1 %*% M2
multiM

det(M2)

#matriz inversa
solve(M2) %*% M2

#listas y dataframes
l1 <- list(num = c(11,10,22), 
           letra = c("a","c", "x", "y"),
           palabra = "hola soy rodrigo",
           mat = diag(1:5), 
           lista = list(nombre = "RODRIGO", edad = 39)
           
           )
l1

library(jsonlite)
# Convertir la lista a JSON
json_data <- toJSON(l1)
# Imprimir el JSON
cat(json_data)

str(l1) #muestra el numero de cosas (o las claves)

length(l1)
names(l1)
class(l1)
typeof(l1)

#podemos guardar los objetos en slots con nombres

l2 <- list(n2 = 4, p2 = c("w", "z")) # en este caso no tiene nombres
names(l2) <- c("n2", "p2") #otra forma de dar nombre a los elementos de la lista

#podemos combinar listas
l3 <- c(l1, l2)
l3

#acceso mediante posicion

l1[2] #lista con el segundo elemento (slot)
l1[c(1,3)] #lista con el primer y tercer elementos


l1[2][3] #da null

class(l1[2]) #esto es una lista

l1[[2]][3] #de la lista obtengo el vector, y del vector obtengo el tercer elemento
class(l1[[2]])


class(l1$letra[2])

l1$letra[3]

#quiero rescatar la edad de Juan
l1[[5]][2] #retorna una lista

l1[[5]][[2]] #retorna al elemento , al numero

#accediendo por nombre

l1["letra"] #lista con el vector
l1[["letra"]] #es el vector
l1[["letra"]][[3]]

#edad de juan

l1[["lista"]]["edad"] #lista con la edad
l1[["lista"]][["edad"]]

#accediendo usando " $ "

l1$letra[3]
l1$lista$edad #accede directo al dato dentro de la lista, (no es una lista)

length(l1) #cantidad de elementos

#para saber los nombres
names(l1)

#si quiere renombrar los slots

names(l1)[5] <- "Persona"

#tambien podemos crear nuevos "slots"
#indicando el nombre en dos [[]]
l1[["nueva_persona"]] <- "Pedro"
l1

#otra opcion usando signo $

l1$otro_numero <- c(1,7)
str(l1) #da un resumen

#podemos modificar un slot
#Ejemplo:modificar la edad de juan

l1$Persona$edad <- 30

l1$mat[4,4] <- 99
l1

l1[["mat"]][4,4] <- 88

#para borrar un slot

l1$otro_numero <- NULL
str(l1)

#CAMBIAR TIPO DE DATO

l1$Persona$edad <- as.character(l1$Persona$edad)
str(l1)

#ejemplo de uso
x <- rnorm(1000)
y <- rnorm(1000) +2 + 3*x
plot(x,y)
modelo <- lm(y ~ x)
hist(modelo$residuals)

plot(x,modelo$fitted.values)
plot(modelo)

#dataframes
df <- data.frame(nombre = c("Juan", "Diego", "Miguel"),
                 edad = c(25,40,33),
                 id = 1:3
                 )
str(df)

df["nombre"] #retorna un data frame
df[["nombre"]] #retorna un vector de la columna nombres
df$nombre

#accediendo como una si fuera una matriz
df[2,] #accediendo a la segunda fila

#segunda fila segunda columna
df[2,2]

#accede a la columna edad
df[,"edad"]

#crear columnas
df$tiene_ccte <- c(T,F,T)
df
df[["nombre_mascota"]] <- c("bobi", "akira", "daenerys")
df

#podemos seleccionar mas de una columna
df[,c("nombre", "nombre_mascota")] #accede a todas las filas

#pensando como una lista
df[c("nombre", "nombre_mascota")] #es como si accediera a la "lista" nombre y nombre mascota

#quiero traer las filas 1 y 3

df[c(1,3), c("nombre", "nombre_mascota")]

#hacer un data frame nuevo con las otras columnas
otro_df <- data.frame("nombre" = df$nombre , "mascotas" = df$nombre_mascota)

#quiero primera y tercera columna

df[,c(1,3)]

#consultando todas las columnas, donde la edad es mayor a 30, y que entregue el nombre de mascota
df[df$edad>30,"nombre_mascota"]
df[df$edad>30,]$nombre_mascota
str(df)

#cambiar nombre de una columna
colnames(df[4]) <- "tiene_cuenta_corriente" #buscado por posicion


#consultar una tabla especifica
names(df) == "tiene_cuenta_corriente" #Consulta las columnas
names(df)[names(df) == "tiene_cuenta_corriente"] <- "tiene_TC"

#agregar una fila
rbind(df, nuevo_df) #hace el match por los nombres


#mini taller

temp <- rnorm(n=100, mean = 21, sd =8)
N <- length(temp)
dia <- rep(c("L", "M", "W", "J", "V", "S", "D"), length.out = 100)

#e1 <- rep(c("E1"), N/3)
#e2 <- rep(c("E2"), N/3)
#e3 <- rep(c("E3"), N - (length(e1) + length(e2)))
#est <- c(e1, e2, e3)

#otra forma (NO FUNCIONA)
#rep(c("E1", "E2", "E3"), each = c(N/3, N/3, N/3+1))

est <- rep(c("E1", "E2", "E3"), times = c(33, 33, 34))

lluvia <- temp<20
lluvia

#crear un data frame
precipitaciones <- data.frame(Temperatura = temp,
                              Dia_semana = dia, 
                              Estacion = est,
                              Llueve = lluvia
                              )

head(precipitaciones, 15) #los primeros n registros
tail(precipitaciones, 15) #los ultimos 15 registros

summary(precipitaciones)






