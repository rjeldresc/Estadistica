
# curso de R 

setwd("d:/dev/Estadistica")

x<-50
y<-10

x*y

#1000 valores de una distribucion normal
muestra <- rnorm(1000)
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