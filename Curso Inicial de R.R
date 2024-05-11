
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

