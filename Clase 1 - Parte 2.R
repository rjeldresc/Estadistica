
# Script 1 - Parte 1

# Comentarios ####
# Con # se puede realizar un comentario (este texto no se ejecutara)

# Ejecucion ####
## Para poder correr una/s linea/s de codigo se debe apretar Ctrl + ENTER, F5 en R base.

## Operaciones ####

1+1 # suma
2*2 # multiplicacion
4/23331 # division

## Objeto ####

a <- 1 # Crea un objeto llamado a con el valor 1 dentro
b <- 2 # Para crear un objeto se puede utilizar = o <- 
c <- 10
d <- "1" # esto es un caracter, el cual se define con comillas ""

a*b # Con los objetos se pueden realizar operaciones, ejecutar comandos de manera mas facil
c/b

## que pasaria si ejecuta una operacion con un objeto no definido ##
a*e ## e no existe, por ende, produce un Error

## si definimos un objeto con el mismo nombre otra vez se sobre escriben ##
A <- 100 
A

A <- 900
A


## Directorio de trabajo ####

## Esto sera la ruta de mis archivos, es donde, yo estare trabajando localmente

## como puedo saber mi directorio?
getwd()

## como puedo definir mi directorio?
setwd("C:/Users/Matias/Desktop/Diplomado 2024")
## otra forma de hacerlo es a traves de clic, pestaña Session, Set Working Directory, Choose Directory,
## elegir la carpeta, Open.


## Instalar y cargar paquetes ####

# install.packages("readxl") # esto se realiza una vez "en la vida" util de su R
library(readxl)

# readxl::read_excel() ## llamo directamente a una funcion

## Ojo, al instalar, el nombre del paquete va entre comillas
## para llamar a un paquete no va con comillas necesariamente

## Importas tablas de datos ##

?read_excel # consulta la funcion en el help, para saber un poco mas de ella

base <- read_excel("Bases de datos/Base_de_datos_1.xlsx")
## esta la forma directa y que se puede volver a reproducir facilmente

base <- read_excel(file.choose()) ## Esta es la forma mas sencilla

?prop.table
