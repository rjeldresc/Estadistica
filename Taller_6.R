##### CALCULO PROBABILIDAD TABLA: EJEMPLO 1 #####
datos <- matrix(c(40,73,28,10,9,24,14,3,46,77,31,22,33,62,36,21),4, 4)
colnames(datos)=c("Corea","Francia","Japon","USA")
rownames(datos)=c("4X4","Automovil","Camioneta","Familiar")

base <- as.data.frame(datos)
attach(base)
addmargins(datos)

##¿Cuál es la probabilidad que un vehículo sea de tipo 4x4 y provenga de Corea?
E1<- round(prop.table(datos),2)
addmargins(E1)

##¿Cuál es la probabilidad que un vehículo de tipo 4x4, provenga de Corea?
E2<- round(prop.table(datos,1),2) #"totales por fila"
addmargins(E2,2)

##¿Cuál es la probabilidad que un vehículo sea de tipo 4x4, si proviene de Corea?
E3<- round(prop.table(datos,2),2) # totales por columna
addmargins(E3,1)  


##### CALCULO PROBABILIDAD TABLA: EJEMPLO 2 #####
datos2 <- matrix(c(50,45,30,20),2, 2)
colnames(datos2)=c("Propietario","Arrendatario")
rownames(datos2)=c("A favor","En contra")

base2 <- as.data.frame(datos2)
attach(base2)
addmargins(datos2)

#¿Cuál es la probabilidad de estar a favor del reglamento?, Si se considera al total de personas
A1<- round(prop.table(datos2),2) 
addmargins(A1)

#Si se considera solo propietarios
A2<- round(prop.table(datos2,2),2) 
addmargins(A2,1)

#Si se considera solo arrendatarios
A3<- round(prop.table(datos2,2),2) 
addmargins(A3,1)


##### EJEMPLO 3 #####
setwd("d:/dev/Estadistica")
load(file="Estudiantes.Rdata")
typeof(Estudiantes)
Estudiantes <- as.data.frame(Estudiantes)

#a) Defina como "datos" al data frame con las 100 primeras observaciones: 
datos <- Estudiantes[1:100,]
attach(datos)

#b) Construya una tabla de frecuencias para la variable Sexo y el diagrama de barras correspondiente

Cuentas <- table(Sexo) #tabla de frecuencias
typeof(Cuentas)

barplot(Cuentas, 
        main="Diagrama de barras", 
        xlab="Sexo", ylab="Frecuencias", 
        #legend = rownames(Cuentas), 
        col=c("pink","blue"),
        ylim = c(0, 70)
        )

#Considere solamente las primeras 100 observaciones. Supongamos que se seleccionan cuatro estudiantes al azar
#c) ¿Cuál es la probabilidad de seleccionar dos mujeres y dos hombres?
em <- choose(100, 4) #espacio muestral , es la combinacion de 100c4
Mujer <- choose(49, 2) #combinacion de mujeres. número 49 de tabla anterior
Hombre <- choose(51,2) #combinacion de hombres. número 51 de tabla anterior
?choose
Probabilidad_c <- (Mujer * Hombre)/em

#d)¿Cuál es la probabilidad de seleccionar cuatro mujeres?
Mujer <- choose(49, 4)

Probabilidad_d <- Mujer/em

#e)¿Cuál es la probabilidad de seleccionar tres hombres? 
Hombre <- choose(51,3)
Mujer <- choose(49, 1)

Probabilidad_e <- (Hombre * Mujer)/em

#f)¿Cuál es la probabilidad de seleccionar cuatro mujeres y tres hombres?

#La probabilidad de seleccionar cuatro mujeres y tres hombres es 0, 
#ya que los eventos “seleccionar cuatro mujeres” y “seleccionar tres hombres” son disyuntos (intersecciones vacías)

#g) ¿Cuál es la probabilidad de seleccionar cuatro mujeres o tres hombres?

Probabilidad_g <- Probabilidad_d +Probabilidad_e

#h) ¿Cuál es la probabilidad de que no seleccionemos hombres?

#si no se seleccionan hombres, entonces, hemos seleccionado cuatro mujeres

Probabilidad_h <- Probabilidad_d 

#i) ¿Cuál es la probabilidad de que seleccionemos un hombre? 
Hombre <- choose(51,1)
Mujer <- choose(49, 3)
Probabilidad_i <- (Hombre * Mujer)/em

#     R: 0.2396251
#j) ¿Cuál es la probabilidad de que seleccionemos dos hombres?
Hombre <- choose(51,2)
Mujer <- choose(49, 2)
Probabilidad_j <- (Hombre * Mujer)/em

#     R: 0.3823805
#k) ¿Cuál es la probabilidad de seleccionar máximo dos hombres?

Hombre <- choose(51,0)
Mujer <- choose(49, 4)
Probabilidad_1 <- (Hombre * Mujer)/em

Hombre <- choose(51,1)
Mujer <- choose(49, 3)
Probabilidad_2 <- (Hombre * Mujer)/em

Hombre <- choose(51,2)
Mujer <- choose(49, 2)
Probabilidad_3 <- (Hombre * Mujer)/em

Probabilidad_k <- Probabilidad_1 + Probabilidad_2 + Probabilidad_3

#     R: 0.6760387
#     Hint: Debemos sumar las probabilidades de seleccionar 0, 1 y 2 hombres

#l) ¿Cuál es la probabilidad de seleccionar máximo dos mujeres? 
#     R: 0.7063418
#m) ¿Cuál es la probabilidad de seleccionar al menos tres hombres?
#      R: 0.3239613
#      Hint: el evento “seleccionar al menos tres hombres” es el complemento del evento “seleccionar máximo dos hombres”
#n) ¿Cuál es la probabilidad de seleccionar máximo dos mujeres? 
#      R: 0.7063418 
#      Hint: El evento “seleccionar máximo dos mujeres” es el complemento del evento “seleccionar al menos tres mujeres”
#            Y el evento “seleccionar al menos tres mujeres” es la suma de los eventos "de seleccionar 4" y "seleccionar 3" 
