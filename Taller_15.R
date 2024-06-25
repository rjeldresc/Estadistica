#### Función chisq.test ####
# La función chisq.test de R realiza contrastes chi-cuadrado de Pearson para la independencia, 
# la bondad de ajuste y la homogeneidad, analizando relaciones entre datos categóricos. 

chisq.test(x, y = NULL, correct = TRUE,
           p = rep(1/length(x), length(x)), rescale.p = FALSE,
           simulate.p.value = FALSE, B = 2000)

# x: una tabla de contingencia, data frame, matriz o vector.
# y: opcionalmente, una segunda variable para realizar una prueba de independencia en una tabla de contingencia.
# correct: lógico. Si es TRUE (por defecto), aplica la corrección de continuidad de Yates para tablas de contingencia 2x2.
# p: vector de probabilidades para una prueba de bondad de ajuste.
# rescale.p: lógico. Si es TRUE, reescala las probabilidades para que sumen 1.
# simulate.p.value: lógico. Si es TRUE, calcula el p-valor utilizando la simulación Monte Carlo.
# B: número de réplicas para la simulación Monte Carlo.

# *La corrección de Yates se aplica a la prueba ji-cuadrado cuando al menos el valor de una frecuencia 
#  esperada es menor que 5. Chi-cuadrado corregida: En general, se aplica la corrección de Yates o 
#  también corrección por continuidad cuando se aproxima una variable discreta a una distribución continua.


#### Ejemplo: Test de independencia
# Se utiliza para determinar si existe una asociación significativa entre DOS variables categóricas. 
# Evalúa si la distribución de frecuencias de una variable depende de la distribución de otra variable.

# Considera que tienes 100 productos clasificados por calidad y precio alto-bajo:
library(ggplot2) 
#install.packages("ggmosaic")
library(ggmosaic)

#setwd("C:/Users/caroh/Documents/RStudio/Clases/Clase 15") 
setwd("D:/dev/Estadistica")

base <- readxl::read_excel("Base.xlsx",sheet="EJE1")
attach(base)

ggplot(base,aes(x=Calidad,fill=Precio))+
  geom_bar(position = "dodge")

#H0: las variables Calidad y Precio SON INDEPENDIENTES en la población.
#H1: las variables Calidad y Precio SON DEPENDIENTES en la población.

datos<-table(Calidad,Precio)

chi1 <- chisq.test(datos)
chi1

# El p-valor es casi cero, por lo que hay pruebas significativas para rechazar la hipótesis nula 
# de que no existe asociación entre las variables sometidas a prueba. 
# Por lo tanto, es probable que exista una asociación entre calidad y precio.

# OJO: Se recomienda utilizar la prueba exacta de Fisher (fisher.test()) para tablas de contingencia 2x2 
# en lugar del test Chi-cuadrado cuando el tamaño de la muestra es pequeño (n<30) o cuando las frecuencias 
# esperadas son inferiores a 5.

# Un gráfico utilizado para visualizar la posible asociación entre dos variables categóricas 
# es el gráfico mosaico o gráfico de Marimekko (de barras apiladas)

ggplot(base) +
  geom_mosaic(aes(x = product(Precio,Calidad), fill = Precio))

# Las columnas se forman de barras apiladas que suman 100%. El ancho de las barras es proporcional 
# al total de los valores de la columna. El objetivo es hacer una comparación entre las categorías 
# de la variable ordinal.


#### Ejemplo: Test de homogeniedad
# Compara las distribuciones de UNA única variable categórica entre varios grupos o poblaciones 
# independientes. Determina si la distribución de frecuencias de una única variable categórica 
# es similar u homogénea entre diferentes grupos.

# Se quiere comprobar si la distribución de frecuencias de un tratamiento es igual o 
# diferente entre grupos de edad, es decir, si la proporción de uso de varios fármacos es 
# igual o diferente entre grupos de edad. 

base2 <- readxl::read_excel("Base.xlsx",sheet="EJE2")
attach(base2)

ggplot(base2,aes(x=Farmaco,fill=Edad))+
  geom_bar(position = "dodge")

datos2<-table(Edad,Farmaco)

# H0: las distribuciones de dos rangos de edad en cada grupo SON IGUALES (proporciones iguales).
# H1: las distribuciones de dos rangos de edad en cada grupo SON DIFERENTES (al menos una proporción es diferente).

chi2<-chisq.test(datos2)
chi2
# El p-valor es 0.6967, por lo que no hay pruebas para rechazar la hipótesis nula de que la distribución 
# es igual entre los grupos de edad.

ggplot(base2) +
  geom_mosaic(aes(x = product(Farmaco,Edad), fill = Farmaco))


#### Ejemplo: Test de bondad de ajuste
# Examina si una distribución de frecuencias observada coincide con una distribución teórica esperada, 
# como la distribución uniforme o cualquier otra distribución.

# Si tienes un vector con frecuencias observadas, puedes realizar un test chi-cuadrado para comprobar 
# si las frecuencias observadas se ajustan a las probabilidades esperadas (teóricas):

observadas <- c(11, 32, 24)   # Frecuencias observadas
esperadas <- c(0.2, 0.5, 0.3) # Probabilidades esperadas (suman 1)

#H0: la distribución de la población ES F.
#H1: la distribución de la población NO ES F.

#¿Son las probabilidades poblacionales iguales a 'p'? 
chisq.test(x = observadas, p = esperadas)

# El p-valor es superior a los niveles de significación habituales, por lo que no hay pruebas para 
# rechazar la hipótesis nula.Por lo tanto, la distribución de la población es F.

# Ten en cuenta que también puedes introducir frecuencias esperadas en lugar de probabilidades 
# si estableces rescale.p = TRUE, ya que las frecuencias se reescalarán si es necesario para que sumen 1.

observadas <- c(11, 32, 24) # Frecuencias observadas
esperadas <- c(20, 53, 25)  # Frecuencias esperadas

chisq.test(x = observadas, p = esperadas, rescale.p = TRUE)

# Por último, si no se especifica p, se contrastará si todas las probabilidades son iguales 
# (distribución uniforme)

observadas <- c(15, 25, 20)
#¿Son las probabilidades poblacionales iguales? (¿Es la población uniforme?)
chisq.test(observadas)
# Como p-value = 0.2865, entonces la poblacion es uniforme.
#valores esperados:
chisq.test(observadas)$expected


