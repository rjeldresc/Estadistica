####### TLC. TEOREMA DEL LIMITE CENTRAL #######

#Teorema fundamental de probabilidad y estadística.
#Describe la distribución de la suma y la media de una muestra aleatoria proveniente de una población con
#varianza finita como una aproximación de la distribución normal.

library(ggplot2)

####### Ejemplo: Población UNIFORME (0,1) (Medias muestrales)
#Supongamos que en un campo hay 30.000 árboles y cada uno de éstos tiene entre 150 y 300 piezas de fruta.
set.seed(900) #Establezco una semilla para que R siempre saque las mismas muestras.
Poblacion <- round(runif(30000, min = 150, max = 300),0)
plot(density(Poblacion)) 
#la probabilidad de encontrar un árbol con determinado número de frutos (dentro del intervalo 150-300).

#calculamos la media y desv tipica de la poblacion
MediaPoblacion <- mean(Poblacion)
DesTipicaPoblacion <- sd(Poblacion)

#Vamos a ver cómo es la distribución de 5 muestras, con un tamaño de 30 árboles cada una. 
Muestras_5_30 <- replicate(n = 5, sample(Poblacion, 30, replace = FALSE))

#calculo las medias de cada muestra
Medias_5_30 <- colMeans(Muestras_5_30)
plot(density(Medias_5_30)) 

#la media muestral entonces es:
MediaMuestral_5_30 <- mean(Medias_5_30)

#la desviacion muestral es:
DesTipicaMuestral_5_30 <- sd(Medias_5_30)

#la media y desviacion poblacion segun el TLC sería:
MediaPoblacion <- mean(Poblacion)
DesTipicaPoblacion <- sd(Poblacion)/sqrt(30) 

plot(density(Poblacion), col = "red", ylim = c(0, 0.05), xlim = c(125, 325), lwd = 2) 
abline(v = MediaPoblacion, col = "red", lty = 4, lwd = 2)
lines(density(Medias_5_30), col = "green", lwd = 2)
abline(v = MediaMuestral_5_30, col = "blue", lty = 2, lwd = 2)
abline(v = MediaMuestral_5_30 - DesTipicaMuestral_5_30, col = "blue", lty = 3, lwd = 3)
abline(v = MediaMuestral_5_30 + DesTipicaMuestral_5_30, col = "blue", lty = 3, lwd = 3)


####### Ejemplo: Población UNIFORME (0,1)
r1 <- 1000 # Número de muestras
n <- 20    # Tamaño de cada muestra

#forma 1
M1 = matrix(0,n,r1)  # crear matriz de tamaño nxr
Xbarra1 = rep(0,r1) # crea columna para almacenar MEDIAS

for (i in 1:r1)   {M1[,i] = runif(n,0,1)}  # 10000 datos de población uniforme (0,1)
for (i in 1:r1)   {Xbarra1[i] = mean(M1[,i])} # medias muestrales

hist(M1)
plot(density(M1))
abline(v = mean(Xbarra1), col = "red", lty = 4, lwd = 2)

hist(Xbarra1)
abline(v = mean(Xbarra1), col = "red", lty = 4, lwd = 2)
plot(density(Xbarra1))
abline(v = mean(Xbarra1), col = "red", lty = 4, lwd = 2)


#forma 2
set.seed(123)


datos_matriz <- replicate(r1, runif(n)) 
# Genera r1 muestras de n valores aleatorios cada una de una distribución uniforme.
Xbarra2 <- replicate(r1, mean(runif(n))) # Genera las medias de múltiples muestras de una distribución uniforme
?replicate

#gráfico de los valores de las muestras
datos<-as.vector(datos_matriz)
data1 <- data.frame(datos)
ggplot(data1, aes(x = datos)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "blue") +
  #Es una variable estadística que representa la densidad de probabilidad en lugar del conteo de observaciones en cada bin del histograma
  #calcula y muestra la densidad de probabilidad en cada bin, en lugar del número de observaciones en cada bin. 
  #Esto es útil cuando deseas comparar el histograma con una función de densidad
  #De esta manera, el histograma y la curva de densidad estarán en la misma escala.
  geom_function(fun = dunif, args = list(min=0, max = 1), color = "red", size = 1) +
  #otra forma de graficar funciones definidas
  labs(title = "Población Uniforme",
      x = "Valores",
      y = "Densidad") +
  theme_minimal()

#De acuerdo al TLC para los promedios:
Media_TLC <- mean(datos)
Des_TLC <- sd(datos)/sqrt(n) #n es el tamaño de cada muestra

#gráfico de los promedios
data2 <- data.frame(Xbarra1) # Crear un data frame con las medias
ggplot(data2, aes(x = Xbarra1)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "blue") + 
  geom_function(fun = dnorm, args = list(mean = Media_TLC, sd = Des_TLC), color = "red", size = 2) +
  #agrega una función de densidad normal (en rojo) con la media y desviación estándar de las medias de la muestra Uniforme
  labs(title = "Población Uniforme",
       x = "Media de la Muestra",
       y = "Densidad") +
  theme_minimal()


####### Ejemplo: Población Exponencial (1/12) 
r1 = 1000   # 1.000 muestras
n = 20      # tamaño de cada muestra

datos_matriz<- replicate(r1,rexp(n,1/12)) 

#gráfico de los valores de las muestras
datos<-as.vector(datos_matriz)
data1 <- data.frame(datos)
ggplot(data1, aes(x = datos))+
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "blue")+
  geom_function(fun = dexp, args = list(rate = 1/12), color = "red", size = 1)+
  labs(title = "Población Exponencial",
       x = "Valores",
       y = "Densidad") +
  theme_minimal()

#De acuerdo al TLC para los promedios:
Media_TLC <- mean(datos)
Des_TLC <- sd(datos)/sqrt(n)  
?rexp
#gráfico de los promedios
Xbarra1 <- replicate(r1, mean(rexp(n,1/12))) # Genera las medias de múltiples muestras de una distribución exponencial
data2 <- data.frame(Xbarra1) # Crear un data frame con las medias
ggplot(data2, aes(x = Xbarra1)) +
  geom_histogram(aes(y = ..density..), bins = 50, fill = "skyblue", color = "blue") + 
  geom_function(fun = dnorm, args = list(mean = Media_TLC, sd = Des_TLC), color = "red", size = 1) +
  labs(title = "Población Exponencial",
       x = "Media de la Muestra",
       y = "Densidad") +
  theme_minimal()

####### Ejemplo: Población Poison (3)
r1 <- 1000     # Número de muestras
n <- 20  # Tamaño de cada muestra
datos_matriz<- replicate(r1,rpois(n,3)) 

#gráfico de los valores de las muestras
datos<-as.vector(datos_matriz)
data1 <- data.frame(datos)
ggplot(data1, aes(x = datos))+
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "blue")+
  labs(title = "Población Poisson",
       x = "Valores",
       y = "Densidad") +
  theme_minimal()

#De acuerdo al TLC para los promedios:
Media_TLC <- mean(datos)
Des_TLC <- sd(datos)/sqrt(n)

#gráfico de los promedios
Xbarra1 <- replicate(r1, mean(rpois(n,3))) # Genera las medias de múltiples muestras de una distribución Poison
data2 <- data.frame(Xbarra1) # Crear un data frame con las medias
ggplot(data2, aes(x = Xbarra1)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "blue") + 
  geom_function(fun = dnorm, args = list(mean = Media_TLC, sd = Des_TLC), color = "red", size = 1) +
  labs(title = "Población Poisson",
        x = "Media de la Muestra",
        y = "Densidad") +
  theme_minimal()


##### Ejemplos Sumas de distribuciones

# E1
#Muestras provenientes de una población Normal (media=75, desv=10)
r2 = 1000   # 1.000 muestras
n = 30      # tamaño de cada muestra

M2 = matrix(0,n,r2)  # crear matriz de tamaño nxr
Sumas2 = rep(0,r2) # crea columna para almacenar SUMAS

for (i in 1:r2)   {M2[,i] = rnorm(n,mean=75,sd=10)}  # 10000 datos de población Normal
for (i in 1:r2)   {Sumas2[i] = sum(M2[,i])} # sumas muestrales

hist(M2, main="Histograma de X")
hist(Sumas2, main="Histograma de sumas")


mean(Sumas2)
n*75
sd(Sumas2)
sqrt(n*10*10)

#otra forma
datos_matriz<- replicate(r2,rnorm(n,mean=75,sd=10)) 
Xbarra <- replicate(r2, sum(rnorm(n,mean=75,sd=10))) 

datos<-as.vector(datos_matriz)
data1 <- data.frame(datos)
ggplot(data1, aes(x = datos))+
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "blue")+
  geom_function(fun = dnorm, args = list(mean=75,sd=10), color = "red", size = 1)+
  labs(title = "Histograma",
       x = "Valores",
       y = "Densidad") +
  theme_minimal()

#De acuerdo al TLC para las sumas:
Media_TLC <- n*75
Des_TLC <- sqrt(n*10*10)

data2 <- data.frame(Xbarra) 
ggplot(data2, aes(x = Xbarra)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "blue") + 
  geom_function(fun = dnorm, args = list(Media_TLC, sd = Des_TLC), color = "red", size = 1) +
  labs(title = "Suma",
       x = "Suma",
       y = "Densidad") +
  theme_minimal()
