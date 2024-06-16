library(ggplot2)
#install.packages("ggfortify")
library(ggfortify)

#la media es μ= 500 y la desviación estándar es σ= 100.
#P(X>600) 
x1<-seq(200, 800, 0.1)
x2<-seq(600, 800, 0.1) #pinta desde el 600 en adelante

ggdistribution(dnorm, 
               x1, 
               mean=500, 
               sd=100 , 
               colour = "blue",
               p = ggdistribution(dnorm, x2, 
                                  mean=500, 
                                  sd=100, 
                                  colour = "red", fill = "yellow"))

1-pnorm(600, mean=500,sd=100) 
# 0.1586553

#P(X<600)
x1<-seq(200, 800, 0.1)
x2<-seq(200, 600, 0.1)
ggdistribution(dnorm, x1, 
               mean=500, 
               sd=100 , 
               colour = "blue",
               p = ggdistribution(dnorm, 
                                  x2, 
                                  mean=500, 
                                  sd=100, 
                                  colour = "red", 
                                  fill = "yellow"))

#P(400<X<600) 
x1<-seq(200, 800, 0.1)
x2<-seq(400, 600, 0.1)
ggdistribution(dnorm, x1, mean=500, sd=100 , colour = "blue",
               p = ggdistribution(dnorm, x2, mean=500, sd=100, colour = "red", fill = "yellow"))
?ggdistribution
##############

#EJERCICIO 1
#En una ciudad se estima que la temperatura máxima en el mes de junio sigue una distribución normal, 
#con media 23° y desviación típica 5°.
#Calcular el número de días del mes en los que se espera alcanzar máximas entre 21° y 27°.

#P(21 < X < 27); μ=23°y σ=5°      
#a) P(21 < X < 27) = P(X<27)-P(X<21)
p <- pnorm(q = 27, mean = 23, sd = 5)-pnorm(q = 21, mean = 23,sd = 5) # Probabilidad de un día
p
# 0.4435663

#Convertir la probabilidad en un número esperado de días: 0.4435×30≈13.305

30*p #número de  días 13.30699

x1 <- seq(23 - 3 * 5, 23 + 3 * 5, 0.1) #(μ-3*σ,μ+3*σ) truco para el dibujo
x2 <- seq(21, 27, 0.1) #el area pintada
ggdistribution(dnorm, x1, 
               mean=23, 
               sd=5 , 
               colour = "blue",
               p = ggdistribution(dnorm, 
                                  x2, 
                                  mean=23, 
                                  sd=5, 
                                  colour = "red", fill = "yellow"))

#b)  P(21 < X < 27) =  P((21-23)/5 < Z < (27-23)/5) = P(-0.4 < Z < 0.8)
#    = P(Z<0.8) - (1-P(Z<0.4)) = P(Z<0.8)+P(Z<0.4)-1
p<-pnorm(0.8)+pnorm(0.4)-1
p

30 * p


#EJERCICIO 2
#Siguiendo con las temperatura máxima X en un día de verano, pero ahora considerando 2 regiones (independientes).
#X1 Normal de media 28°C y desviación estándar 3°C
#X2 Normal de media 24°C y desviación estándar 5°C

#a) Graficar las dos distribuciones
x1<-seq(28-3*3,28+3*3,0.01)
x2<-seq(24-3*5,24+3*5,0.01)

ggdistribution(dnorm, x1, mean=28, sd=3 , colour = "blue",
               p=ggdistribution(dnorm, x2, mean=24, sd=5, colour = "red"))


#b)¿Cuál es la probabilidad aproximada que un día cualquiera la temperatura máxima de la región 1 
# supere a la temperatura máxima del mismo día en la región 2?

#P(X1>X2) = P(X1-X2 > 0) = 1-P(X1-X2 < 0) = 1-P(X3<0)
# X1 - X2 = X3~ Normal(media = 28-24 = 4, varianza = 3^2+5^2 = 34)

1 - pnorm(q = 0,mean = 4,sd = sqrt(34))
#0.7536417
pnorm(q = 0,mean = 4,sd = sqrt(34),lower.tail = FALSE)
#0.7536417

#c) ¿Cuál es la probabilidad que el promedio de una muestra de 10 días de la región 1 sea inferior a 26°C?
#P(X1_prom<26)
#X1_prom ~ Normal(media = 28, varianza = 3^2/10=9/10=0.9487)
pnorm(26,mean = 28, sd = sqrt(9/10))

ggdistribution(dnorm, x1, mean=28, sd=3 , colour = "blue",
               p=ggdistribution(dnorm, x1, mean=28, sd=sqrt(9/10), colour = "red"))

#EJERCICIO 3
#Una compañía fabrica ampolletas con vida media de 500 horas y desviación estándar de 100. 
#Suponga que los tiempos de vida útil de las ampolletas se distribuyen normalmente.
#a) Encuentre la probabilidad de que cierta cantidad de ampolletas dure menos de 650 horas.
#   P(X<650) = P(Z<(150/100)) (650 -500)/100

pnorm(650,mean = 500,sd = 100)
pnorm(1.5)
#R: 0.9331928

#b)Calcule la probabilidad de que cierta cantidad de ampolletas dure más de 780 horas.
#P(X>780) = 1-P(X<780)
1 - pnorm(780,500,100)
pnorm(780,500,100,lower.tail = FALSE)
#R: 0.00255513

#c) Determine la probabilidad de que cierta cantidad de ampolletas dure entre 650 y 780 horas.
#P(650<X<780)=P(X<780)-P(X<650)
pnorm(780,500,100)-pnorm(650,500,100)
#R: 0.06425207

#d) Halle el valor de k tal que el 5% de las ampolletas tenga un tiempo de vida mayor que  k horas? 
#P(X>k)=0.05
qnorm(1-0.05,500, 100)
#R: 664.4854

#e) Si se eligen 10.000 ampolletas, ¿cuántas tuvieron un tiempo de vida entre 650 y 780 horas?

#De la pregunta c), la probabilidad de que cierta cantidad de ampolletas dure entre 650 y 780 horas es p=0.0643.
#Si se eligen n=10.000 ampolletas, entonces 
10000*0.0643 #ampolletas que tuvieron un tiempo de vida entre 650 y 780 horas.
#R: 643 ampolletas

#f) Si se eligen 1.200 ampolletas, ¿cuál es la probabilidad de que al menos 3 
# duren más de 780 horas?
# De la pregunta b) la probabilidad de que cierta cantidad de ampolletas dure más de 780 horas es p=0.0026
# Se eligen n = 1.200 ampolletas. 
# Definamos como Y a la variable aleatoria que representa al número de ampolletas en la muestra que duran más de 780 horas. 
# Entonces, Y tiene distribución binomial con parámetros n=1200 y p=0.0026.
#P(Y >= 3) = 1 - P(Y <= 2)
1 - pbinom(2, size = 1200, prob = 0.0026) #0.0026 sale de la parte b
#R: 0.603464


#g) Si se eligen 20 ampolletas, ¿cuál es la probabilidad de que entre 16 y 19 (ambos inclusive) duren menos de 650 horas?
# De la pregunta a), 
# la probabilidad de que cierta cantidad de ampolletas dure menos de 650 horas es p=0.9332

#Se eligen n=20 ampolletas. 
# Definamos como Y a la variable aleatoria que representa al número de ampolletas 
# en la muestra que duran menos de 650 horas. 
#Entonces, Y tiene distribución binomial con parámetros n=20 y p=0.9332.
#P(16<=Y<=19) = P(Y<=19)-P(Y<=15)
pbinom(19, 20, 0.9332) - pbinom(15, 20, 0.9332)
#R: 0.7402734


#h) Si se eligen 20 ampolletas, ¿cuál es la probabilidad de que entre 16 y 19 (ambos inclusive) 
# no duren menos de 650 horas?
# De la pregunta a), la probabilidad de que cierta cantidad de ampolletas dure menos de 650 horas es p=0.9332,
# entonces que no duren es 1 - 0.9332=0.0668

pbinom(19, 20, 0.0668) - pbinom(15, 20, 0.0668)



#EJERCICIO 4 TAREA
# La media de los pesos de 500 estudiantes de un Instituto es 70 kg y la desviación típica 3 kg. 
# Suponiendo que los pesos se distribuyen normalmente, hallar cuántos estudiantes pesan:
#a) Entre 60 kg y 65 kg. (R=476)
#b) Más de 90 kg. (R=0)
#c) Menos de 64 kg. (R=11)
#d) 64 kg. (R=0)
#e) 64 kg o menos.(R=11)
# Mostrar los gráficos en cada caso.
