########## ANOVA DE UNA VIA PARA DATOS INDEPENDIENTES ##########
# Supóngase que se un estudio quiere comprobar si existe una diferencia significativa entre el 
# % de bateos exitosos de los jugadores de béisbol dependiendo de la posición en la que juegan. 
# En caso de que exista diferencia se quiere saber qué posiciones difieren del resto. 
# La siguiente tabla contiene una muestra de jugadores seleccionados aleatoriamente

# La hipótesis nula de la que parten los diferentes tipos de ANOVA es que la media de la variable 
# estudiada es la misma en los diferentes grupos, en contraposición a la hipótesis alternativa de 
# que al menos dos medias difieren de forma significativa. 
# ANOVA permite comparar múltiples medias, pero lo hace mediante el estudio de las varianzas.

# H0: µ1 = µ2 = ... = µk 
# H1: µi distinto µj , para algún i j

library(readxl)
setwd("d:/dev/Estadistica")
datos <- data.frame(read_excel("Datos.xlsx"))
attach(datos)

                       ##### 1. Estudio de los datos #####  

# Se identifica el número de grupos y cantidad de observaciones por grupo para determinar si es 
# un modelo equilibrado. También se calculan la media y desviación típica de caga grupo

table(posicion)
aggregate(bateo ~ posicion, data = datos, FUN = mean) #calcula resúmenes estadísticos (esta caso, promedio) para subconjuntos de datos.
aggregate(bateo ~ posicion, data = datos, FUN = sd)
?aggregate
# Dado que el número de observaciones por grupo no es constante, 
# se trata de un modelo No Equilibrado.

# La representación gráfica mas útil antes de realizar un ANOVA es el modelo Box-Plot
library(ggplot2)
ggplot(data = datos, aes(x = posicion, y = bateo, color = posicion)) +
  geom_boxplot() +
  #geom_point() +
  theme_bw()

# Este tipo de representación permite identificar de forma preliminar si existen asimetrías, 
# datos atípicos o diferencia de varianzas. En este caso, los 4 grupos parecen seguir una 
# distribución simétrica. En el nivel del factor IF se detectan algunos valores extremos.
# El tamaño de las cajas es similar para todos los niveles por lo que no hay indicios de 
# falta de homocedasticidad (varianzas iguales).


                      ##### 2. Condiciones para una ANOVA ##### 

# Para un ANOVA de una vía se debe comprobar el supuesto de independencia, el supuesto de normalidad 
# y el supuesto de homocedasticidad. Despúes se estima el modelo ANOVA sobre lo que queremos probar, 
# en nuestro caso si existe diferencia significativas entre  % de bateos exitosos dependiendo de la posición 

# - Independencia:
#     Los grupos (variable categórica, la posición) son independientes entre ellos ya que se ha hecho un 
#     muestreo aleatorio de jugadores de toda la liga (no solo de un mismo equipo).
# - Distribución normal de las observaciones: 
#     La variable cuantitativa debe de distribuirse de forma normal en cada uno de los grupos. 
#     El estudio de normalidad puede hacerse de forma gráfica (qqplot) o con test de hipótesis.

par(mfrow = c(2,2))
qqnorm(datos[posicion == "C","bateo"], main = "C")
qqline(datos[posicion == "C","bateo"])
qqnorm(datos[posicion == "DH","bateo"], main = "DH")
qqline(datos[posicion == "DH","bateo"])
qqnorm(datos[posicion == "IF","bateo"], main = "IF")
qqline(datos[posicion == "IF","bateo"])
qqnorm(datos[posicion == "OF","bateo"], main = "OF")
qqline(datos[posicion == "OF","bateo"])
par(mfrow = c(1,1))

# qqnorm: un gráfico de Q-Q (quantile-quantile) es un gráfico de probabilidad, 
# es un método gráfico para comparar dos distribuciones de probabilidad al 
# trazar sus cuantiles uno contra el otro.En este caso, lo ideal es que los puntos 
# se acerquen a una recta diagonal.

#para OF
d1<-datos[posicion=="OF",]
ks.test(d1$bateo,"pnorm",mean(d1$bateo),sd(d1$bateo))
# p-valor = 0.9738

#para IF
d2<-datos[posicion=="IF",]
ks.test(d2$bateo,"pnorm",mean(d2$bateo),sd(d2$bateo))
# p-valor = 0.4256

#para DH
d3<-datos[posicion=="DH",]
ks.test(d3$bateo,"pnorm",mean(d3$bateo),sd(d3$bateo))
# p-valor = 0.9862

#para C
d4<-datos[posicion=="C",]
ks.test(d4$bateo,"pnorm",mean(d4$bateo),sd(d4$bateo))
# p-valor = 0.9281

# Los test de hipótesis no muestran evidencias de falta de normalidad

# - Varianza constante entre grupos (homocedasticidad):
#install.packages("car")
library(car)
leveneTest(bateo ~ posicion,datos) #primero va la variable numerica, luego la categorica

#No hay evidencias significativas de falta de homocedasticidad 

# El estudio de las condiciones puede realizarse previo cálculo del ANOVA, 
# puesto que si no se cumplen no tiene mucho sentido seguir adelante. 
# Sin embargo la forma más adecuada de comprobar que se satisfacen las condiciones 
# necesarias es estudiando los residuos del modelo una vez generado el ANOVA.


             ##### 3. Análisis de varianza ANOVA #####
 
anova <- aov(bateo ~ posicion)
summary(anova)

plot(anova,1)
plot(anova$residuals)

# DF: grados de libertad
# Sum Sq: sumas cuadraticas
# Mean Sq: medias cuadraticas
# F value: Estadítico F
# Pr: Valor p

par(mfrow = c(2,2))
plot(anova)
dev.off

# Dado que el p-value es superior a 0.05 no hay evidencias suficientes para considerar que al 
# menos dos medias son distintas. La representación gráfica de los residuos no muestra falta 
# de homocedasticidad (gráfico 1) y en el qqplot los residuos se distribuyen muy cercanos a 
# la linea de la normal (gráfico 2).


              ##### 4. Comprobación de los supuestos ##### 

# - NORMALIDAD
#     H0: residuos distribuyen normales
#     H1: residuos no normales
ks.test(anova$residuals,"pnorm",mean=mean(anova$residuals),sd= sd(anova$residuals))
# p-value = 0.3414

shapiro.test(anova$residual)
#p-value = 0.01061
lillie.test(datos$bateo)
plot(anova,2)

# - HOMOCEDASTICIDAD (varianzas iguales)

leveneTest(anova$residuals,posicion)
#p-valor= 0.0518 

#install.packages("lmtest")
library(lmtest)
bptest(anova)
#p-value = 0.03255

# Las varianzas entre los grupos no son significativamente iguales.
# Los grupos varian distintos con respecto a la media.

# Si no se puede aceptar la homocedasticidad, se recurre a lo que se conoce como 
# ANOVA heterodástica que emplea la corrección de Welch (Welch test).

oneway.test(bateo ~ posicion)
#p-value = 0.2046

# - INDEPENDENCIA
# H0: residiuos son independientes
# H1: no son independientes
plot(anova$residuals)
# si el gráfico muestra aleatoridad, se cumple la independencia

save.image("D:/dev/Estadistica/Taller_14.RData")

             ##### 5. Comparaciones múltiples ##### 

TukeyHSD(anova)
plot(TukeyHSD(anova),las=1)

# No se encuentra diferencia significativa entre ningún par de medias.


             ##### 6. Conclusión ##### 
# La técnicas de inferencia ANOVA no han encontrado significancia estadística 
# para rechazar que las medias son iguales entre todos los grupos.
# Entonces, ni existe diferencia significativas entre  % de bateos exitosos dependiendo de la posición


             #### 7. Si no se cumplen supuestos ####

kruskal.test(bateo ~ posicion)  # en caso de No normalidad
oneway.test(bateo ~ posicion)   # en caso de No homocedasticidad
