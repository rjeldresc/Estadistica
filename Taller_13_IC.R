#### INTERVALOS DE CONFIANZA ####
#install.packages("misty")
library(misty)

#ci.mean :     calcula un intervalo de confianza para la media aritmética con desviación estándar 
#              o varianza poblacional conocida o desconocida.
#ci.prop:      calcula un intervalo de confianza para proporciones para una o más variables, 
#ci.var:       calcula el intervalo de confianza para la varianza
#ci.mean.diff: calcula un intervalo de confianza para la diferencia de medias aritméticas en 
#              un diseño de una muestra, dos muestras y muestras pareadas con desviación estándar o 
#              varianza poblacional conocida o desconocida para una o más variables

#EJEMPLOS
#1.
x <- c(1.91,5.29,7.9,7.33,3.86,2.63,14.16,12.16,5.53,6.27,2.34,
       3.1,4.54,9.52,9.79,3.5)

ci.mean(x, conf.level = 0.95)
#la media poblacional es 6.24 con un IC de 4.3 a 8.18

#2.
y <-c(1,1,0,0,1,1,1,1,1,1,1,1,0,0,1,0,0,1,0,0,0,0,0,0,1)
ci.prop(y,conf.level = 0.95, method ="wald")  # asume normal
ci.prop(y,method = "wilson")

# PARA LA VARIANZA
x <- c(1.91,5.29,7.9,7.33,3.86,2.63,14.16,12.16,5.53,6.27,2.34,
       3.1,4.54,9.52,9.79,3.5)
ci.var(x,method = "chisq", conf.level = 0.95)  

