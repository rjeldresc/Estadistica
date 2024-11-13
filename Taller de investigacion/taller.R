# proyecto de analisis tiempos de respuesta

getwd()
setwd("c:/dev/estadistica/Taller de investigacion/")
## Consultar archivos disponible en la carpeta ##
dir()

library(forecast)
library(lubridate)


datos <- read.csv("tiempos_respuesta.csv", sep=";")

datos$fecha <- ymd_hms(datos$fecha)

library(dplyr)
datos_diarios <- datos %>%
  group_by(fecha = as.Date(fecha)) %>%
  summarize(promedio_respuesta = mean(tiempo_respuesta))

serie_tiempo <- ts(datos_diarios$promedio_respuesta, frequency = 365, start = c(2022, 11, 12))

plot(serie_tiempo, main="Tiempo de Respuesta Diario Promedio", ylab="Tiempo de Respuesta (ms)", xlab="Fecha")
