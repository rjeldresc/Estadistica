# proyecto de analisis tiempos de respuesta

library(forecast)
library(lubridate)
library(dplyr)
library(ggplot2)
library(moments)

getwd()
setwd("d:/dev/estadistica/Taller de investigacion/")
dir()

datos <- read.csv("tiempos_respuesta.csv", sep=";")

datos$fecha <- ymd_hms(datos$fecha)

datos_diarios <- datos %>%
  group_by(fecha = as.Date(fecha)) %>%
  summarize(promedio_respuesta = mean(tiempo_respuesta))

serie_tiempo <- ts(datos_diarios$promedio_respuesta, frequency = 365, start = c(2022, 11, 12))

plot(serie_tiempo, main="Tiempo de Respuesta Diario Promedio", ylab="Tiempo de Respuesta (ms)", xlab="Fecha")




# Calcular las fechas reales a partir del objeto ts
fechas <- seq.Date(
  from = as.Date("2022-11-12"),  # Fecha de inicio (según tu serie)
  by = "day",                    # Incremento diario
  length.out = length(serie_tiempo)  # Longitud de la serie
)

# Crear un data frame con fechas y valores
datos_diarios_df <- data.frame(
  fecha = fechas,  # Fechas calculadas
  promedio_respuesta = as.numeric(serie_tiempo)  # Valores numéricos
)

# Graficar con ggplot2
ggplot(datos_diarios_df, aes(x = fecha, y = promedio_respuesta)) +
  geom_line(color = "blue") +
  labs(
    title = "Tiempo de Respuesta Diario Promedio",
    x = "Fecha",
    y = "Tiempo de Respuesta (ms)"
  ) +
  scale_x_date(
    date_breaks = "1 month",       # Etiquetas cada mes
    date_labels = "%b %Y"         # Formato Mes Año
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar etiquetas


#definicion de los 3 casos

#caso 100 registros
set.seed(20241117)
muestra_100 <- datos %>%
  slice_sample(n = 100)
head(muestra_100)


#caso 1000 registros
set.seed(20241118)
muestra_1000 <- datos %>%
  slice_sample(n = 1000)
head(muestra_1000)

#caso 4000 registros
set.seed(20241119)
muestra_4000 <- datos %>%
  slice_sample(n = 4000) 
head(muestra_4000)

# Función para calcular estadísticas por día, incluyendo la mediana
calcular_estadisticas <- function(data) {
  data %>%
    mutate(fecha = as.Date(fecha)) %>%  # Asegurar formato de fecha
    group_by(fecha) %>%                # Agrupar por día
    summarize(
      n = n(),                                             # Tamaño muestral
      promedio = mean(tiempo_respuesta, na.rm = TRUE),     # Promedio
      mediana = median(tiempo_respuesta, na.rm = TRUE),    # Mediana
      desviacion = sd(tiempo_respuesta, na.rm = TRUE),     # Desviación estándar
      asimetria = skewness(tiempo_respuesta, na.rm = TRUE),# Asimetría
      curtosis = kurtosis(tiempo_respuesta, na.rm = TRUE), # Curtosis
      maximo = max(tiempo_respuesta, na.rm = TRUE),        # Máximo
      minimo = min(tiempo_respuesta, na.rm = TRUE)         # Mínimo
    )
}


# Aplicar la función a las 3 muestras
estadisticas_100 <- calcular_estadisticas(muestra_100)
estadisticas_1000 <- calcular_estadisticas(muestra_1000)
estadisticas_4000 <- calcular_estadisticas(muestra_4000)

head(estadisticas_100)
head(estadisticas_1000)
head(estadisticas_4000)


# Cargar las librerías necesarias
library(dplyr)
library(moments)  # Para asimetría y curtosis

# Función para calcular estadísticas globales
calcular_estadisticas_globales <- function(data) {
  data %>%
    summarize(
      promedio = mean(tiempo_respuesta, na.rm = TRUE),     # Promedio
      mediana = median(tiempo_respuesta, na.rm = TRUE),    # Mediana
      desviacion = sd(tiempo_respuesta, na.rm = TRUE),     # Desviación estándar
      asimetria = skewness(tiempo_respuesta, na.rm = TRUE),# Asimetría
      curtosis = kurtosis(tiempo_respuesta, na.rm = TRUE), # Curtosis
      maximo = max(tiempo_respuesta, na.rm = TRUE),        # Máximo
      minimo = min(tiempo_respuesta, na.rm = TRUE)         # Mínimo
    )
}

# Aplicar la función a las 3 muestras
estadisticas_100 <- calcular_estadisticas_globales(muestra_100)
estadisticas_1000 <- calcular_estadisticas_globales(muestra_1000)
estadisticas_4000 <- calcular_estadisticas_globales(muestra_4000)

# Ver resultados
estadisticas_100
estadisticas_1000
estadisticas_4000


# Función para calcular estadísticas combinadas de una muestra
calcular_estadisticas <- function(datos) {
  promedio <- mean(datos$tiempo_respuesta, na.rm = TRUE)
  mediana <- median(datos$tiempo_respuesta, na.rm = TRUE)
  desviacion <- sd(datos$tiempo_respuesta, na.rm = TRUE)
  asimetria <- moments::skewness(datos$tiempo_respuesta, na.rm = TRUE)
  curtosis <- moments::kurtosis(datos$tiempo_respuesta, na.rm = TRUE)
  maximo <- max(datos$tiempo_respuesta, na.rm = TRUE)
  minimo <- min(datos$tiempo_respuesta, na.rm = TRUE)
  
  tibble(
    promedio = promedio,
    mediana = mediana,
    desviacion = desviacion,
    asimetria = asimetria,
    curtosis = curtosis,
    maximo = maximo,
    minimo = minimo
  )
}

# Cálculo de estadísticas para cada muestra
estadisticas_100 <- calcular_estadisticas(muestra_100)
estadisticas_1000 <- calcular_estadisticas(muestra_1000)
estadisticas_4000 <- calcular_estadisticas(muestra_4000)

# Ver resultados
estadisticas_100
estadisticas_1000
estadisticas_4000

# Cargar las librerías necesarias
library(ggplot2)

# Crear histogramas para cada muestra
# Histograma para muestra_100
ggplot(muestra_100, aes(x = tiempo_respuesta)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(
    title = "Distribución de Tiempos de Respuesta - Muestra 100",
    x = "Tiempo de Respuesta (ms)",
    y = "Frecuencia"
  ) +
  theme_minimal()

# Histograma para muestra_1000
ggplot(muestra_1000, aes(x = tiempo_respuesta)) +
  geom_histogram(binwidth = 5, fill = "green", color = "black", alpha = 0.7) +
  labs(
    title = "Distribución de Tiempos de Respuesta - Muestra 1000",
    x = "Tiempo de Respuesta (ms)",
    y = "Frecuencia"
  ) +
  theme_minimal()

# Histograma para muestra_4000
ggplot(muestra_4000, aes(x = tiempo_respuesta)) +
  geom_histogram(binwidth = 5, fill = "orange", color = "black", alpha = 0.7) +
  labs(
    title = "Distribución de Tiempos de Respuesta - Muestra 4000",
    x = "Tiempo de Respuesta (ms)",
    y = "Frecuencia"
  ) +
  theme_minimal()


#### analisis para los 3 casos


