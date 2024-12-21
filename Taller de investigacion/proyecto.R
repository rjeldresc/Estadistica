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
  from = as.Date("2022-11-12"),  # Fecha de inicio
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

### 1. Análisis por días críticos (jueves y viernes)


# Cargar librerías
library(forecast)
library(lubridate)
library(dplyr)
library(ggplot2)
library(moments)

# Configuración del directorio
setwd("d:/dev/estadistica/Taller de investigacion/")
dir()

# Cargar los datos
datos <- read.csv("tiempos_respuesta.csv", sep = ";")

# Convertir las columnas a tipos adecuados
datos$fecha <- ymd_hms(datos$fecha)
# Configurar idioma para días de la semana en inglés
Sys.setlocale("LC_TIME", "C")
# Añadir columna con día de la semana
datos <- datos %>%
  mutate(dia_semana = wday(fecha, label = TRUE, abbr = FALSE))  # Añade el día completo

# Filtrar solo los días jueves y viernes
datos_dias_criticos <- datos %>%
  filter(dia_semana %in% c("Thursday", "Friday"))

# Agrupar por fecha y calcular estadísticas descriptivas
estadisticas_dias_criticos <- datos_dias_criticos %>%
  group_by(fecha = as.Date(fecha)) %>%
  summarise(
    promedio_respuesta = mean(tiempo_respuesta, na.rm = TRUE),
    mediana_respuesta = median(tiempo_respuesta, na.rm = TRUE),
    desviacion_estandar = sd(tiempo_respuesta, na.rm = TRUE),
    curtosis = kurtosis(tiempo_respuesta, na.rm = TRUE),
    asimetria = skewness(tiempo_respuesta, na.rm = TRUE),
    n = n()  # Cantidad de registros por día
  )

# Visualización: Serie de tiempo para los jueves y viernes
ggplot(estadisticas_dias_criticos, aes(x = fecha)) +
  geom_line(aes(y = promedio_respuesta, color = "Promedio"), size = 1) +
  geom_line(aes(y = mediana_respuesta, color = "Mediana"), size = 1, linetype = "dashed") +
  labs(
    title = "Tiempos de Respuesta Promedio y Mediana (Jueves y Viernes)",
    x = "Fecha",
    y = "Tiempo de Respuesta (ms)"
  ) +
  scale_color_manual(
    values = c("Promedio" = "blue", "Mediana" = "red"),
    name = "Detalle"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Estadísticas descriptivas generales para jueves y viernes
estadisticas_globales <- datos_dias_criticos %>%
  summarise(
    promedio_global = mean(tiempo_respuesta, na.rm = TRUE),
    mediana_global = median(tiempo_respuesta, na.rm = TRUE),
    desviacion_estandar_global = sd(tiempo_respuesta, na.rm = TRUE),
    curtosis_global = kurtosis(tiempo_respuesta, na.rm = TRUE),
    asimetria_global = skewness(tiempo_respuesta, na.rm = TRUE),
    total_registros = n()
  )

# Imprimir estadísticas globales
print(estadisticas_globales)

# Histograma para visualizar la distribución de los tiempos (Jueves y Viernes)
ggplot(datos_dias_criticos, aes(x = tiempo_respuesta)) +
  geom_histogram(binwidth = 5, fill = "orange", alpha = 0.7, color = "black") +
  labs(
    title = "Distribución de Tiempos de Respuesta (Jueves y Viernes)",
    x = "Tiempo de Respuesta (ms)",
    y = "Frecuencia"
  ) +
  scale_x_continuous(breaks = seq(100, 200, by = 5)) + # Aumentar los intervalos del eje X
  theme_minimal()



