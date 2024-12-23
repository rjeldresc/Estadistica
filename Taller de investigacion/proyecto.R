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


#### 2 Análisis mensual para años 2022 y 2023 (foco en diciembre)

# Cargar librerías necesarias
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(car)      # Para Levene Test
library(emmeans)  # Para análisis post-hoc

# Configurar el directorio de trabajo
setwd("d:/dev/estadistica/Taller de investigacion/")

# Leer los datos
datos <- read.csv("tiempos_respuesta.csv", sep = ";")

# Convertir la columna de fecha a formato datetime
datos$fecha <- ymd_hms(datos$fecha)

# Filtrar datos para noviembre y diciembre de 2022 y 2023
datos_nov_dic <- datos %>%
  filter(
    month(fecha) %in% c(11, 12),      # Seleccionar noviembre y diciembre
    year(fecha) %in% c(2022, 2023)   # Incluir solo 2022 y 2023
  ) %>%
  mutate(
    mes = factor(month(fecha, label = TRUE, abbr = TRUE)),  # Etiqueta mes
    año = factor(year(fecha))                              # Etiqueta año
  )

# Calcular estadísticas descriptivas por año y mes
estadisticas_nov_dic <- datos_nov_dic %>%
  group_by(año, mes) %>%
  summarize(
    promedio = mean(tiempo_respuesta),
    mediana = median(tiempo_respuesta),
    desviacion = sd(tiempo_respuesta),
    n = n()
  )
print(estadisticas_nov_dic)

# Graficar los promedios por año y mes
ggplot(estadisticas_nov_dic, aes(x = mes, y = promedio, fill = año)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Promedio de Tiempos de Respuesta en Noviembre y Diciembre",
    x = "Mes",
    y = "Promedio de Tiempos de Respuesta (ms)",
    fill = "Año"
  ) +
  theme_minimal()

# Preparar datos para ANOVA
anova_data <- datos_nov_dic %>%
  select(año, mes, tiempo_respuesta)

# Prueba de homogeneidad de varianzas
levene_test <- leveneTest(tiempo_respuesta ~ mes * año, data = anova_data)
print("Prueba de homogeneidad de varianzas:")
print(levene_test)

# Realizar ANOVA
anova_model <- aov(tiempo_respuesta ~ año * mes, data = anova_data)
anova_summary <- summary(anova_model)

print("Resultados del ANOVA:")
print(anova_summary)

# Extraer valores p de las pruebas F
p_año <- anova_summary[[1]]$`Pr(>F)`[1]
p_mes <- anova_summary[[1]]$`Pr(>F)`[2]
p_interaccion <- anova_summary[[1]]$`Pr(>F)`[3]

# Verificar significancia
if (p_año < 0.05) {
  print("El factor 'año' tiene un efecto significativo.")
} else {
  print("El factor 'año' no tiene un efecto significativo.")
}

if (p_mes < 0.05) {
  print("El factor 'mes' tiene un efecto significativo.")
} else {
  print("El factor 'mes' no tiene un efecto significativo.")
}

if (p_interaccion < 0.05) {
  print("La interacción entre 'año' y 'mes' tiene un efecto significativo.")
} else {
  print("No hay efecto significativo en la interacción entre 'año' y 'mes'.")
}

# Realizar análisis post-hoc si el ANOVA muestra significancia
if (p_interaccion < 0.05) {
  post_hoc <- emmeans(anova_model, pairwise ~ año * mes)
  print("Resultados del análisis post-hoc:")
  print(post_hoc$contrasts)
}

# Boxplot para observar las distribuciones
ggplot(datos_nov_dic, aes(x = interaction(mes, año), y = tiempo_respuesta, fill = mes)) +
  geom_boxplot() +
  labs(
    title = "Distribución de Tiempos de Respuesta por Mes y Año",
    x = "Mes y Año",
    y = "Tiempo de Respuesta (ms)",
    fill = "Mes"
  ) +
  theme_minimal()



#### Análisis por tamaños de muestra (100, 1000, 4000 registros)

set.seed(20241221)  # Asegurar reproducibilidad
muestras <- list(
  muestra_100 = datos_nov_dic %>% sample_n(100),
  muestra_1000 = datos_nov_dic %>% sample_n(1000),
  muestra_4000 = datos_nov_dic %>% sample_n(4000)
)

resultados_estadisticas <- lapply(muestras, function(muestra) {
  muestra %>%
    group_by(año, mes) %>%
    summarize(
      promedio = mean(tiempo_respuesta),
      mediana = median(tiempo_respuesta),
      desviacion = sd(tiempo_respuesta),
      n = n()
    )
})


resultados_anova <- lapply(muestras, function(muestra) {
  anova_model <- aov(tiempo_respuesta ~ año * mes, data = muestra)
  summary(anova_model)
})


#graficos para resultados_estadisticas


# Librerías necesarias
library(ggplot2)
library(gridExtra)

# Función para generar los gráficos
generar_graficos <- function(datos, titulo_base) {
  # Gráfico de barras con promedios por mes y año
  grafico_barras <- ggplot(datos, aes(x = mes, y = promedio, fill = año, group = año)) +
    geom_bar(stat = "identity", position = position_dodge(0.8), alpha = 0.8) +
    labs(title = paste("Promedios por Mes y Año -", titulo_base),
         x = "Mes",
         y = "Promedio de Tiempos de Respuesta") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Gráfico de barras con barras de error (desviaciones estándar)
  grafico_error_barras <- ggplot(datos, aes(x = mes, y = promedio, fill = año, group = año)) +
    geom_bar(stat = "identity", position = position_dodge(0.8), alpha = 0.8) +
    geom_errorbar(aes(ymin = promedio - desviacion, ymax = promedio + desviacion),
                  position = position_dodge(0.8), width = 0.2, color = "black") +
    labs(title = paste("Promedios y Desviaciones -", titulo_base),
         x = "Mes",
         y = "Promedio de Tiempos de Respuesta") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Gráfico de interacción entre año y mes
  grafico_interaccion <- ggplot(datos, aes(x = mes, y = promedio, color = año, group = año)) +
    geom_line(size = 1.2) +
    geom_point(size = 3) +
    labs(title = paste("Interacción Año y Mes -", titulo_base),
         x = "Mes",
         y = "Promedio de Tiempos de Respuesta") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Devolver los gráficos
  return(list(barras = grafico_barras, error_barras = grafico_error_barras, interaccion = grafico_interaccion))
}

# Generar los gráficos para cada muestra
graficos_muestra_100 <- generar_graficos(resultados_estadisticas$muestra_100, "Muestra 100")
graficos_muestra_1000 <- generar_graficos(resultados_estadisticas$muestra_1000, "Muestra 1000")
graficos_muestra_4000 <- generar_graficos(resultados_estadisticas$muestra_4000, "Muestra 4000")

# Mostrar gráficos para cada muestra en conjuntos de 3
grid.arrange(graficos_muestra_100$barras, graficos_muestra_100$error_barras, graficos_muestra_100$interaccion, ncol = 3)
grid.arrange(graficos_muestra_1000$barras, graficos_muestra_1000$error_barras, graficos_muestra_1000$interaccion, ncol = 3)
grid.arrange(graficos_muestra_4000$barras, graficos_muestra_4000$error_barras, graficos_muestra_4000$interaccion, ncol = 3)


#### tabla resumen

# Crear una función para generar el resumen de estadísticas
generar_tabla_resumen <- function(datos, nombre_muestra) {
  resumen <- datos %>%
    group_by(año, mes) %>%
    summarise(
      promedio = mean(promedio),
      desviacion = mean(desviacion),
      n = mean(n),
      #n = n(),
      .groups = "drop" # Eliminar agrupación y suprimir el mensaje
    ) %>%
    mutate(muestra = nombre_muestra)
  return(resumen)
}

# Generar tablas resumen para las tres muestras
resumen_100 <- generar_tabla_resumen(resultados_estadisticas$muestra_100, "Muestra 100")
resumen_1000 <- generar_tabla_resumen(resultados_estadisticas$muestra_1000, "Muestra 1000")
resumen_4000 <- generar_tabla_resumen(resultados_estadisticas$muestra_4000, "Muestra 4000")

# Combinar las tablas en un único data frame
tabla_resumen <- bind_rows(resumen_100, resumen_1000, resumen_4000)

# Formatear los valores con 2 decimales y usar coma como separador decimal
tabla_resumen <- tabla_resumen %>%
  mutate(
    promedio = format(round(promedio, 2), decimal.mark = ","),
    desviacion = format(round(desviacion, 2), decimal.mark = ","),
    n = round(n, 2) # El tamaño de muestra (n) no necesita coma como separador
  )

# Guardar la tabla en un archivo CSV con ; como separador
write.csv2(tabla_resumen, "resumen_estadisticas.csv", row.names = FALSE)

# Nota: write.csv2 usa automáticamente ; como separador y , como separador decimal.
