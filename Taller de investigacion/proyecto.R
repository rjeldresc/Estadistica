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
summary(datos$tiempo_respuesta)  # Ver valores mínimos, medianos y máximos
boxplot(datos$tiempo_respuesta)  # Ver si hay valores atípicos
hist(datos$tiempo_respuesta, breaks = 30)  # Ver la distribución

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
    geom_line(linewidth  = 1.2) +
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



# Dataframe original se llama "datos"
# Convertir la columna fecha a formato de fecha y hora
datos2 <- datos %>%
  mutate(fecha = as.POSIXct(fecha, format = "%Y-%m-%d %H:%M:%S"))

# Agregar las nuevas columnas al dataframe
datos_transformado <- datos2 %>%
  mutate(
    DiaSemana = weekdays(fecha, abbreviate = FALSE), # Nombre del día
    DiaCritico = ifelse(DiaSemana %in% c("jueves", "viernes"), 1, 0), # 1 si es jueves o viernes
    DiaNumerico = wday(fecha, label = FALSE, week_start = 1), # Día numérico (1 = lunes, 7 = domingo)
    DiaNumericoMes = day(fecha)    ,                    # Día del mes
    MesNumerico = month(fecha),                         # Número del mes (1 = enero, 12 = diciembre)
    SemanaNumerico = isoweek(fecha),                    # Número de la semana (ISO)
    Anio = year(fecha)                                  # Año de la fecha
    )

# Visualizar los primeros registros
head(datos_transformado)


# Agrupar por día y calcular la mediana
datos_agrupados <- datos_transformado %>%
  mutate(Dia = as.Date(fecha)) %>% # Extraer la fecha sin hora
  group_by(Dia) %>%               # Agrupar por día
  summarise(
    tiempo_respuesta_MEDIANA = median(tiempo_respuesta, na.rm = TRUE), # Mediana del tiempo de respuesta
    DiaSemana = first(DiaSemana),      # Mantener el nombre del día
    DiaCritico = first(DiaCritico),    # Mantener si es día crítico
    DiaNumerico = first(DiaNumerico),  # Día de la semana
    DiaNumericoMes = first(DiaNumericoMes), # Día del mes
    MesNumerico = first(MesNumerico),  # Número del mes
    SemanaNumerico = first(SemanaNumerico), # Número de la semana
    Anio = first(Anio)                 # Año
  ) %>%
  ungroup() # Desagrupar

# Visualizar los primeros registros
head(datos_agrupados)

datos_agrupados <- datos_agrupados %>%
  mutate(Noviembre = ifelse(MesNumerico == 11, 1, 0),
         mes = factor(ifelse(MesNumerico == 11, 12, MesNumerico))) # Cambiar noviembre a diciembre


# Verificar si se agregó correctamente
head(datos_agrupados)
colnames(datos_agrupados)

# Guardar el dataframe en un archivo CSV con separador de punto y coma
#write.csv(datos_agrupados, "datos_agrupados.csv", row.names = FALSE, sep=";")

# Guardar el dataframe con punto y coma como separador
write.table(datos_agrupados, "datos_agrupados.csv", row.names = FALSE, sep = ";", dec = ".", quote = TRUE)

#?write.csv

# Verificar los nombres de las columnas en datos_agrupados
colnames(datos_agrupados)


##1

# Convertir 'Dia' a formato numérico continuo
tiempo <- as.numeric(datos_agrupados$Dia - min(datos_agrupados$Dia))

# Verificar el rango de 'tiempo' para asegurarse que esté correcto
range(tiempo)





##2
# Ajuste del modelo de regresión lineal
mod <- lm(tiempo_respuesta_MEDIANA ~ tiempo, data = datos_agrupados)

# Resumen del modelo
summary(mod)



##3
# Graficar los datos originales
plot(datos_agrupados$tiempo_respuesta_MEDIANA ~ datos_agrupados$Dia, 
     type = "l", col = "black", 
     xlab = "Fecha", ylab = "Tiempo de Respuesta (Mediana)", 
     main = "Tiempo de Respuesta vs Fecha")

# Añadir la línea ajustada (modelo de regresión)
lines(datos_agrupados$Dia, mod$fitted.values, col = "red", lwd = 2)


##4 Ajustar el modelo con el efecto del mes:
# Crear la variable 'mes' (el número de mes para cada fecha)
mes <- factor(format(datos_agrupados$Dia, "%m"))

# Ajustar el modelo con 'tiempo' y 'mes' como variables

mod2 <- lm(tiempo_respuesta_MEDIANA ~ (tiempo * mes * Anio) * Noviembre, data = datos_agrupados)


# Definir el archivo de salida
sink("resumen_modelo_mod2.txt")

# Resumen del modelo con el efecto de los meses
summary(mod2)

# Detener la captura de salida
sink()


# Ajustar el modelo de spline
spline_mod <- smooth.spline(datos_agrupados$Dia, datos_agrupados$tiempo_respuesta_MEDIANA)

sink("spline_mod.txt")
# Verificar el modelo ajustado
summary(spline_mod)
# Detener la captura de salida
sink()

# Crear el gráfico con ggplot2
ggplot(datos_agrupados, aes(x = Dia, y = tiempo_respuesta_MEDIANA)) +
  geom_line(color = "black") +  # Línea de los datos originales
  geom_line(aes(x = Dia, y = spline_mod$y), color = "orange", linewidth = 1.2) + # Línea del spline ajustado
  labs(
    title = "Tiempo de Respuesta vs Fecha con Spline Ajustado",
    x = "Fecha", y = "Tiempo de Respuesta (Mediana)"
  ) +
  scale_x_date(
    breaks = "1 month",  # Establecer las marcas cada mes
    labels = scales::date_format("%b-%Y")  # Formato mes-año
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Incluir rotación de etiquetas


##6

# # Graficar los datos originales
# plot(datos_agrupados$tiempo_respuesta_MEDIANA ~ datos_agrupados$Dia, 
#      type = "l", col = "black", 
#      xlab = "Fecha", ylab = "Tiempo de Respuesta (Mediana)", 
#      main = "Tiempo de Respuesta vs Fecha con Ajuste")
# 
# # Añadir la línea ajustada con mod2
# lines(datos_agrupados$Dia, mod2$fitted.values, col = "red", lwd = 2)

library(ggplot2)

# Crear el gráfico con ggplot2 para el "mod2"
ggplot(datos_agrupados, aes(x = Dia, y = tiempo_respuesta_MEDIANA)) +
  geom_line(color = "black") + # Línea de los datos originales
  geom_line(aes(y = mod2$fitted.values), color = "red", linewidth = 1.2) + # Línea ajustada
  labs(
    title = "Tiempo de Respuesta vs Fecha con Ajuste",
    x = "Fecha", y = "Tiempo de Respuesta (Mediana)"
  ) +
  scale_x_date(
    breaks = "1 month",  # Establecer las marcas cada mes
    labels = scales::date_format("%b-%Y")  # Formato mes-año
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Incluir rotación de etiquetas



write.table(datos_agrupados, "datos_agrupados.csv", row.names = FALSE, sep = ";", dec = ".", quote = TRUE)

## 7 Predicciones para el año 2025


# Crear un rango de fechas para predicción
fechas_prediccion <- seq(from = as.Date("2024-11-12"), to = as.Date("2025-06-30"), by = "days")

# Calcular 'tiempo2' para estas fechas
tiempo2 <- as.numeric(fechas_prediccion - min(datos_agrupados$Dia))

# Crear el factor 'mes' para estas fechas
mes <- factor(format(fechas_prediccion, "%m"))

# Crear la tabla de predicción
tabla_prediccion <- data.frame(tiempo = tiempo2, mes = mes)

# Generar las predicciones usando el modelo ajustado
predicciones <- predict(mod2, newdata = tabla_prediccion)

# Agregar las predicciones al dataframe
tabla_prediccion$prediccion <- predicciones

# # Graficar los resultados
# plot(datos_agrupados$tiempo_respuesta_MEDIANA ~ datos_agrupados$Dia, 
#      type = "l", col = "black", 
#      xlab = "Fecha", ylab = "Tiempo de Respuesta (Mediana)", 
#      main = "Predicción de Tiempos de Respuesta para 2024-2025")
# 
# # Línea ajustada para los datos históricos
# lines(datos_agrupados$Dia, mod2$fitted.values, col = "red", lwd = 2)
# 
# # Superponer las predicciones
# lines(fechas_prediccion, predicciones, col = "orange", lwd = 4)

# # Guardar el dataframe de predicción como CSV
# write.csv(tabla_prediccion, "tabla_prediccion.csv", row.names = FALSE)
# 
# 
# # Verificar las fechas de predicción
# print(head(fechas_prediccion))
# print(tail(fechas_prediccion))
# 
# # Verificar las predicciones generadas
# print(head(predicciones))
# print(tail(predicciones))
# 
# # Verificar el rango del eje x en el gráfico
# print(range(datos_agrupados$Dia))
# print(range(fechas_prediccion))
# 
# plot(  predicciones ~ fechas_prediccion)

# # Graficar datos históricos
# plot(datos_agrupados$tiempo_respuesta_MEDIANA ~ datos_agrupados$Dia, 
#      type = "l", col = "black", 
#      xlab = "Fecha", ylab = "Tiempo de Respuesta (Mediana)", 
#      main = "Predicción de Tiempos de Respuesta para 2024-2025",
#      xlim = range(c(datos_agrupados$Dia, fechas_prediccion))) # Ajustar rango del eje x
# 
# # Añadir línea ajustada para los datos históricos
# lines(datos_agrupados$Dia, mod2$fitted.values, col = "red", lwd = 2)
# 
# # Añadir las predicciones
# lines(fechas_prediccion, predicciones, col = "blue", lwd = 2)

# Graficar datos históricos con rango ajustado
plot(datos_agrupados$tiempo_respuesta_MEDIANA ~ datos_agrupados$Dia, 
     type = "l", col = "black", 
     xlab = "Fecha", ylab = "Tiempo de Respuesta (Mediana)", 
     main = "Predicción de Tiempos de Respuesta para 2024-2025",
     xlim = range(c(datos_agrupados$Dia, fechas_prediccion)), # Asegurarse de incluir el rango de fechas
     ylim = range(c(datos_agrupados$tiempo_respuesta_MEDIANA, predicciones))) # Asegurar que los valores predichos entren en el eje Y

# Añadir la línea del modelo ajustado
lines(datos_agrupados$Dia, mod2$fitted.values, col = "red", lwd = 2)

# Verificar la alineación de las fechas y las predicciones
# print(length(fechas_prediccion))
# print(length(predicciones))

# Añadir las predicciones al gráfico
lines(fechas_prediccion, predicciones, col = "blue", lwd = 2)


## prediccion

#### ejemplo 1
library(ggplot2)

# Crear un data frame combinado con los datos históricos y las predicciones
datos_historicos <- data.frame(
  Fecha = datos_agrupados$Dia,
  TiempoRespuesta = datos_agrupados$tiempo_respuesta_MEDIANA,
  Tipo = "Histórico"
)

datos_predicciones <- data.frame(
  Fecha = fechas_prediccion,
  TiempoRespuesta = predicciones,
  Tipo = "Predicción"
)

datos_combinados <- rbind(datos_historicos, datos_predicciones)

# Graficar con ggplot2
ggplot(datos_combinados, aes(x = Fecha, y = TiempoRespuesta, color = Tipo)) +
  geom_line(size = 1) + # Líneas para ambos tipos de datos
  scale_color_manual(values = c("Histórico" = "black", "Predicción" = "blue")) + # Colores personalizados
  labs(
    title = "Predicción de Tiempos de Respuesta para 2024-2025",
    x = "Fecha",
    y = "Tiempo de Respuesta (Mediana)"
  ) +
  scale_x_date(
    date_labels = "%Y-%m", # Mostrar años y meses en el eje X
    date_breaks = "1 month" # Saltos de un mes
  ) +
  theme_minimal() + # Tema minimalista
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotar etiquetas del eje X
    plot.title = element_text(hjust = 0.5) # Centrar el título
  )


# library(ggplot2)
# 
# # Calcular la mediana de los tiempos históricos
# mediana_historica <- median(datos_agrupados$tiempo_respuesta_MEDIANA)
# 
# # Crear un data frame combinado con los datos históricos y las predicciones
# datos_historicos <- data.frame(
#   Fecha = datos_agrupados$Dia,
#   TiempoRespuesta = datos_agrupados$tiempo_respuesta_MEDIANA,
#   Tipo = "Histórico"
# )
# 
# datos_predicciones <- data.frame(
#   Fecha = fechas_prediccion,
#   TiempoRespuesta = predicciones,
#   Tipo = "Predicción"
# )
# 
# datos_combinados <- rbind(datos_historicos, datos_predicciones)

# Graficar con ggplot2
# ggplot(datos_combinados, aes(x = Fecha, y = TiempoRespuesta, color = Tipo)) +
#   geom_line(size = 1) + # Líneas para datos históricos y predicciones
#   scale_color_manual(values = c("Histórico" = "black", "Predicción" = "blue")) + # Colores personalizados
#   geom_hline(yintercept = mediana_historica, color = "red", linetype = "dashed", size = 1) + # Línea roja para la mediana
#   labs(
#     title = "Predicción de Tiempos de Respuesta para 2024-2025",
#     x = "Fecha",
#     y = "Tiempo de Respuesta (Mediana)"
#   ) +
#   scale_x_date(
#     date_labels = "%Y-%m", # Mostrar años y meses en el eje X
#     date_breaks = "1 month" # Saltos de un mes
#   ) +
#   theme_minimal() + # Tema minimalista
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1), # Rotar etiquetas del eje X
#     plot.title = element_text(hjust = 0.5) # Centrar el título
#   )



#### ejemplo 2
library(ggplot2)

# Convertir fechas a formato Date si no lo están
datos_agrupados$Dia <- as.Date(datos_agrupados$Dia)
fechas_prediccion <- as.Date(fechas_prediccion)

# Crear un dataframe para las predicciones
df_predicciones <- data.frame(
  Dia = fechas_prediccion,
  tiempo_respuesta_MEDIANA = predicciones
)

# Crear el gráfico con ggplot2
ggplot() +
  # Datos históricos (línea negra)
  geom_line(data = datos_agrupados, 
            aes(x = Dia, y = tiempo_respuesta_MEDIANA), 
            color = "black") +
  
  # Línea roja del modelo ajustado (mediana histórica)
  geom_line(data = datos_agrupados, 
            aes(x = Dia, y = mod2$fitted.values), 
            color = "red", size = 1.2) +
  
  # Línea azul de las predicciones
  geom_line(data = df_predicciones, 
            aes(x = Dia, y = tiempo_respuesta_MEDIANA), 
            color = "blue", size = 1.2) +
  
  # Personalización de las escalas
  scale_x_date(
    date_labels = "%Y-%m", # Etiquetas por año y mes
    date_breaks = "1 month" # Detalles de las fechas por mes
  ) +
  
  # Etiquetas y título
  labs(
    title = "Predicción de Tiempos de Respuesta para 2024-2025",
    x = "Fecha",
    y = "Tiempo de Respuesta (Mediana)"
  ) +
  
  # Tema
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotar etiquetas del eje X
    plot.title = element_text(hjust = 0.5, size = 14)  # Centrar título
  ) +
  
  # Agregar una leyenda manual
  scale_color_manual(
    values = c("black", "red", "blue"),
    name = "Leyenda",
    labels = c("Histórico", "Mediana Ajustada", "Predicción")
  )





#Análisis estacional

# library(forecast)
# 
# # Crear serie de tiempo mensual
# datos_mensuales <- datos %>%
#   mutate(anio_mes = floor_date(fecha, "month")) %>%
#   group_by(anio_mes) %>%
#   summarise(promedio = mean(tiempo_respuesta, na.rm = TRUE))
# 
# # Convertir a serie de tiempo
# ts_mensual <- ts(datos_mensuales$promedio, start = c(year(min(datos$fecha)), month(min(datos$fecha))), frequency = 12)
# 
# # Descomposición de la serie
# descomposicion <- decompose(ts_mensual)
# 
# # Graficar la descomposición
# plot(descomposicion)
# 
# # Modelo ARIMA para análisis de patrones estacionales
# modelo_arima <- auto.arima(ts_mensual)
# summary(modelo_arima)
# 
# # Predicción
# predicciones <- forecast(modelo_arima, h = 12)  # Predicción para 12 meses
# autoplot(predicciones)




#### serie de tiempo 2025-02-07 ####

# Librerías necesarias
library(tidyverse)
library(lubridate)
library(forecast)
library(tseries)

# Establecer directorio de trabajo (ajústalo según corresponda)
setwd("d:/dev/estadistica/Taller de investigacion/")

# Cargar los datos
datos <- read.csv("tiempos_respuesta.csv", sep=";")

# Convertir la columna de fecha a formato de fecha y hora
datos$fecha <- ymd_hms(datos$fecha)

# Agregar datos por mes y calcular el promedio del tiempo de respuesta
datos_mensuales <- datos %>%
  mutate(anio_mes = floor_date(fecha, "month")) %>%
  group_by(anio_mes) %>%
  summarise(promedio = mean(tiempo_respuesta, na.rm = TRUE))

# Convertir a serie de tiempo
ts_mensual <- ts(datos_mensuales$promedio, start = c(year(min(datos$fecha)), month(min(datos$fecha))), frequency = 12)

# Visualizar los primeros datos
head(ts_mensual)

# Graficar la serie de tiempo original
autoplot(ts_mensual) + 
  ggtitle("Evolución Mensual del Tiempo de Respuesta") + 
  ylab("Tiempo de Respuesta (ms)") + 
  xlab("Fecha")

# Prueba de Dickey-Fuller aumentada (ADF)
adf.test(ts_mensual)


# Modelo ARIMA sin diferenciación (d=0)
modelo_arima_d0 <- auto.arima(ts_mensual, d=0)

# Modelo ARIMA con una diferenciación (d=1)
modelo_arima_d1 <- auto.arima(ts_mensual, d=1)

# Modelo ARIMA con dos diferenciaciones (d=2)
modelo_arima_d2 <- auto.arima(ts_mensual, d=2)

# Predicción para 7 meses (diciembre 2024 - junio 2025)
pred_d0 <- forecast(modelo_arima_d0, h=7)
pred_d1 <- forecast(modelo_arima_d1, h=7)
pred_d2 <- forecast(modelo_arima_d2, h=7)


# Cargar librerías necesarias
library(ggplot2)
library(ggrepel)  # Para evitar superposición de etiquetas

# Crear un dataframe con todas las predicciones
df_pred <- data.frame(
  Fecha = seq(from = tail(datos_mensuales$anio_mes, 1) + months(1), length.out = 7, by = "months"),
  Predicción_d0 = pred_d0$mean,
  Predicción_d1 = pred_d1$mean,
  Predicción_d2 = pred_d2$mean
)

# Convertir fechas a formato Date para mejor visualización
df_pred$Fecha <- as.Date(df_pred$Fecha)
# Convertir fechas de datos históricos a formato Date
datos_mensuales$anio_mes <- as.Date(datos_mensuales$anio_mes)

# Graficar con etiquetas y detalles adicionales
ggplot() +
  # Datos históricos
  geom_line(data = datos_mensuales, aes(x = anio_mes, y = promedio), color = "black", size = 1, linetype = "solid") +
  
  # Predicción sin diferenciación (d=0) - Azul
  geom_line(data = df_pred, aes(x = Fecha, y = Predicción_d0), color = "blue", size = 1, linetype = "dashed") +
  geom_text_repel(data = df_pred, aes(x = Fecha, y = Predicción_d0, label = round(Predicción_d0, 1)), color = "blue") +
  
  # Predicción con d=1 - Roja
  geom_line(data = df_pred, aes(x = Fecha, y = Predicción_d1), color = "red", size = 1, linetype = "dashed") +
  geom_text_repel(data = df_pred, aes(x = Fecha, y = Predicción_d1, label = round(Predicción_d1, 1)), color = "red") +
  
  # Predicción con d=2 - Verde
  geom_line(data = df_pred, aes(x = Fecha, y = Predicción_d2), color = "green", size = 1, linetype = "dashed") +
  geom_text_repel(data = df_pred, aes(x = Fecha, y = Predicción_d2, label = round(Predicción_d2, 1)), color = "green") +
  
  # Ajustes del gráfico
  ggtitle("Comparación de Predicciones con Diferentes Diferenciaciones") +
  xlab("Fecha") +
  ylab("Tiempo de Respuesta (ms)") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y") +  # Espaciado mensual en eje X
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Inclinación del eje X
  
  # Agregar leyenda manualmente
  annotate("text", x = max(df_pred$Fecha), y = max(df_pred$Predicción_d0), label = "ARIMA d=0", color = "blue", hjust = 1) +
  annotate("text", x = max(df_pred$Fecha), y = max(df_pred$Predicción_d1), label = "ARIMA d=1", color = "red", hjust = 1) +
  annotate("text", x = max(df_pred$Fecha), y = min(df_pred$Predicción_d2), label = "ARIMA d=2", color = "green", hjust = 1)


# Cargar funciones adicionales
source("summary.arima.R")
source("TS.diag.R")

ts_mensual_diff2 <- diff(ts_mensual, differences=2)
adf.test(ts_mensual_diff2)
#p-valor = 0.01254  serie con 2 diferenciaciones es estacionaria


# modelo_arima <- auto.arima(ts_mensual, d=2)
# summary(modelo_arima)
# modelo_arima$coef

modelo_arima <- Arima(ts_mensual, order = c(1,2,1))
summary(modelo_arima)
modelo_arima$coef

# modelo_arima <- Arima(ts_mensual, order = c(1,1,1))
# summary(modelo_arima)
# modelo_arima$coef

#Resumen del modelo con detalles de coeficientes y errores estándar
summary_arima(modelo_arima, fixed = rep(NA, length(modelo_arima$coef)))

#Prueba de blancura (Box-Ljung Test) para validar residuos
Box.Ljung.Test(modelo_arima$residuals, lag = 12)

#Diagnóstico de residuos (Autocorrelación, normalidad, histograma, etc.)
#TS.diag(modelo_arima$residuals, lag = 12)
#TS.diag(modelo_arima, lag = 12)

tsdiag(modelo_arima, gof.lag = 12)

#Validación de normalidad y homocedasticidad
ks.test(scale(modelo_arima$residuals), "pnorm")  # Normalidad
lmtest::bptest(lm(modelo_arima$residuals ~ time(modelo_arima$residuals)))  # Homocedasticidad


#Aplicar Transformación Box-Cox para normalizar residuos
lambda <- BoxCox.lambda(ts_mensual)  # Calcular parámetro de transformación
ts_bc <- BoxCox(ts_mensual, lambda)  # Aplicar transformación

# Ajustar modelo con datos transformados
modelo_bc <- auto.arima(ts_bc, d=2)
summary(modelo_bc)

# Validar normalidad en residuos del nuevo modelo
ks.test(scale(modelo_bc$residuals), "pnorm")


#### modelo_final ####
modelo_final <- Arima(ts_mensual, order = c(1,2,1))
summary(modelo_final)
modelo_final$coef
ks.test(scale(modelo_final$residuals), "pnorm")  # Normalidad
lmtest::bptest(lm(modelo_final$residuals ~ time(modelo_final$residuals)))  # Homocedasticidad

pred_final <- forecast(modelo_final, h = 7)

autoplot(pred_final) + 
  ggtitle("Predicción de Tiempo de Respuesta (Dic 2024 - Jun 2025)") + 
  ylab("Tiempo de Respuesta (ms)") + 
  xlab("Fecha")



#### final final serie de tiempo ####


# Cargar librerías necesarias
library(tidyverse)
library(lubridate)
library(forecast)
library(tseries)
library(lmtest)
library(ggplot2)
library(ggrepel)

# Cargar las funciones del profesor
source("summary.arima.R")
source("TS.diag.R")

# Establecer directorio de trabajo
setwd("d:/dev/estadistica/Taller de investigacion/")

# Cargar los datos
datos <- read.csv("tiempos_respuesta.csv", sep=";")

# Convertir la columna de fecha a formato Date
datos$fecha <- ymd_hms(datos$fecha)

# Agregar datos por mes y calcular el promedio del tiempo de respuesta
datos_mensuales <- datos %>%
  mutate(anio_mes = floor_date(fecha, "month")) %>%
  group_by(anio_mes) %>%
  summarise(promedio = mean(tiempo_respuesta, na.rm = TRUE))

# Convertir a serie de tiempo
ts_mensual <- ts(datos_mensuales$promedio, start = c(year(min(datos$fecha)), month(min(datos$fecha))), frequency = 12)

# Graficar la serie de tiempo original
autoplot(ts_mensual) + 
  ggtitle("Evolución Mensual del Tiempo de Respuesta") + 
  ylab("Tiempo de Respuesta (ms)") + 
  xlab("Fecha")

# Prueba de Dickey-Fuller Aumentada (ADF)
adf.test(ts_mensual)

# Aplicar primera diferenciación
ts_mensual_diff <- diff(ts_mensual)

# Prueba ADF después de la primera diferenciación
adf.test(ts_mensual_diff)

# Aplicar segunda diferenciación
ts_mensual_diff2 <- diff(ts_mensual_diff)

# Prueba ADF después de la segunda diferenciación
adf.test(ts_mensual_diff2)

# Ajustar modelo ARIMA(1,2,1)
modelo_final <- Arima(ts_mensual, order = c(1,2,1))

# Ver resumen del modelo con la función personalizada del profesor
summary_arima(modelo_final, fixed = rep(NA, length(modelo_final$coef)))

# Diagnóstico de residuos usando la función TS.diag del profesor
TS.diag(modelo_final, lag = 12)

tsdiag(modelo_final, lag = 12)

# Prueba de Box-Ljung para confirmar blancura de residuos
Box.Ljung.Test(modelo_final$residuals, lag = 12)

# Prueba de Normalidad
ks.test(scale(modelo_final$residuals), "pnorm")

# Prueba de Homocedasticidad
lmtest::bptest(lm(modelo_final$residuals ~ time(modelo_final$residuals)))

# 📌 Cargar librerías necesarias
library(tidyverse)
library(lubridate)
library(forecast)
library(tseries)
library(lmtest)
library(ggplot2)
library(ggrepel)

# 📌 Establecer directorio de trabajo
setwd("d:/dev/estadistica/Taller de investigacion/")

# 📌 Cargar los datos
datos <- read.csv("tiempos_respuesta.csv", sep=";")

# 📌 Convertir la columna de fecha a formato Date
datos$fecha <- ymd_hms(datos$fecha)

# 📌 Agregar datos por mes y calcular el promedio del tiempo de respuesta
datos_mensuales <- datos %>%
  mutate(anio_mes = floor_date(fecha, "month")) %>%
  group_by(anio_mes) %>%
  summarise(promedio = mean(tiempo_respuesta, na.rm = TRUE))

# 📌 Verificar los últimos registros
tail(datos_mensuales, 2)  # Se asegura de que noviembre 2024 está en la serie con su valor de 143 ms

# 📌 Convertir a serie de tiempo
ts_mensual <- ts(datos_mensuales$promedio, start = c(year(min(datos$fecha)), month(min(datos$fecha))), frequency = 12)

# 📌 Ajustar el modelo ARIMA(1,2,1)
modelo_final <- Arima(ts_mensual, order = c(1,2,1))

# 📌 Generar predicción para los próximos 7 meses
pred_final <- forecast(modelo_final, h = 7)

# 📌 Generar fechas correctas para la predicción
df_pred <- data.frame(
  Fecha = seq(from = as.Date(max(datos_mensuales$anio_mes)) %m+% months(1), 
              by = "month", length.out = 7),
  Predicción = as.numeric(pred_final$mean),
  LI_80 = as.numeric(pred_final$lower[,1]),
  LS_80 = as.numeric(pred_final$upper[,1]),
  LI_95 = as.numeric(pred_final$lower[,2]),
  LS_95 = as.numeric(pred_final$upper[,2])
)

# 📌 Última fila para etiquetar los intervalos de confianza en junio 2025
df_ultimo <- df_pred %>% tail(1)

# 📌 Gráfico Mejorado
ggplot() +
  # 📌 Datos históricos
  geom_line(data = datos_mensuales, aes(x = as.Date(anio_mes), y = promedio), color = "black", size = 1) +
  
  # 📌 Intervalo de confianza 95%
  geom_ribbon(data = df_pred, aes(x = Fecha, ymin = LI_95, ymax = LS_95), fill = "lightblue", alpha = 0.4) +
  
  # 📌 Intervalo de confianza 80%
  geom_ribbon(data = df_pred, aes(x = Fecha, ymin = LI_80, ymax = LS_80), fill = "blue", alpha = 0.4) +
  
  # 📌 Línea de predicción
  geom_line(data = df_pred, aes(x = Fecha, y = Predicción), color = "blue", size = 1) +
  
  # 📌 Agregar etiquetas para la predicción final en junio 2025
  geom_text_repel(data = df_ultimo, aes(x = Fecha, y = Predicción, label = round(Predicción, 1)), 
                  color = "blue", size = 5, fontface = "bold", nudge_y = 5) +
  
  geom_text_repel(data = df_ultimo, aes(x = Fecha, y = LS_80, label = round(LS_80, 1)), 
                  color = "darkblue", size = 4, fontface = "italic", nudge_y = 5) +
  
  geom_text_repel(data = df_ultimo, aes(x = Fecha, y = LI_80, label = round(LI_80, 1)), 
                  color = "darkblue", size = 4, fontface = "italic", nudge_y = -5) +
  
  geom_text_repel(data = df_ultimo, aes(x = Fecha, y = LS_95, label = round(LS_95, 1)), 
                  color = "darkblue", size = 4, fontface = "italic", nudge_y = 5) +
  
  geom_text_repel(data = df_ultimo, aes(x = Fecha, y = LI_95, label = round(LI_95, 1)), 
                  color = "darkblue", size = 4, fontface = "italic", nudge_y = -5) +
  
  # 📌 Ajustes del gráfico
  ggtitle("Predicción del Tiempo de Respuesta (Dic 2024 - Jun 2025)") +
  xlab("Tiempo") +
  ylab("Tiempo de Respuesta (ms)") +
  
  # 📌 Ajuste de eje X para mostrar meses correctamente
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y") +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#### grafico comparacion con diferenciaciones ####

# Cargar librerías necesarias
library(ggplot2)
library(ggrepel)
library(dplyr)
library(forecast)
library(lubridate)

# 📌 Ajustar modelos ARIMA con diferentes diferenciaciones
modelo_d0 <- auto.arima(ts_mensual, d=0, seasonal = FALSE)
modelo_d1 <- auto.arima(ts_mensual, d=1, seasonal = FALSE)
modelo_d2 <- auto.arima(ts_mensual, d=2, seasonal = FALSE)

# 📌 Generar predicciones para cada modelo
pred_d0 <- forecast(modelo_d0, h = 7)
pred_d1 <- forecast(modelo_d1, h = 7)
pred_d2 <- forecast(modelo_d2, h = 7)

# 📌 Generar las fechas de predicción correctas
fechas_pred <- seq(from = as.Date(max(datos_mensuales$anio_mes)) %m+% months(1), 
                   by = "month", length.out = 7)

# 📌 Crear un dataframe con todas las predicciones y agregar una columna "Modelo"
df_pred <- bind_rows(
  data.frame(Fecha = fechas_pred, Predicción = as.numeric(pred_d0$mean), Modelo = "ARIMA d=0"),
  data.frame(Fecha = fechas_pred, Predicción = as.numeric(pred_d1$mean), Modelo = "ARIMA d=1"),
  data.frame(Fecha = fechas_pred, Predicción = as.numeric(pred_d2$mean), Modelo = "ARIMA d=2")
)

# 📌 Definir los colores y estilos de línea por modelo
colores <- c("ARIMA d=0" = "blue", "ARIMA d=1" = "red", "ARIMA d=2" = "green")
lineas <- c("ARIMA d=0" = "dashed", "ARIMA d=1" = "dashed", "ARIMA d=2" = "dashed")

# 📌 Gráfico mejorado con leyenda corregida
ggplot() +
  # 📌 Datos históricos
  geom_line(data = datos_mensuales, aes(x = as.Date(anio_mes), y = promedio), 
            color = "black", size = 1) +
  
  # 📌 Predicciones con leyenda corregida
  geom_line(data = df_pred, aes(x = Fecha, y = Predicción, color = Modelo, linetype = Modelo), size = 1.2) +
  
  # 📌 Etiquetas para cada modelo
  geom_text_repel(data = df_pred, aes(x = Fecha, y = Predicción, label = round(Predicción, 1), color = Modelo),
                  size = 5, fontface = "bold") +
  
  # 📌 Ajustes del gráfico
  ggtitle("Comparación de Predicciones con Diferentes Diferenciaciones") +
  xlab("Fecha") +
  ylab("Tiempo de Respuesta (ms)") +
  
  # 📌 Ajuste de colores y estilos en la leyenda
  scale_color_manual(name = "Modelos ARIMA", values = colores) +
  scale_linetype_manual(name = "Modelos ARIMA", values = lineas) +
  
  # 📌 Corregir la leyenda para que los colores aparezcan correctamente
  guides(color = guide_legend(override.aes = list(linetype = "solid", size = 2)), 
         linetype = guide_legend(override.aes = list(size = 1.2))) +
  
  # 📌 Ajuste del eje X para mostrar meses claramente
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y") +
  
  # 📌 Ajustes de tema para mejorar la claridad
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",  # Mueve la leyenda abajo del gráfico
        legend.title = element_text(size = 12, face = "bold"),  # Agranda el título de la leyenda
        legend.text = element_text(size = 11),  # Agranda los textos de la leyenda
        legend.background = element_rect(fill = "white", color = "black"))  # Fondo blanco para la leyenda


#### serie de tiempo usando la mediana ####

# 📌 Cargar librerías necesarias
library(tidyverse)
library(lubridate)
library(forecast)
library(tseries)
library(lmtest)
library(ggplot2)
library(ggrepel)

# 📌 Cargar bibliotecas del profesor
source("summary.arima.R")
source("TS.diag.R")

# 📌 Establecer directorio de trabajo
setwd("d:/dev/estadistica/Taller de investigacion/")

# 📌 Cargar los datos
datos <- read.csv("tiempos_respuesta.csv", sep=";")

# 📌 Convertir la columna de fecha a formato Date
datos$fecha <- ymd_hms(datos$fecha)

# 📌 Agregar datos por mes usando la MEDIANA en lugar del promedio
datos_mensuales_mediana <- datos %>%
  mutate(anio_mes = floor_date(fecha, "month")) %>%
  group_by(anio_mes) %>%
  summarise(mediana = median(tiempo_respuesta, na.rm = TRUE))  # Usar mediana

# 📌 Crear la serie de tiempo mensual basada en la MEDIANA
ts_mensual_mediana <- ts(datos_mensuales_mediana$mediana, 
                         start = c(year(min(datos$fecha)), month(min(datos$fecha))), 
                         frequency = 12)

# 📌 Verificar estacionariedad con la prueba de Dickey-Fuller Aumentada (ADF)
adf_test_result <- adf.test(ts_mensual_mediana)
print(adf_test_result)  # Si p-value > 0.05, se necesita diferenciación

# 📌 Aplicar diferenciaciones hasta lograr estacionariedad
ts_mensual_diff1 <- diff(ts_mensual_mediana)
adf_test_diff1 <- adf.test(ts_mensual_diff1)

ts_mensual_diff2 <- diff(ts_mensual_diff1)
adf_test_diff2 <- adf.test(ts_mensual_diff2)

# 📌 Ajustar el modelo ARIMA con los parámetros correctos (según la prueba ADF)
modelo_mediana <- Arima(ts_mensual_mediana, order = c(1,2,1))

# 📌 Resumen del modelo ARIMA
summary(modelo_mediana)

# 📌 Análisis de residuos
tsdiag(modelo_mediana)  # Diagnóstico general
ks.test(scale(modelo_mediana$residuals), "pnorm")  # Prueba de normalidad
lmtest::bptest(lm(modelo_mediana$residuals ~ time(modelo_mediana$residuals)))  # Prueba de homocedasticidad

# 📌 Ajustar los residuos a la misma frecuencia de la serie original
#residuos_mediana <- ts(modelo_mediana$residuals, frequency = 12)

# 📌 Aplicar la prueba de blancura corregida
#TS.diag(residuos_mediana, lag = 12)


# 📌 Aplicar la prueba de blancura del profesor
#TS.diag(modelo_mediana$residuals, lag = 12)

# 📌 Generar predicción para los próximos 7 meses
pred_mediana <- forecast(modelo_mediana, h = 7)

# 📌 Crear dataframe con las predicciones
df_pred_mediana <- data.frame(
  Fecha = seq(from = as.Date(max(datos_mensuales_mediana$anio_mes)) %m+% months(1), 
              by = "month", length.out = 7),
  Predicción = as.numeric(pred_mediana$mean),
  LI_80 = as.numeric(pred_mediana$lower[,1]),
  LS_80 = as.numeric(pred_mediana$upper[,1]),
  LI_95 = as.numeric(pred_mediana$lower[,2]),
  LS_95 = as.numeric(pred_mediana$upper[,2])
)

ggplot() +
  # 📌 Datos históricos
  geom_line(data = datos_mensuales_mediana, aes(x = as.Date(anio_mes), y = mediana), 
            color = "black", size = 1) +
  
  # 📌 Intervalos de confianza
  geom_ribbon(data = df_pred_mediana, aes(x = Fecha, ymin = LI_95, ymax = LS_95), 
              fill = "lightblue", alpha = 0.4) +
  geom_ribbon(data = df_pred_mediana, aes(x = Fecha, ymin = LI_80, ymax = LS_80), 
              fill = "blue", alpha = 0.4) +
  
  # 📌 Predicción central
  geom_line(data = df_pred_mediana, aes(x = Fecha, y = Predicción), 
            color = "blue", size = 1) +
  
  # 📌 Etiquetas para la predicción central
  geom_text_repel(data = df_pred_mediana, aes(x = Fecha, y = Predicción, 
                                              label = round(Predicción, 1)), 
                  color = "blue", size = 5, fontface = "bold", nudge_y = 3) +
  
  # 📌 Etiquetas para el peor escenario (LI_95) - en rojo
  geom_text_repel(data = df_pred_mediana, aes(x = Fecha, y = LI_95, 
                                              label = round(LI_95, 1)), 
                  color = "red", size = 5, fontface = "bold", nudge_y = -5) +
  
  # 📌 Etiquetas para el mejor escenario (LS_95) - en verde oscuro y negrita
  geom_text_repel(data = df_pred_mediana, aes(x = Fecha, y = LS_95, 
                                              label = round(LS_95, 1)), 
                  color = "darkgreen", size = 5, fontface = "bold", nudge_y = 5) +
  
  # 📌 Títulos y etiquetas
  ggtitle("Predicción del Tiempo de Respuesta con Mediana y Escenarios (Dic 2024 - Jun 2025)") +
  xlab("Tiempo") +
  ylab("Tiempo de Respuesta (ms)") +
  
  # 📌 Ajuste del eje X
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y") +
  
  # 📌 Mejorar la visibilidad
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 📌 Crear DataFrame con los intervalos de confianza y la predicción central
df_pred_mediana <- data.frame(
  Fecha = seq(from = as.Date("2024-12-01"), by = "month", length.out = 7),  # Fechas de predicción
  Predicción_Central = pred_mediana$mean,  # Predicción central
  LI_80 = pred_mediana$lower[,1],  # Intervalo de confianza al 80% (línea inferior)
  LS_80 = pred_mediana$upper[,1],  # Intervalo de confianza al 80% (línea superior)
  LI_95 = pred_mediana$lower[,2],  # Intervalo de confianza al 95% (línea inferior)
  LS_95 = pred_mediana$upper[,2]   # Intervalo de confianza al 95% (línea superior)
)

# 📌 Imprimir tabla en consola
print(df_pred_mediana)

# 📌 Si quieres exportarla a un archivo CSV:
write.csv(df_pred_mediana, "predicciones_intervalos.csv", row.names = FALSE)



#### spline ####

# 📌 Cargar librerías necesarias
library(tidyverse)
library(lubridate)
library(splines)
library(ggplot2)
library(ggrepel)

# 📌 Cargar los datos
datos <- read.csv("tiempos_respuesta.csv", sep = ";")

# 📌 Convertir la columna fecha a formato de fecha
datos$fecha <- ymd_hms(datos$fecha)

# 📌 Transformaciones y variables auxiliares
datos_transformado <- datos %>%
  mutate(
    Dia = as.Date(fecha),
    DiaSemana = weekdays(fecha, abbreviate = FALSE),
    DiaCritico = ifelse(DiaSemana %in% c("jueves", "viernes"), 1, 0),
    DiaNumerico = wday(fecha, label = FALSE, week_start = 1),
    DiaNumericoMes = day(fecha),
    MesNumerico = month(fecha),
    SemanaNumerico = isoweek(fecha),
    Anio = year(fecha)
  )

# 📌 Agrupar por día y calcular la mediana
datos_agrupados <- datos_transformado %>%
  group_by(Dia) %>%
  summarise(
    tiempo_respuesta_MEDIANA = median(tiempo_respuesta, na.rm = TRUE),
    DiaSemana = first(DiaSemana),
    DiaCritico = first(DiaCritico),
    DiaNumerico = first(DiaNumerico),
    DiaNumericoMes = first(DiaNumericoMes),
    MesNumerico = first(MesNumerico),
    SemanaNumerico = first(SemanaNumerico),
    Anio = first(Anio)
  ) %>%
  ungroup()

# 📌 Crear variables adicionales
datos_agrupados <- datos_agrupados %>%
  mutate(
    Noviembre = ifelse(MesNumerico == 11, 1, 0),
    mes = factor(ifelse(MesNumerico == 11, 12, MesNumerico)),
    tiempo = as.numeric(Dia - min(Dia))
  )

# 📌 Ajustar el modelo de spline con mayor precisión
spline_mod <- smooth.spline(datos_agrupados$tiempo, datos_agrupados$tiempo_respuesta_MEDIANA, spar = NULL)

# 📌 Guardar el resumen del modelo spline
sink("spline_mod_resumen.txt")
summary(spline_mod)
sink()

# 📌 Generar gráfico con spline muy ajustado a los datos
ggplot(datos_agrupados, aes(x = Dia, y = tiempo_respuesta_MEDIANA)) +
  geom_line(color = "black", size = 1) +  # Línea de los datos originales
  
  # 📌 Línea del spline ajustado sobre los datos
  geom_line(aes(x = Dia, y = predict(spline_mod)$y), color = "orange", size = 1) +
  
  # 📌 Títulos y etiquetas
  labs(
    title = "Spline Ajustado sobre los Datos Originales",
    x = "Fecha", y = "Tiempo de Respuesta (Mediana)"
  ) +
  
  # 📌 Ajuste del eje X
  scale_x_date(
    breaks = "1 month", 
    labels = scales::date_format("%b-%Y")
  ) +
  
  # 📌 Mejorar visibilidad
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
