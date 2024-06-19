# Alumno: Rodrigo Jeldres Carrasco

library(readxl)


setwd("d:/dev/Estadistica/")

base <- read_excel("Control_1.xlsx", sheet = "Salarios")
attach(base)
tabla_frecuencia <- table(Sexo)
porcentajes <- prop.table(tabla_frecuencia) * 100

tabla_frecuencia_df <- data.frame(
  Sexo = names(tabla_frecuencia),
  Frecuencia = as.vector(tabla_frecuencia),
  Porcentaje = round(porcentajes, 2)
)
total_row <- data.frame(
  Sexo = "Total",
  Frecuencia = total_frecuencia,
  Porcentaje = round(total_porcentaje, 2)
)

print(tabla_frecuencia_df)

pie(tabla_frecuencia, 
    labels = paste(names(tabla_frecuencia), round(porcentajes, 1), "%"),
    main = "Distribución por Sexo")


tabla_frecuencia_cargo <- table(Sexo, Cargo)
porcentajes_cargo <- prop.table(tabla_frecuencia_cargo, 1) * 100

tabla_frecuencia_cargo_df <- as.data.frame(tabla_frecuencia_cargo)
colnames(tabla_frecuencia_cargo_df) <- c("Sexo", "Cargo", "Frecuencia")

porcentajes_cargo_df <- as.data.frame(porcentajes_cargo)
colnames(porcentajes_cargo_df) <- c("Sexo", "Cargo", "Porcentaje")

tabla_frecuencia_porcentaje <- tabla_frecuencia_cargo_df %>%
  inner_join(porcentajes_cargo_df, by = c("Sexo", "Cargo"))

tabla_frecuencia_porcentaje <- tabla_frecuencia_porcentaje %>%
  arrange(Sexo, Cargo)

print(tabla_frecuencia_porcentaje)

library(ggplot2)
ggplot(tabla_frecuencia_porcentaje, aes(x = Cargo, y = Frecuencia, fill = Sexo)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Frecuencia de Cargos según Sexo", x = "Cargo", y = "Frecuencia") +
  theme_minimal()
ggplot(tabla_frecuencia_porcentaje, aes(x = Cargo, y = Porcentaje, fill = Sexo)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Porcentaje de Cargos según Sexo", x = "Cargo", y = "Porcentaje") +
  theme_minimal()


ggplot(base, aes(x = Cargo, y = Salarios)) +
  geom_boxplot() +
  labs(title = "Distribución de salarios por cargo", x = "Cargo", y = "Salarios")

ggplot(base, aes(x = Sexo, y = Salarios)) +
  geom_boxplot() +
  labs(title = "Distribución de salarios por sexo", x = "Sexo", y = "Salarios")

ggplot(base, aes(x = Antig, y = Salarios, color = Sexo)) +
  geom_point() +
  labs(title = "Relación entre Salarios y Antigüedad", x = "Antigüedad", y = "Salarios")

ggplot(base, aes(x = Experiencia, y = Salarios, color = Sexo)) +
  geom_point() +
  labs(title = "Relación entre Salarios y Experiencia", x = "Experiencia", y = "Salarios")

resumen_edad <- summary(Edad)
resumen_edad$Quintiles <- quantile(Edad, probs = c(0.25, 0.5, 0.75))
resumen_edad$Mediana <- median(Edad)
resumen_edad$Promedio <- mean(Edad)
print(resumen_edad)


datos_agrupados_cargo <- base %>%
  group_by(Cargo) %>%
  summarize(Media_Salario = mean(Salarios),
            DE_Salario = sd(Salarios),
            Min_Salario = min(Salarios),
            Max_Salario = max(Salarios),
            Q1_Salario = quantile(Salarios, probs = 0.25),
            Q2_Salario = median(Salarios),
            Q3_Salario = quantile(Salarios, probs = 0.75))
print(datos_agrupados_cargo)

datos_agrupados_sexo <- base %>%
  group_by(Sexo) %>%
  summarize(Media_Salario = mean(Salarios),
            DE_Salario = sd(Salarios),
            Min_Salario = min(Salarios),
            Max_Salario = max(Salarios),
            Q1_Salario = quantile(Salarios, probs = 0.25),
            Q2_Salario = median(Salarios),
            Q3_Salario = quantile(Salarios, probs = 0.75))
print(datos_agrupados_sexo)

## PARTE 2  
library(dplyr)

setwd("d:/dev/Estadistica/")
baseinicial <- read_excel("Control_1.xlsx", sheet = "Espectaculos")
basefinal <- baseinicial %>%
  filter(REGION %in% c("BIOBIO", "METROPOLITANA"))

distribucion_porcentual <- basefinal %>%
  group_by(REGION) %>%
  summarise(TOTAL_ESPECTAC = sum(TOTAL_ESPECTAC)) %>%
  mutate(Porcentaje = TOTAL_ESPECTAC / sum(TOTAL_ESPECTAC) * 100)

print(distribucion_porcentual)

ggplot(distribucion_porcentual, aes(x = REGION, y = Porcentaje, fill = REGION)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribución Porcentual de los Espectáculos por Región",
       x = "Región",
       y = "Porcentaje de Espectáculos") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal()



#b)	Muestre la trayectoria, con un gráfico de líneas, de los tipos de espectáculos en el transcurso de los años (2014-2019) sin separar por regiones. 
datos_resumidos <- baseinicial %>%
  group_by(AÑO, ESPECTACULOS) %>%
  summarise(TOTAL_ESPECTAC = sum(TOTAL_ESPECTAC, na.rm = TRUE))

ggplot(datos_resumidos, aes(x = AÑO, y = TOTAL_ESPECTAC, color = ESPECTACULOS)) +
  geom_line(size = 1) +
  labs(title = "Trayectoria de los Tipos de Espectáculos (2014-2019)",
       x = "Año",
       y = "Total de Espectáculos",
       color = "Tipo de Espectáculo") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Letra C
datos_region <- baseinicial %>%
  filter(REGION %in% c("BIOBIO", "METROPOLITANA"))

datos_resumidos <- datos_region %>%
  group_by(AÑO, ESPECTACULOS, REGION) %>%
  summarise(TOTAL_ESPECTAC = sum(TOTAL_ESPECTAC, na.rm = TRUE)) %>%
  ungroup()

# Crear gráficos de líneas separados para cada región
grafico_biobio <- ggplot(datos_resumidos %>% filter(REGION == "BIOBIO"), 
                         aes(x = AÑO, 
                             y = TOTAL_ESPECTAC, 
                             color = ESPECTACULOS)) +
  geom_line(size = 1) +
  labs(title = "Trayectoria de los Tipos de Espectáculos (BIOBIO)",
       x = "Año",
       y = "Total de Espectáculos",
       color = "Tipo de Espectáculo") +
  theme_minimal() +
  theme(legend.position = "bottom")

grafico_metropolitana <- ggplot(datos_resumidos %>% filter(REGION == "METROPOLITANA"), 
                                aes(x = AÑO, 
                                    y = TOTAL_ESPECTAC, 
                                    color = ESPECTACULOS)) +
  geom_line(size = 1) +
  labs(title = "Trayectoria de los Tipos de Espectáculos (METROPOLITANA)",
       x = "Año",
       y = "Total de Espectáculos",
       color = "Tipo de Espectáculo") +
  theme_minimal() +
  theme(legend.position = "bottom")

library(gridExtra)
grid.arrange(grafico_biobio, grafico_metropolitana, ncol = 2)
