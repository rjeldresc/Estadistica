getwd()
setwd("d:/dev/estadistica/Serie UF/")

# Instala y carga las librerías necesarias
install.packages("readxl")  # Para leer archivos Excel
install.packages("ggplot2") # Para graficar
install.packages("dplyr")   # Para manipular datos

library(readxl)
library(ggplot2)
library(dplyr)

# Paso 1: Cargar el archivo Excel
# Cambia "ruta_del_archivo.xlsx" por la ruta al archivo descargado
Base <- rio::import("Indicador.csv")
head(Base)
# Paso 2: Limpiar y transformar los datos
# Convertir "Día" a formato Date
# Transformar el valor correctamente
# Transformar el valor correctamente
library(dplyr)
# Configurar el idioma de las fechas (español)
Sys.setlocale("LC_TIME", "es_ES.UTF-8") # Para sistemas Unix/Linux/Mac
# Asegúrate de que la columna "Valor" esté como texto al importarlo
library(dplyr)

# Cambiar "ene" por "01", "feb" por "02", etc.
Base$Dia <- gsub("ene", "01", Base$Dia)
Base$Dia <- gsub("feb", "02", Base$Dia)
Base$Dia <- gsub("mar", "03", Base$Dia)
Base$Dia <- gsub("abr", "04", Base$Dia)
Base$Dia <- gsub("may", "05", Base$Dia)
Base$Dia <- gsub("jun", "06", Base$Dia)
Base$Dia <- gsub("jul", "07", Base$Dia)
Base$Dia <- gsub("ago", "08", Base$Dia)
Base$Dia <- gsub("sept", "09", Base$Dia)
Base$Dia <- gsub("oct", "10", Base$Dia)
Base$Dia <- gsub("nov", "11", Base$Dia)
Base$Dia <- gsub("dic", "12", Base$Dia)

# Ahora convierte las fechas al formato correcto
Base <- Base %>%
  mutate(
    Dia = as.Date(Dia, format = "%d.%m.%Y"),  # Ahora debería funcionar correctamente
    # Limpiar los valores numéricos
    Valor_limpio = gsub("\\.", "", Valor),    # Elimina los separadores de miles
    Valor_limpio = gsub(",", ".", Valor_limpio), # Reemplaza la coma decimal por un punto
    Valor = as.numeric(Valor_limpio)          # Convierte a numérico
  ) %>%
  select(-Valor_limpio)  # Elimina la columna auxiliar

# Revisa los datos
head(Base)


# Revisa los datos procesados
head(Base)

df<-Base
# Paso 4: Graficar la serie de tiempo
ggplot(df, aes(x = Dia, y = Valor)) +
  geom_line(color = "blue") +
  labs(title = "Evolución de la UF en 2024", x = "Fecha", y = "UF") +
  theme_minimal()

# Paso 5: Transformar a logaritmo si es necesario
df <- df %>%
  mutate(Log_Valor = log(Valor))

# Graficar la transformación logarítmica
ggplot(df, aes(x = Dia, y = Log_Valor)) +
  geom_line(color = "red") +
  labs(title = "Evolución logarítmica de la UF en 2024", x = "Fecha", y = "Log(UF)") +
  theme_minimal()


# Filtrar por los meses específicos de 2024
Base_filtrada <- Base %>%
  filter(format(Dia, "%Y") == "2024",           # Filtra por el año 2024
         format(Dia, "%m") %in% c("06", "07", "08", "09", "10", "11"))  # Filtra los meses de junio a noviembre

# Verifica el resultado
head(Base_filtrada)

library(ggplot2)

# Gráfico con etiquetas en el eje x para cada mes
ggplot(Base_filtrada, aes(x = Dia, y = Valor)) +
  geom_line(color = "blue") +
  labs(title = "Evolución de la UF en 2024", x = "Fecha", y = "UF") +
  scale_x_date(
    date_labels = "%b",  # Muestra los nombres de los meses (abreviados)
    date_breaks = "1 month"  # Muestra una etiqueta por mes
  ) +
  theme_minimal()

Base_filtrada_log <- Base_filtrada %>%
  mutate(Log_Valor = log(Valor))

# Graficar la transformación logarítmica
ggplot(Base_filtrada_log, aes(x = Dia, y = Log_Valor)) +
  geom_line(color = "red") +
  labs(title = "Evolución logarítmica de la UF en 2024", x = "Fecha", y = "Log(UF)") +
  theme_minimal()
