library(samplingbook)
library(readxl)
library(dplyr)

# definir el directorio
setwd("D:/dev/Estadistica/Control Muestreo")

# lectura de los datos
base = readxl::read_excel("Establecimientos.xlsx")
head(base)
tail(base)

## muestreo estratificado

## Requerimiento:
## Nos piden estimar el rendimiento de MAT con un error no superior a 3 puntos y que 
## la proporción de establecimientos en condición de riesgo (rendimiento en SIMCE MAT < 200)
## estimada presente un error menor o igual al 2%.

## Material:
## Se dispone del listado de los RBD de los 3632 establecimientos

## Informacion:

## Región	           Rendimiento SIMCE     	    Establecimientos  en riesgo %    N
##                   Promedio	Desv. Estándar	
## Biobío	             249	      35	                        10.2%              1.058
## Metropolitana	     255	      30	                         3.8%              1.808
## Valparaíso	         240	      25	                         5.4%                766

## Tamaño de muestra 

## Para la media, queremos un error no mayor a 3 puntos
## necesitamos Nh y Sh

Sh = c(35, 30, 25)
Nh = c(1058, 1808, 766)
stratasize(e = 3, Nh = Nh, Sh = Sh, type = "prop")
## total sample size determinated: 361

## Para la proporcion, queremos un error no mayor a 0.02 (2%)
PQ = c(0.102*0.898, 0.038*0.962, 0.054*0.946) # p*(1-p)
Sh_prop = sqrt(PQ)
Nh = c(1058, 1808, 766)
stratasize(e = 0.02, Nh = Nh, Sh = Sh_prop, type = "prop")
## total sample size determinated: 467

## Con cual nos quedamos?
max(361, 467) # 467

## Este tamaño de muestra es global y necesitamos separar
stratasamp(n = 467, Sh = Sh_prop, Nh = Nh, type = "prop")

## Muestras por region
RBD <- readxl::read_excel("Establecimientos.xlsx")
head(RBD)
nrow(RBD)
table(RBD$Region)
Nh = table(RBD$Region)

## Muestreo
set.seed(14102024) #fecha de hoy 14102024

n1 = 136
muestra1 <- RBD %>% filter(Region == "DEL BIOB?O") %>% 
  sample_n(n1)

n2 = 232
muestra2 <- RBD %>% filter(Region == "METROPOLITANA DE SANTIAGO") %>% 
  sample_n(n2)

n3 = 98
muestra3 <- RBD %>% filter(Region == "DE VALPARA?SO") %>% 
  sample_n(n3)

muestraR <- rbind(muestra1, muestra2, muestra3)
nrow(muestraR)

nh <- table(muestraR$Region)
Nh; nh

## Recoleccion de la informacion
head(muestraR)

## Establecimientos en riesgo
muestraR$riesgo <- ifelse(muestraR$MAT < 200, 1, 0)
head(muestraR, 20)

## estimaciones
stratamean(muestraR$MAT, muestraR$Region, Nh, eae = TRUE)
stratamean(muestraR$riesgo, muestraR$Region, Nh, eae = TRUE)

#para el mas

# Instala el paquete si no lo tienes
# install.packages("readxl")

# Cargar el paquete
library(readxl)

# Cargar el archivo de Excel
datos <- read_excel("Establecimientos.xlsx")

# Establecer la semilla
set.seed(06031985)  # Reemplaza DDMMYY con tu fecha de nacimiento, por ejemplo, 250894

# Definir el tamaño de la muestra
n_muestra <- 880

# Realizar el muestreo aleatorio simple
muestra_mas <- datos[sample(nrow(datos), n_muestra, replace = FALSE), ]

## Establecimientos en riesgo
muestra_mas$riesgo <- ifelse(muestra_mas$MAT < 200, 1, 0)

# Calcular el promedio del rendimiento SIMCE en la muestra
prom_rendimiento_simce <- mean(muestra_mas$MAT)
prom_rendimiento_simce


# Calcular la proporción de establecimientos en riesgo en la muestra
prop_riesgo <- mean(muestra_mas$riesgo)
prop_riesgo

# Error estándar del promedio del rendimiento SIMCE
err_est_rendimiento <- sd(muestra_mas$MAT) / sqrt(n_muestra)
err_est_rendimiento

# Error estándar de la proporción de establecimientos en riesgo
err_est_prop <- sqrt(prop_riesgo * (1 - prop_riesgo) / n_muestra)
err_est_prop


# Parámetros
Z <- 1.96        # Valor crítico para un nivel de confianza del 95%
p <- 0.102       # Proporción estimada de establecimientos en riesgo
E_p <- 0.02      # Error permitido en la proporción (2%)

# Cálculo del tamaño de muestra
n_proporcion <- (Z^2 * p * (1 - p)) / E_p^2
n_proporcion <- ceiling(n_proporcion)  # Redondear hacia arriba

# Mostrar el tamaño de muestra calculado
n_proporcion
