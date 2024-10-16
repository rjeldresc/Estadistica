
library(dplyr)
library(readxl)
library(samplingbook)
options(scipen = 999)

# definir el directorio
setwd("d:/dev/Estadistica")

# lectura de los datos
base = readxl::read_excel("Bases de datos/Precenso-comunas.xlsx")
head(base)
tail(base)

# corregir la lectura
base = readxl::read_excel("Bases de datos/Precenso-comunas.xlsx", skip = 1)
head(base)
tail(base)
names(base)

names(base) <- stringr::str_replace_all(names(base), " |-", "_")
names(base)
head(base)
nrow(base)

# base va a simular la poblacion: de manzanas en las comunas de
# Las Condes, La Reina y Penalolen

# ------------------------------------------------------------------------------

# Nos puede interesar estimar la proporcion de mujeres
# con un error cercano al 5% y ademas, vamos a mantener un nivel de confianza al 95%
# Los expertos dicen que la proporcion es 0.6

# Tamaño de muestra minimo 
# Parentesis( ------------------------------------------------------------------
# Que pasaria si tenemos un tamaño de poblacion de 350 manzanas

sample.size.prop(e = 0.05,
                 P = 0.6,
                 N = 350)
# Sample size needed: 180

# Ahora, buscamos un error del 1%
sample.size.prop(e = 0.01,
                 P = 0.6,
                 N = 350)
# Sample size needed: 338
# Cierre Parentesis) -----------------------------------------------------------

# Y si sabemos que son 3569
sample.size.prop(e = 0.05,
                 P = 0.6,
                 N = 3569)
# Sample size needed: 335

# Y si sabemos que hay un tamaño de poblacion muy grande (o infinito)
sample.size.prop(e = 0.05,
                 P = 0.6, #segun "consejo" experto , o proporcion considerara por la region
                 N = Inf)
# Sample size needed: 369

# Que pasa si desconocemos la proporcion?
# P = 0.5
sample.size.prop(e = 0.05,
                 P = 0.5,
                 N = Inf)
# Sample size needed: 385

# Que pasa si disminuyo el error a 3%
# P = 0.5
sample.size.prop(e = 0.03,
                 P = 0.5, #cuando la proporcion es desconocida
                 N = Inf)
# Sample size needed: 1068

# nos quedaremos con la informaciona a priori que se dio:
# Con un error cercano al 5% y ademas, vamos a mantener un nivel de confianza al 95%
# Los expertos dicen que la proporcion es 0.6
# Decidimos: 
sample.size.prop(e = 0.05,
                 P = 0.6,
                 N = 3569)
# Nos quedaremos con que el numero de la muestra minimo debe ser de 335
# FIN del caso 

# ---------------------------------------------------------------------------- #

# # Explicacion/Ejemplo de porqué no sumar sobre la muestra: #
# # 3.596 manzanas, 1.000.000 total de individuos
# # muestreo de 100 manzanas
# # # todas las manzanas son iguales tal que hay 100 individuos por cada una
# 100*100 # 10.000 muy pequeño en comparacion a 1.000.000


# Si nos interesa el total de mujeres en las 3 comunas
# Podemos realizar una estimacion del tipo N*Media, con N total de manzanas
# Conocimiento experto: S = 100
# Error cercano debe ser de 12

# Tamaño de muestra minimo
sample.size.mean(e = 12,
                 S = 100, 
                 N = 3569)
# Sample size needed: 249

# Si aumentamos el error a +- 50
sample.size.mean(e = 50,
                 S = 100, 
                 N = 3569)
# Sample size needed: 16

# En caso de no disponer de informacion experta:
# Metodo conservador
# alguna idea de sigma (S)?
# minimo = 1    persona
# maximo = 1000 personas
# Metodo conservador -> S = (max-min)/6 o S = (max-min)/4
# En ambos casos estare abarcando un 99% o 95%, respectivamente
max = 1000; min = 1
s1 = (max-min)/6; s2 = (max-min)/4

sample.size.mean(e = 12,
                 S = s1, 
                 N = 3569)
# Sample size needed: 613

sample.size.mean(e = 12,
                 S = s2, 
                 N = 3569)
# Sample size needed: 1135

# nos quedaremos con la informaciona a priori que se dio:
# Conocimiento experto: S = 100
# Error cercano debe ser de 12
# Decidimos que el tamaño de muestra minimo es de 249


# En la practica, si nos interesa ambos parametros y sus definiciones
# Debemos tomar una muestra de 335 max(335, 249)



