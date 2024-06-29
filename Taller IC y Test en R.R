
# Taller IC y TEST 
library(dplyr)
library(readxl)
library(misty)

# Antes de importar una tabla, debemos definir nuestro directorio
setwd("d:/dev/Estadistica/")

# Base de datos ####
Base <- read_excel("TCM2020.xlsx")
head(Base)

# Antiguos ##

# Comprobar: Los clientes antiguos tiene montos medios M$50 inferiores a los clientes nuevos.

# dos poblaciones: Clientes antiguos y clientes nuevos (ambos tienen que haber utilizado la TCM)
# comparamos medias -> mu1 vs mu2

# mu1: monto medio de compra/utilizacion de clientes que usan la TCM y son antiguos
# mu2: monto medio de compra/utilizacion de clientes que usan la TCM y son nuevos

# H0: mu2 - mu1 <= 50
# H1: mu2 - mu1  > 50

summary(Base$MontoAcum)

si_uso_2020 <- filter(Base, Uso2020 == 1) # seleccionamos quienes utilizaron la TCM, entre 
                                          # mayo 2019 a mayo 2020
nrow(si_uso_2020)

# separar las muestras

antiguos <- filter(si_uso_2020, Cliente <= 250000)
montos_antiguos <- antiguos$MontoAcum # extraemos el monto de compra/utilizacion entre mayo 2019
                                      # a mayo 2020
nrow(antiguos)

nuevos  <- filter(si_uso_2020, Cliente > 250000)
montos_nuevos <- nuevos$MontoAcum
nrow(nuevos)


# un paso siempre requerido en el caso de test de dos medias
# H0: sigma1  = sigma2 ---> sigma1/sigma2  = 1
# H1: sigma1 != sigma2 ---> sigma1/sigma2 != 1
?var.test
var.test(montos_antiguos, 
         montos_nuevos, 
         ratio = 1, 
         alternative = "two.sided",
         conf.level = 0.95)

# Estadistico F = 0.65416
#       p-value = 0.04633
# Se define por enunciado alpha = 0.05

# Siempre p-value < alpha --> Rechazo H0, por ende, se concluye H1
# R: 0.04633 < 0.05 --> Rechazo H0, se concluye que las varianzas son distintas

## Volviendo
# H0: mu2 - mu1 <= 50
# H1: mu2 - mu1  > 50

t.test(montos_nuevos, 
       montos_antiguos, 
       alternative = "greater", 
       mu = 50,
       conf.level = 0.95, 
       var.equal = FALSE)

# Estadistico t = 1.1091
#       p-value = 0.1346
# Se define por enunciado alpha = 0.05

# R: 0.1346 > 0.05 --> No rechazo H0, no hay suficiente evidencia para afirmar
# que los montos medios de compra de los clientes antiguos sean $50M inferiores al de 
# los clientes nuevos

# Interpretación del resultado:
#   
#   Dado que el p-valor de 0.1346 es mayor que α=0.05α=0.05, no podemos rechazar la hipótesis nula (H0H0​). Esto significa que no hay suficiente evidencia para afirmar que la diferencia entre los montos medios de compra/utilización de los clientes nuevos y antiguos que usan la TCM es mayor a 50.
# Conclusión:
#   
#   Con un nivel de significancia del 5%, no hay suficiente evidencia para rechazar la hipótesis nula. Por lo tanto, no podemos concluir que la diferencia en los montos medios de compra/utilización entre clientes nuevos y antiguos que usan la TCM sea mayor a 50.
# 
# Esto implica que, basado en los datos y el test realizado, no se puede afirmar que los clientes nuevos tengan un monto medio de compra/utilización significativamente mayor que el de los clientes antiguos por más de 50 unidades.

# Segmento Joven ##

# mu1: monto medio de compra de clientes jovenes de regiones que utilizaron la TCM
# mu2: monto medio de compra de clientes jovenes de RM que utilizaron la TCM

head(si_uso_2020) # muestra de quienes usaron la TCM entre mayo 2019 a mayo 2020

si_uso_hombres  <- filter(si_uso_2020, `Sex(1=Fem)` == 0)
hombres_jovenes <- filter(si_uso_hombres, Edad <= 35)

hombres_jovenes <- si_uso_2020 %>%
  filter(`Sex(1=Fem)` == 0) %>%
  filter(Edad <= 35)

# RM
hombres_jovenes_rm <- filter(hombres_jovenes, `Reg(1=RM)` == 1)
montos_hjRM <- hombres_jovenes_rm$MontoAcum

# regiones
hombres_jovenes_regiones <- filter(hombres_jovenes, `Reg(1=RM)` == 0)
montos_hjregiones <- hombres_jovenes_regiones$MontoAcum


# un paso siempre requerido en el caso de test de dos medias
# H0: sigma1  = sigma2 ---> sigma1/sigma2  = 1
# H1: sigma1 != sigma2 ---> sigma1/sigma2 != 1

var.test(montos_hjRM, 
         montos_hjregiones, 
         ratio = 1, 
         alternative = "two.sided",
         conf.level = 0.95)
#aca da lo mismo el orden, solo se está probando si son distintos a 1 o no
# p-value = 0.2892
# R: No rechazo H0, no podemos concluir H1 --> concluimos que las varianzas no difieren


# H0: mu1 <= mu2 --> mu1 - mu2 <= 0
# H1: mu1 > mu2  --> mu1 - mu2 > 0

t.test(montos_hjregiones, 
       montos_hjRM, 
       alternative = "greater", 
       mu = 0,
       conf.level = 0.95, 
       var.equal = TRUE)
# p-value = 0.02648
# Rechazo H0, por ende, existe suficiente evidencia para concluir que los hombres jovenes
# de region gastan mas que los hombres jovenes de RM.


