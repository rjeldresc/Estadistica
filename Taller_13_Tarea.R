#### EJERCICIO ####
#### Problemas con el uso Tarjeta de crédito TCM ####

#En una reunión de trabajo se determina que el uso de la tarjeta presenta áreas de mejora en varios aspectos.
#Sin embargo, es necesario verificar o refutar las apreciaciones que se plantearon sobre la TCM. 
#Estás se pueden resumir como sigue:

#Bajo uso: dado que hasta mayo 2019 el 62% utilizó la tarjeta al menos una vez, y ahora (hasta mayo
#          2020) se cree que ese porcentaje ha bajado significativamente.

#Montos: El comité de promociones discute que ellos han focalizado apropiadamente las ofertas, de tal
#        forma que han incrementado los montos de compras mensuales, y para comprobar indican que en
#        mayo2019 fue de m$400, aseguran que este mes fue superior.

#Antiguos: El área de fidelización de clientes (renegociación) es acusada de impedir que antiguos clientes
#          incrementen sus compras al limitar sus niveles de endeudamiento. En otras palabras, entre los que usan
#          la tarjeta, los clientes antiguos tiene montos medios M$50 inferiores a los clientes nuevos 
#          (las tarjetas NumCliente ≤ 250.000 fueron emitidas antes de enero2010 --> antiguos)

#Segmento joven:  ¿Hay evidencia que permita afirmar que los hombres jóvenes ( grupo ≤ 35 años) de
#                  regiones gastan más que los hombres jóvenes de la RM?

#Para cada uno de los 4 test de hipótesis indique exactamente lo siguiente:
# 1.- Defina el/los parámetros (por ejemplo, Mu = gasto medio en clientes con menos de 50 años)
# 2.- Plantear las hipótesis H0 y H1
# 3.- Entregue el valor del estadístico y el correspondiente valor-p
# 4.- Redacte la conclusión de su decisión en el contexto de lo planteado

#Para todo evento, asuma normalidad y utilice 𝜶 = 0,05

library(dplyr)
library(readxl)

Base <- read_excel("TCM2020.xlsx")

#### Bajo uso #### 
# 1.- 
# P = proporción de clientes que usaron la tarjeta al menos una vez en el último año (mayo 2019 a mayo 2020)

# 2.- 
# Los datos indican que hasta mayo 2019 el 62% utilizó la tarjeta al menos una vez
# H0: P >= 0.62 (el porcentaje de personas que usan TCM no ha bajado)
# H1: P < 0.62

# 3.- 
uso2020 <- Base$Uso2020    # Selecciona solo la variable uso 2020   
total_uso <- sum(uso2020) # Total de personas que si la usaron el 2020
n_uso <- length(uso2020) # Total de personas

prop.test(total_uso, n_uso, p = 0.62, alternative = "less", conf.level = 0.95) # Hace el test de proporciones
# alternativa = less,  para probar menor que

#4.-
# Si el valor p es mayor a 0.05 no rechaza H0
# Como p-valor = 0.5822, entonces, la proporcion de el uso de tarjeta del ultimo año no es menor a la proporcion de hasta mayo 2019

# ¿Qué pasa si nos interesa un i.c. para el parametro de la poblacion, 
# proporción de personas que si usaron la tarjeta el 2020

test <- prop.test(total_uso, n_uso, alternative = "two.sided", conf.level = 0.95) # 
test$conf.int


#### Montos #### 
# 1.- 
# mu: monto medio de compra de mayo 2020

# 2.-
# los datos indican que monto medio de compra en mayo 2019 fue de m$ 400
# H0: mu <= 400 (monto medio de compra es inferior)
# H1: mu > 400

# 3.-
uso_mayo <- Base %>% filter(UsoMayo == 1) # solo clientes que utilizan la TCM en mayo 2020
nrow(uso_mayo) #númer de filas
monto_mayo <- uso_mayo$MontoMayo #solo uso la columena "MontoMayo"

t.test(monto_mayo, mu = 400, alternative = "greater", conf.level = 0.95) # test de media

# t = 3.1324
# g.l = 167
# valor-p = 0.001024

# 4.-
# Como el valor-p es menor a 0.05, se rechaza H0, entonces existe evidencia para 
# concluir que el monto promedio de compra de mayo 2020 es mayor a los m$400 de mayo 2019

# ¿Qué pasa si nos interesa un i.c. para el parámetro de la poblacion? 
test <- t.test(monto_mayo, alternative = "two.sided", conf.level = 0.95) 
test$conf.int


#### Antiguos #### 
#Conclusión: No hay suf evidencia para concluir que los cientes nuevos gastan mas que los antiguos

#### Segmento Joven #### 
#Conclusión: los hombres jovenes de region si gastan mas que los hombres jovenes de la RM

