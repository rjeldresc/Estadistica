
## TALLER 1

# instalar librerias/paquetes
# install.packages("readxl")
# install.packages("dplyr")

# Librerias
library(readxl)
library(dplyr)

# definir directorio
#setwd("C:/Users/Matias/Desktop/Diplomado 2024")
setwd("d:/dev/Estadistica/")
## lectura de archivos
base <- read_excel("Bases de datos/orders_data.xlsx")
head(base) # muestras las primeras 6 filas
tail(base) # muestras las ultimas 6 filas
str(base) # muestra formatos y un extracto de los valores por columna
glimpse(base) # muestra formatos y un extracto de los valores por columna
summary(base)

## Pregunta 1 ##
# Mostrar la forma o comportamiento del dato

summary(base$itemtotal)
hist(base$itemtotal)
boxplot(base$itemtotal)
moments::skewness(base$itemtotal) # coef. de asimetria
moments::kurtosis(base$itemtotal) # leptocurtica
psych::describe(base$itemtotal)   # varios estadisticos

## Pregunta 2 ##
# mostramos valores y frecuencias
table(base$order_status)

# recodificados 
# con r base
base$order_recodificado <- ifelse(base$order_status == 1, "Delivered", "Returned")
head(base, 20)

# con dplyr 
base <- base %>% 
  mutate(order_recodificado = case_when(base$order_status == 1 ~ "Delivered",
                                        TRUE                   ~ "Returned")) #TRUE significa en otro caso, es el default
head(base, 20)

## seguimos, calculamos estadisticos agrupados
aggregate(data = base, itemtotal ~ order_recodificado, FUN = mean) # media
aggregate(data = base, itemtotal ~ order_recodificado, FUN = sd)   # desviacion estandar

prop.table(table(base$order_recodificado))
#prop.table(table(base$order_recodificado, base$))

# con dplyr
base %>% 
  group_by(order_recodificado) %>% 
  summarise(promedio       = mean(itemtotal),
            desviacion     = sd(itemtotal),
            coef_variacion = sd(itemtotal)/mean(itemtotal))

## Pregunta 3 ##

# si queremos reordenar (no es necesario)
base$month_date <- factor(base$month_date, levels = c("JAN","FEB","JUN","JUL",
                                                      "AUG","SEPT","OCT","NOV",
                                                      "DEC"))
# boxplot
boxplot(base$itemtotal ~ base$month_date)

# vioplot
vioplot::vioplot(base$itemtotal ~ base$month_date)

# con dplyr
base %>% 
  group_by(month_date) %>% 
  summarise(promedio       = mean(itemtotal),
            mediana        = median(itemtotal),
            desviacion     = sd(itemtotal),
            coef_variacion = sd(itemtotal)/mean(itemtotal),
            minimo         = min(itemtotal),
            maximo         = max(itemtotal),
            coef_asim      = moments::skewness(itemtotal))

## Pregunta 5 ##
junio <- filter(base, month_date == "JUN")
head(junio)
table(junio$month_date); table(base$month_date) # check

tabla <- table(junio$month_date, junio$order_recodificado)
tabla
tabla2 <- prop.table(tabla) # distribucion porcentual
tabla2

barplot(tabla2, col = "steelblue", 
        ylim = c(0,1), 
        ylab = "Frecuencia Relativa", 
        xlab = "Estado orden")

# cual es la probabilidad de que un pedido sea entregado (no devuelto) en
# noviembre?
table(base$month_date, base$order_recodificado)
27/29

## Pregunta 6 ##

summary(base$shipping_fee)

# recodificamos
base <- base %>% 
  mutate(tramo_envio = case_when(is.na(shipping_fee) ~ "gratuito",
                                 shipping_fee > 20   ~ "Mayor a 20",
                                 shipping_fee >= 10  ~ "De 10 a 20",
                                 TRUE                ~ "Menor a 10"))
head(base)

# ejemplo
base %>% filter(shipping_fee>20) %>% select(shipping_fee, tramo_envio)

## Pregunta 7 ##

# boxplot
boxplot(base$itemtotal ~ base$tramo_envio)

# con dplyr
base %>% 
  group_by(tramo_envio) %>% 
  summarise(promedio       = mean(itemtotal),
            mediana        = median(itemtotal),
            desviacion     = sd(itemtotal),
            coef_variacion = sd(itemtotal)/mean(itemtotal),
            minimo         = min(itemtotal),
            maximo         = max(itemtotal),
            coef_asim      = moments::skewness(itemtotal))

## Pregunta 8 ##

base$ship_city2 <- stringr::str_remove_all(toupper(base$ship_city), ",") #en la columna ship_city hay ciudades con una ","

# top 3 ciudades con mayor recaudacion de monto pagado
# ship_city, item_total
base %>% group_by(ship_city2) %>% 
  summarise(itemtotal = sum(itemtotal)) %>% 
  arrange(desc(itemtotal)) %>% 
  slice(1:3) -> top3

base_top3 <- base %>% filter(ship_city2 == "MAHARASHTRA" |
                               ship_city2 == "WEST BENGAL" |
                               ship_city2 == "TAMIL NADU")  
table(base_top3$ship_city2)  
  
base_top3 <- base %>% filter(ship_city2 %in% top3$ship_city2)
table(base_top3$ship_city2)  

table(base_top3$tramo_envio, base_top3$ship_city2)
prop.table(table(base_top3$tramo_envio, base_top3$ship_city2), margin = 2) %>% 
  addmargins()


