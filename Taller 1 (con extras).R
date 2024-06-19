
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
# coef. de asimetria mayor a 0, tenemos una asimetria hacia la derecha
moments::kurtosis(base$itemtotal) # mayor a 0 -> leptocurtica
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
                                        TRUE                   ~ "Returned"))
head(base, 20)

## seguimos, calculamos estadisticos agrupados
aggregate(data = base, itemtotal ~ order_recodificado, FUN = mean) # media
aggregate(data = base, itemtotal ~ order_recodificado, FUN = sd)   # desviacion estandar

# probabilidad de que un pedido sea devuelto?
prop.table(table(base$order_recodificado)) # R: 0.06432749

# con dplyr obtenemes estadisticos resumidos igualmente
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

# descriptivo con dplyr
base %>% 
  group_by(month_date) %>% 
  summarise(promedio       = mean(itemtotal),
            mediana        = median(itemtotal),
            desviacion     = sd(itemtotal),
            coef_variacion = sd(itemtotal)/mean(itemtotal),
            minimo         = min(itemtotal),
            maximo         = max(itemtotal),
            coef_asim      = moments::skewness(itemtotal))

# Pregunta 4 ## no realizada en los modulos, se deja a revision (simil a pregunta 8)

top_5 = base %>% group_by(ship_city) %>%
  summarise(cantidad_de_pedidos = n()) %>% # n() nos sirve para contar el numero de casos que tenemos
  arrange(desc(cantidad_de_pedidos)) %>% # arrange nos sirve para ordenar y desc() para ir descendiendo
  slice(1:5) # slice nos permite seleccionar, remover y duplicar observaciones

pie(top_5$cantidad_de_pedidos, label = top_5$ship_city) # como se distribuye entre ellas

# Como nos hubiesemos quedado con solo esas 5 ciudades en nuestra data?

base %>% filter(ship_city %in% top_5$ship_city)
# Esto es optimo solo para pequeños cruces, para intersecciones mas grandes
# es recomendable realizar un left_join y filtrar segun interes, por ver!!

## Pregunta 5 ##
junio <- filter(base, month_date == "JUN")
head(junio)
table(junio$month_date); table(base$month_date) # revisamos lo obtenido

# vemos frecuencias absoluta del estado del pedido en junio
tabla <- table(junio$month_date, junio$order_recodificado)
tabla
# vemos frecuencias relativas del estado del pedido en junio
tabla2 <- prop.table(tabla)
tabla2

# realizamos grafico de barras
barplot(tabla2, col = "steelblue", ylim = c(0,1), 
        ylab = "Frecuencia Relativa", xlab = "Estado orden")

# cual es la probabilidad de que un pedido sea entregado (no devuelto) en
# noviembre?
table(base$month_date, base$order_recodificado)
prop.table(table(base$month_date, base$order_recodificado), margin = 1)
# R: 27/29 = 0.93103448


## Pregunta 6 ##
# revisamos primero
summary(base$shipping_fee)

# recodificamos
base <- base %>% 
  mutate(tramo_envio = case_when(is.na(shipping_fee) ~ "gratuito",
                                 shipping_fee > 20   ~ "Mayor a 20",
                                 shipping_fee >= 10  ~ "De 10 a 20",
                                 TRUE                ~ "Menor a 10"))
head(base)

# ejemplo para la revision del tramo Mayor a 20
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
# a raiz de dos peticiones, realizamos;
# - removemos comas ","
# - transformamor a mayusculas
base$ship_city2 <- stringr::str_remove_all(toupper(base$ship_city), ",")

# top 3 ciudades con mayor recaudacion de monto pagado
# ship_city, item_total
base %>% group_by(ship_city2) %>% 
  summarise(itemtotal = sum(itemtotal)) %>% 
  arrange(desc(itemtotal)) %>% 
  slice(1:3) -> top3

# obtenemos distribuciones de los pedidos del top 3 ciudades con mayor recaudacion
base_top3 <- base %>% filter(ship_city2 == "MAHARASHTRA" |
                               ship_city2 == "WEST BENGAL" |
                               ship_city2 == "TAMIL NADU")  
table(base_top3$ship_city2)  
  
# otra forma
base_top3 <- base %>% filter(ship_city2 %in% top3$ship_city2)
table(base_top3$ship_city2)  

# distribucion de los pedidos por tramo de envio para el top 3 ciudades
table(base_top3$tramo_envio, base_top3$ship_city2)

# cual es la probabilidad de que un envio sea de tramo de envio mayor a 20 dolares
# en la ciudad de MAHARASHTRA?
prop.table(table(base_top3$tramo_envio, base_top3$ship_city2), margin = 2) %>% 
  addmargins()
# R: 0.00000000


# Pregunta 9 ## no realizada en los modulos, se deja a revision
base$cod
base$cod <- tidyr::replace_na(base$cod, "Prepaid") # simil a ifelse(is.na(base$cod), "Prepaid", base$cod)

# Filtro que necesitamos: Prepagados con anterioridad y su costo de envio sea <10 o >20
# Dos opciones:

# filtro1: Menores a 10 o Mayores a 20, filtro2: Prepagados
# filtro2: Todo junto 

# Forma 1
base2 <- base %>% filter(tramo_envio == "Menor a 10" |
                           tramo_envio == "Mayor a 20")
table(base2$tramo_envio)

base3 <- base2 %>% filter(cod == "Prepaid")
base3

# Forma 2
base3 <- base %>% filter(cod == "Prepaid" & (tramo_envio == "Menor a 10" | 
                                               tramo_envio == "Mayor a 20"))
# Como ya sabemos que son prepagados veamos como se distribuyen
# segun el tramo de envio

tabla8 <- table(base3$tramo_envio)
pie(tabla8, main= "Pedidos Prepagados")


# Pregunta 10 ## no realizada en los modulos, se deja a revision

tabla10 <- table(base$cod, base$tramo_envio)
tabla10 # 0? debe ser un error de tipeo, debemos ignorar
prop.table(tabla10, margin = 1)*100 # totales por fila con margin = 1
# Dentro de los pre pagados el 2.44% son pedidos con envio mayor a $20.

