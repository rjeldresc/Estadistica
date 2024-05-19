##############  PAQUETES  ############## 
install.packages("readxl") #para leer archivos excel
install.packages("tidyverse") 
install.packages("psych") 
install.packages("openxlsx") #para exportar a excel

setwd("d:/dev/Estadistica")

library(readxl)
Base_Taller3 <- read_excel("Base_Taller3.xlsx", 
                                  sheet = "Parte1")
View(Base_Taller3)



setwd("d:/dev/Estadistica")
library(readxl)
library(tidyverse)

#setwd("C:/Users/caroh/Documents/RStudio/Clases/Clase 3") #Definición de directorio de trabajo
library(readxl)
base <- readxl::read_excel("Base_Taller3.xlsx", sheet = "Parte1") #Importar y definir base de datos
attach(base) #para reconocer por el nombre las columnas importadas
class(base)

base[1,1]
base[,"Edad"] #trae la columna edad
base[,c(2,3)] #todas las filas, columnas 2 y 3
base[base$Edad >= 40 & base$Crédito=="SI",c("Edad","Crédito")] #filtra por edad mayor o igual a 40
personasMayoresoIguala40 <- base[base$Edad >= 40 , ]
hist(personasMayoresoIguala40$Ingreso_Anual, 
     main = "Histograma para ingreso_anual de personas al menos 40 años de edad",
     xlab = "Ingreso Anual",
     ylab = "Frecuencia",
     col = "blue",
     freq = T
     )

# cargar ggplot2
# install.packages("ggplot2")
library(ggplot2)
ggplot(base, aes (x=Ingreso_Anual)) + geom_histogram()

ggplot(base, aes(x = Ingreso_Anual, color = Sexo, fill = Sexo)) + 
  geom_histogram(alpha = 0.3, 
                 bins = 10,
                 binwidth = 5000,
                 position = "stack")

# hacer un histograma en ggplot2
ggplot(data = personasMayoresoIguala40,
       mapping = aes(x = Ingreso_Anual)) +
  geom_histogram(bins = 9)


##############  TABLA DE FRECUENCIA  ##############

#UNA VARIABLE
t1 <- table(Sucursal)  #resume la cantidad de las frecuencias, es como una cuenta por sucursal
t2 <- round(prop.table(t1)*100,1) # round para redondear con 1 decimal, prop.table tabla de frecuencia relativa a partir de tablas de frecuencia absoluta
tabla1 <- cbind(t1,t2)    # muestra verticalmente las dos columnas
colnames(tabla1) <- c("Frecuencia", "Porcentaje") #nombrar nuevas columnas
addmargins(tabla1,margin=1) #agregar los totales por filas o por columnas a una tabla de frecuencia.
addmargins(tabla1,margin=2) #por filas
addmargins(tabla1)

#DOS VARIABLES
t3 <- table(Sucursal,Sexo) #ojo con el orden de las variables, agrupa por sucursal, luego detalla por sexo, para ambos casos
addmargins(t3,margin=1)  #suma vertical
addmargins(t3, margin=2) #suma horizontal
addmargins(t3)

tabla2 <- round(prop.table(t3)*100,1)
addmargins(tabla2)
tabla2

##############  TABLAS CON PROMEDIO  ##############
library(tidyverse)
tabla3 <- base %>% group_by(Sucursal) %>% summarise(Promedio_ingreso_por_Sucursal = mean(Ingreso_Anual))
tabla3
#el operador  %>% permite concatenar operaciones, summarise crea una nueva columna
#group_by() agrupa un conjunto de filas seleccionado en un conjunto de filas de resumen.

##############  TABLAS CON PROMEDIO, DESV Y CV ##############
tabla4 <- base %>% 
          group_by(Sexo) %>%
          summarise(Promedio = mean(Ingreso_Anual),
                    Desviacion = sd(Ingreso_Anual),
                    CD = sd(Ingreso_Anual)/mean(Ingreso_Anual)*100)


############## RESUMEN NUMÉRICO ##############
length(Ingreso_Anual)  # entrega tamaño de la columna
mean(Ingreso_Anual)  # media
median(Ingreso_Anual)# mediana
sd(Ingreso_Anual)    # desviación estándar
IQR(Ingreso_Anual)   # rango intercuartil
quantile(Ingreso_Anual) # cuartiles de una variable
var(Ingreso_Anual)  # varianza
sd(Ingreso_Anual)/mean(Ingreso_Anual)  # coeficiente de variación
range(Ingreso_Anual)   # rango de la variable
library(moments)
kurtosis(Ingreso_Anual)
summary(Ingreso_Anual)  # resumen 5 números y media
tapply(Ingreso_Anual, Sexo, summary) # resumen del ingreso anual 5 números y media, por sexo
tapply(Ingreso_Anual, Sucursal, summary)

#Otra forma:
library(psych)
tabla_resumen <- psych::describeBy(Ingreso_Anual) 
psych::describeBy(Ingreso_Anual,group=Sexo) # separado por categorías 

#Recordatorio para guardar tablas
library(openxlsx)
write.xlsx(tabla_resumen$mean, "tablaResumen.xlsx") #en xlsx


############## FILTRAR Y AGREGAR VARIABLES ##############
#Filtro: forma 1
mayores <- base[Edad > 60, ]   #selecciona solo personas mayores y guarda en "mayores"
suc_A <- base[Sucursal=="A", ] #selecciona solo sucursal A y guarda en "suc_A"

#Filtro:forma 2
library(dplyr)
sucursal_A <- filter(base, Sucursal == "A") #selecciona solo sucursal A
sucursal_A_60 <- filter(base, Sucursal == "A", Edad > 60) #selecciona solo sucursal A , edad mayor a 60


############## AGREGAR NUEVA VARIABLE A LA BASE ##############
base <- base %>% 
        mutate(Edad_cod = case_when(Edad <= 30 ~ "Joven",
                                    Edad <= 60 ~ "Adulto",
                                    Edad > 60 ~ "Mayor" )) 
attach(base)  # volver a apoderarse de la base con variable nueva
names(base)   # para ver los nombres las variables incluidas en la base

table(Edad_cod)



