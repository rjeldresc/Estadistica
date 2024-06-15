

### PARTE 1 - BASE SALARIOS ####
setwd("d:/dev/Estadistica/")
#setwd("C:/Users/Matias/Desktop/Diplomado 2024")
base1<-readxl::read_excel("Control_1.xlsx", sheet="Salarios")
head(base1)
attach(base1)

### PREGUNTA A ###

### a.1) Porcentajes por sexo
table(Sexo) ->t1     # frecuencias
round(prop.table(t1)*100, 1) -> t2   # porcentajes
t1
t2

# si uno quisiera embellecer
cbind(t1, t2) -> t12
colnames(t12) <- c("Frec","%")
t12

# grafico
pie(table(Sexo))

### a.2) Tabla de cargos
table(Cargo, Sexo) -> t3
round(prop.table(t3, margin = 2)*100, 1) -> t4
t3
t4

# addmargins(t4, margin = 1)

# si uno quisiera embellecer
cbind(t3,t4) -> tabla  # tabla con ambos
colnames(tabla) <- c("Frec Fem","Frec Masc","% Fem","% Masc") 
# es posible poner los nombres asi, porque los % suman 100 por columna!! ojo
tabla

### a.3)Resumen de edad y salarios
# 1
tapply(Edad, Sexo, summary)
tapply(Salarios, Sexo, summary)
tapply(Salarios, Cargo, summary)

# 2
summary(base1$Edad)

aggregate(data = base1, Salarios ~ Sexo, FUN = summary)

aggregate(data = base1, Salarios ~ Cargo, FUN = summary)

## 3. completar el codigo
# base1 %>% group_by(Sexo) %>% 
#   summarise(minimo = min(Salarios),
#             maximo = max(Salarios))



### PREGUNTA B ###

#Gráficos de caja del salario
boxplot(Salarios~Sexo)
boxplot(Salarios~Cargo)


### PREGUNTA C ###

#Relación antiguedad-salarios (por sexo)

# automatizado
plot(Antig, Salarios, col=factor(Sexo), pch=19)
legend("topleft", legend = levels(factor(Sexo)), pch = 19,col = factor(levels(factor(Sexo)))) 

plot(Experiencia, Salarios, col=factor(Sexo), pch=15)
legend("topleft", legend = levels(factor(Sexo)), pch = 15,col = factor(levels(factor(Sexo)))) 

# manual
plot(Antig, Salarios, col=1:2, pch=19)
legend("topleft", legend = c("Fem","Masc"), pch = 19,col = 1:2) 

plot(Experiencia, Salarios, col=1:2, pch=15)
legend("topleft", legend = c("Fem","Masc"), pch = 15, col = 1:2) 




### PARTE 2 - BASE ESPECTACULOS ####

base2 <- readxl::read_excel("Control_1.xlsx", 
                            sheet="Espectaculos")
head(base2)
# attach(base2)

#Seleccionar 2 regiones
library(dplyr)
table(base2$REGION)
base2_reg <- filter(base2, REGION=="VALPARAISO" | REGION=="ATACAMA")
attach(base2_reg)
# head(base2_reg)

## Discusion: conteo o suma? ambas seran validas segun su interpretacion,
## ideal haber seguido la misma logica en todas las preguntas.

## a)Distribución porcentual de los espectáculos, según región
table(base2_reg$ESPECTACULOS, base2_reg$REGION) -> t5
round(prop.table(t5,2)*100, 2) -> t6
addmargins(t6,1)

barplot(t6, col=c(1:5), main="Tipos de Espectáculos", # col = rainbow(5)
        ylab="Porcentajes", legend.text = TRUE)


##b) gráfico de lineas con todos los datos

# primero debemos contar los tipos de espectaculos por año
table(base2_reg$AÑO, base2_reg$ESPECTACULOS) -> T1; T1
base3 <- as.data.frame(T1)
colnames(base3) = c("AÑO","ESPECTACULOS","frec")
base3
# attach(base3)

# base3 <- base2_reg %>% group_by(AÑO, ESPECTACULOS) %>% 
#   summarise(suma = sum(TOTAL_ESPECTAC), frec = n())

library(ggplot2)
ggplot(base3, aes(x = AÑO, y = frec, color=Espectaculos, group=ESPECTACULOS))+
  geom_point() +
  geom_line()



##c) gráfico de líneas solo dos regiones
library(ggplot2)
library(gridExtra)

table(base2_reg$AÑO,base2_reg$"ESPECTACULOS", base2_reg$REGION) ->T2
base4 <- as.data.frame(T2)
colnames(base4)=c("AÑO","Espectaculos","Region","frec")
# attach(base4)
base4

base5 <-filter(base4, Region=="VALPARAISO")
# attach(base5)
graf1 <- ggplot(base5, aes(AÑO, frec, color=Espectaculos, group=Espectaculos))+
  geom_point()+geom_line()+
  ggtitle("Región Valparaiso")


base6 <-filter(base4,Region=="ATACAMA")
# attach(base6)
graf2 <- ggplot(base6, aes(AÑO, frec, color=Espectaculos, group=Espectaculos))+
  geom_point()+geom_line()+
  ggtitle("Región Atacama")

grid.arrange(graf1, graf2, ncol = 2)









# -- Programacion
# -- Muy pocos controles y mucho contenido
# -- No se vio graficos de linea
# -- LLeva mucho tiempo generar y analizar, tema tiempo
# -- Quizas con mas de tiempo se hubiese hecho mejor