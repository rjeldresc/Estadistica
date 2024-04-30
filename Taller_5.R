##############  PAQUETES  ############## 
#install.packages("readxl") #para leer archivos excel
#install.packages("psych") 
#install.packages("ggplot2") 
install.packages("openxlsx") #para exportar a excel
#install.packages("gridExtra")



#setwd("C:/Users/caroh/Documents/RStudio/Clases/Clase 4") #Definición de directorio de trabajo
setwd("d:/dev/Estadistica")
######## DOS VARIABLES CATEGÓRICAS ########

#### Crear data frame ####

datos <- matrix(c(2,22,6,17,41,12,24,32,8),3,3)
colnames(datos)=c("Malo","Regular","Bueno")
rownames(datos)=c("A","B","C")

base1 <- as.data.frame(datos)
attach(base1)

#### Distribucion porcentual del Tipo y Estado(Malo, Regular, Bueno) ####
tabla <- as.table(datos)
t1 <- round(prop.table(tabla)*100,0)
addmargins(t1)

#### Distribucion porcentual del Tipo, según Estado(Malo, Regular, Bueno) ####
t2 <- round(prop.table(tabla, margin = 2)*100,0) #número 2,  con respecto a las columanas
addmargins(t2,1) 

#### Distribucion porcentual del Estado(Malo, Regular, Bueno), según Tipo ####
t3 <- round(prop.table(tabla,1)*100,0)   #número 1, con respecto a las filas
addmargins(t3,2)  



#### GRÁFICOS #### 
barplot(t2,
        col=c("blue","red","green"), 
        main="Distribución porcentual del tipo de vivienda según el estado", 
        ylab="%",
        legend.text=rownames(t2),
        beside=TRUE,
        ylim=c(0,80))

barplot(t2,
        col=c("blue","red","green"), 
        main="Distribución porcentual del tipo del estado según tipo de vivienda", 
        ylab="%",
        legend.text=rownames(t2))


#ojo!!! el gráfico que queremos llegar (el de la presentacion) no es igual
barplot(t3,
        col=c("blue","red","green"), 
        main="Distribución porcentual del tipo de vivienda según el estado", 
        ylab="%",
        legend.text=rownames(t2),
        beside=TRUE,
        ylim=c(0,80))

t4 <- t(as.matrix(t3)) #para invertir los ejes al graficar
barplot(t4,
        col=rainbow(3), 
        main="Distribución porcentual del tipo del estado según tipo de vivienda", 
        ylab="%",
        legend.text=rownames(t4),
        beside=TRUE,
        ylim=c(0,60))

barplot(t4,
        col=rainbow(3), 
        main="Distribución porcentual del tipo de vivienda según el estado", 
        ylab="%",
        legend.text=rownames(t4))


######## DOS VARIABLES CONTINUAS ########
base2 <- readxl::read_excel("Base_ejemplo__Bivariada.xlsx", sheet = "parte2") 
attach(base2) 

#Edad
mean(Edad)  # media
sd(Edad)    # desviación estándar
sd(Edad)/mean(Edad)  # coeficiente de variación

#Colesterol
mean(Colesterol)  # media
sd(Colesterol)    # desviación estándar
sd(Colesterol)/mean(Colesterol)  # coeficiente de variación

library(psych)
psych::describeBy(Edad) 
psych::describeBy(Colesterol) 


######## UNA VARIABLE CONTINUA Y UNA CATEGÓRICA ########
hombres <- c(72,69,98,66,85,76,79,80,77) #vector con 9 elementos
mujeres <- c(81,67,90,78,81,80,76) #vector con 7 elementos

resultado_hombre <- summary(hombres) 
resultado_mujeres <- summary(mujeres) 

#un recordatorio para guardar tablas
write.xlsx(resultado_hombre, "Resultado.xlsx",sheetName = "Hombres") #en xlsx
write.xlsx(resultado_mujeres, "Resultado.xlsx",sheetName = "Mujeres") #en xlsx

#GRÁFICOS
grupos <- rep(c("Hombres", "Mujeres"), c(9, 7)) #rep(): repite elementos
datos2 <- data.frame(grupos, c(hombres, mujeres))
colnames(datos2) <- c("sexo", "valor")
attach(datos2)
boxplot(valor~sexo)

library(ggplot2) 
ggplot(datos2, aes(x = valor, color = sexo, fill = sexo)) + 
  geom_density(alpha=0.7)   # gráfico de densidades; alpha, transparencia


#grafico de puntos
x1<-seq(1,9,1)
x2<-seq(1,7,1)
p1<-plot(x1,hombres)
p2<-plot(x2,mujeres)

par(mfrow=c(1,2))
p1<-plot(x1,hombres)
p2<-plot(x2,mujeres)
