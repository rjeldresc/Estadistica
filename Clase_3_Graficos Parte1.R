##############  PAQUETES  ############## 
install.packages("readxl") 
install.packages("ggplot2") 
install.packages("ggthemes")
install.packages("plotly")#interactivo
install.packages("plotrix") #circular
install.packages("vioplot") #de violin
install.packages("GGally")


setwd("C:/dev/Estadistica/") #Definición de directorio de trabajo

base <- readxl::read_excel("Base_Taller3.xlsx", sheet = "Parte1") #Importar y definir base de datos
attach(base) #para reconocer por el nombre las columanas importadas

##############  HISTOGRAMA  ##############
hist(Ingreso_Anual)
hist(Ingreso_Anual,main="Histograma",xlab="Ingresos",ylab="Frecuencia")
hist(Ingreso_Anual,main="Histograma",xlab="Ingresos",ylab="Frecuencia",col="green")
hist(Ingreso_Anual,main="Histograma",xlab="Ingresos",ylab="Frecuencia",col="green",density=20) #rayado de la barra
hist(Ingreso_Anual,main="Histograma",xlab="Ingresos",ylab="Frecuencia",col="green",density=20,freq=TRUE)
hist(Ingreso_Anual,main="Histograma",xlab="Ingresos",col="green",density=20,freq=FALSE) #entrega la probabilidad de densidad

#recordatorio de como guardar gráficos

#para crear archivo png 
png(filename = paste0(getwd(),"/Histograma.png"), width = 800, height=600)

hist(Ingreso_Anual,main="Histograma",xlab="Ingresos",ylab="Frecuencia",col="green",density=20,freq=TRUE)
dev.off() #para cerrarla funcion decrear archivo png

#otro tipo de histograma
library(ggplot2) 
ggplot(base,aes(x=Ingreso_Anual))+geom_histogram()
ggplot(base,aes(x=Ingreso_Anual,color=Sexo))+geom_histogram()
ggplot(base,aes(x=Ingreso_Anual,color=Sexo,fill=Sexo))+geom_histogram()
ggplot(base,aes(x=Ingreso_Anual,color=Sexo,fill=Sexo))+geom_histogram(alpha=0.1) #transparencia
ggplot(base,aes(x=Ingreso_Anual,color=Sexo,fill=Sexo))+geom_histogram(alpha=0.3,bins=10) #10 clases o rangos
ggplot(base,aes(x=Ingreso_Anual,color=Sexo,fill=Sexo))+geom_histogram(alpha=0.3,bins=10, binwidth=5000) #ancho de barra
ggplot(base,aes(x=Ingreso_Anual,color=Sexo,fill=Sexo))+geom_histogram(apha=0.3,bins=10,binwidth=5000, position = "identity") 
ggplot(base,aes(x=Ingreso_Anual,color=Sexo,fill=Sexo))+geom_histogram(alpha=0.3,bins=10,binwidth=5000, position = "stack")

#interactivo
library(plotly)
plot_ly(base, x = ~Ingreso_Anual, type = "histogram") 


##############  Barras  ##############
t1 <- table(Sucursal) 
t2 <- round(prop.table(t1)*100,1) #frecuencia relativa porcentual
barplot(t2, col= "lightblue", main="Sucursales (%)", xlab = "Sucursales", ylab="%")
barplot(t1,legend.text=rownames(t1), main="Barras",ylab ="Frecuencias", font.axis=5) #leyenda y titulo y nombre eje y y cambio fuente
barplot(t1,legend.text=rownames(t1),main="Barras",ylab ="Frecuencias",col=rainbow(4)) #color de las barras
barplot(t1,legend.text=rownames(t1),main="Barras",ylab ="Frecuencias", col=rainbow(4),ylim=c(0,300), xlim=c(0,5)) #limites eje y. xlim=tamaño eje x

t3<- table(Sucursal,Sexo)  
barplot(t3,beside=TRUE,legend.text=rownames(t2),col=rainbow(4)) #beside para apilar
barplot(t3,beside=FALSE,legend.text=rownames(t2),col=rainbow(4)) 

#Otro tipo de graf barras
ggplot(base,aes(x=Sexo,fill=Sucursal))+geom_bar()
ggplot(base,aes(x=Sexo,fill=Sucursal))+geom_bar(width=0.8)+theme_light() #ancho de barra y color de fondo

#interactivo
tabla_plot <- data.frame(
  Sucursales = names(table(Sucursal)),
  Total = as.numeric(table(Sucursal)),
  Masc = as.numeric(table(Sucursal[Sexo == "Masc"])),
  Fem = as.numeric(table(Sucursal[Sexo == "Fem"]))) 

#crea una nueva tabla donde Sucursales es una columna con los nombres que aparece en "Sucursales" de la base1. estructura de datos de dos dimesiones, as.numeric = convertir a numero
#otra columna es Total y coloca el total de surcursales de acuerdo a cada tipo, luego dos columnas mas con el total de acuerdo a masc o femenino

plot_ly(data = tabla_plot, x = ~Sucursales, y = ~Masc, type = "bar", name = "Masc") %>%
  add_trace(y = ~Fem, name = "Fem") %>%
  layout(yaxis = list(title = "Personas"))

##############  CIRCULAR  ##############
pie(t2, main="Sucursales", clockwise = FALSE, labels = paste0(t2, "%")) # FALSE = dirección contraria al reloj
pie(t2, main="Sucursales", clockwise = TRUE,labels = paste0(t2, "%"))  # TRUE = dirección sentido reloj
pie(t2, main="Sucursales", clockwise = TRUE, labels = paste0(rownames(t2), " = ", paste0(t2, "%")) ) #agrega leyenda en cada seccion

library(plotrix)
pie3D(t2, main="Sucursales", labels =  paste0(rownames(t2), " = ", paste0(t2, "%")))  
pie3D(t2, main="Sucursales", labels =  paste0(rownames(t2), " = ", paste0(t2, "%")),explode =0.1)  # explode = separa los sectores

#interactivo
plot_ly(data = tabla_plot, labels = ~Sucursales, values = ~Total, type = "pie")

##############  DE CAJA  ##############
boxplot(Ingreso_Anual, horizontal=TRUE, main="Distribución de Ingresos", col="green")
boxplot(Ingreso_Anual ~ Sexo, col="lightblue", main="Distribución de Ingresos") #conrespecto a otra variable
boxplot(Ingreso_Anual ~ Sexo, col="cyan", main="Distribución de Ingresos", horizontal=TRUE)#horizontal

#otro tipo de graf de caja
ggplot(base,aes(x = Sexo, y = Ingreso_Anual, fill = Sexo)) + geom_boxplot(outlier.colour="red") +
  stat_summary(fun.y = "mean", geom = "point", shape = 20, size = 3)+ 
  ggtitle("Grafico de cajas") +  xlab("Sexo") +  ylab("Ingreso anual") + 
  theme_bw()    # size = tamaño legenda

#interactivo
plot_ly(data = base, x = ~Sexo, y = ~Ingreso_Anual, type = "box")


##############  DE VIOLIN  ##############
library(vioplot)
vioplot(Ingreso_Anual ~Sexo, col ="magenta", main="Distribución de Ingresos")
vioplot(Ingreso_Anual ~Sexo, col ="magenta", main="Distribución de Ingresos", horizontal=TRUE)

#otro tipo de violin
ggplot(base,aes(x = Sexo, y = Ingreso_Anual, fill = Sexo)) + geom_violin() + 
  geom_boxplot(outlier.colour = "red", width = 0.1) + 
  theme_linedraw()   # width = tamaño gráfico de caja

#interactivo
plot_ly(data = base, x = ~Sexo, y = ~Ingreso_Anual, type = "violin",
        box = list(visible = T))


##############  DISPERSION  ##############
plot(Edad, Ingreso_Anual, pch = 0) #x,y
plot(Edad, Ingreso_Anual, pch =20) #pch mas grande el numero mas pequeño
plot (Edad, Ingreso_Anual, pch = 19,col=as.factor(Sexo)) #color separado por sexo
legend("topleft", legend = levels(as.factor(Sexo)), fill=as.factor(Sexo))      

#otro tipo de graf dispersion
ggplot(base,aes(x = Edad, y = Ingreso_Anual)) + geom_point() + theme_bw()  # fondo blanco
ggplot(base,aes(x = Edad, y = Ingreso_Anual)) + geom_point() + theme_bw()   #separado por sexo
ggplot(base,aes(x = Edad, y = Ingreso_Anual)) + geom_point() + theme_linedraw()  # agrega cuadrícula en el fondo
ggplot(base,aes(x = Edad, y = Ingreso_Anual)) + geom_point() + theme_dark()  # agrega color en el fondo


##############  DENSIDADES  ##############
ggplot(base,aes(Ingreso_Anual, fill=Sexo)) + geom_density(alpha=0.7)
ggplot(base,aes(Ingreso_Anual, fill=Crédito)) + geom_density(alpha=0.7)
ggplot(base,aes(Ingreso_Anual, fill=Bienes)) + geom_density(alpha=0.7)
ggplot(base,aes(Ingreso_Anual, fill=Sucursal)) + geom_density(alpha=0.7)


##############  TODAS LAS VARIABLES  ##############
library(GGally)
GGally::ggpairs(base) 
#grafica en forma de matriz un set de datos con múltiples variables, el resultado es una correlación de las variables.
