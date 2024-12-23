#setwd("d:/dev/Estadistica/clase 2024-07-08/")
#datos<- readxl::read_excel("Datos.xlsx")
library(DBI)
library(odbc)
# Establece la conexión a la base de datos
con <- dbConnect(odbc::odbc(),
                 Driver = "ODBC Driver 17 for Sql Server", 
                 Server = "localhost",
                 Database = "DataEstadistica",
                 UID = "rodrigo",
                 PWD = "enter",
                 Port = 1433)
datos <- dbGetQuery(con, "SELECT * FROM [dbo].[Soldadura]")
dbDisconnect(con)
head(datos)
attach(datos)

# Se desea ajustar un modelo de regresión lineal simple para explicar 
# la Resistencia de una soldadura en función de la Edad de la soldadura.


           ########### CORRELACIÓN ###########
# Grado de asociación
# Diagrama de dispersión
library(ggplot2)
ggplot(datos, aes(x=Edad, y=Resistencia)) + 
  geom_point() + theme_light()

# Matriz de dispersión
library(GGally)
pairs(datos,upper.panel= panel.smooth, lower.panel = panel.smooth)  #lower.panel=NULL

correlacion <- cor(datos) #por defecto es pearson (method='pearson'), otros valores posibles son kendall y spearman.
# r = 1 -> perfecta asociación lineal
# r < 0 -> correlación negativa, si una aumenta la otra disminuye
print(correlacion)
# Matriz de correlacion
library(corrplot)
corrplot(correlacion, type="lower")
corrplot(correlacion, type="lower", method = "square")
corrplot(correlacion, type="lower", method = "number")

# A medida que aumenta la edad de la soldadura, la resistencia que ella ofrece disminuye.
# Este indicador nos muestra la asociación entre las variables pero no la causalidad.




           ########### Modelo de regresión lineal simple (MRLS) ###########
# El Análisis de Regresión se usa cuando se sabe que existe una relación lineal entre las variables.
#1. Especificación del modelo
#2. Estimación del modelo
#3. Inferencia
#4. Análisis de la varianza
#5. Coeficiente de determinación
#6. Análisis de supuestos de residuos: linealidad, homocedasticidad, independencia, normalidad


#1.Especificación del modelo 
# El modelo que se va a ajustar es: 
# Resistencia = B0 + B1 * Edad
# Supuestos: - los residuos distribuyen normal
#            - los residuos son independientes
#            - los residuos tienen varianza constante (Homocedasticidad)

#2. Estimación del modelo
# Para obtener las estimaciones de los parámetros del modelo anterior se usa el comando lm
mod <- lm(Resistencia ~ Edad, data=datos)
# Resistencia ~ Edad indica que Resistencia es la variable respuesta y que Edad es la variable explicativa
summary(mod)

# Resistencia = 2596.856 -33.556 * Edad

# - Por cada semana que envejezca la soldadura, se espera que la resistencia promedio disminuya en 33.556 psi.
# - Si la soldadura es nueva (Edad=0), se espera que la resistencia promedio sea de 2596.856 psi.

# Podemos obtener, si quisieramos , los IC para los parametros
confint(mod) #caso poblacional
# si la soldadura es nueva T=0 , para la poblacion en promedio
# la resistencia está entre 2477.28347 y 2716.42864 con 95% de confianza

# Incluyamos la recta de regresión que representa el modelo ajustado anterior
ggplot(datos, aes(x=Edad, y=Resistencia)) + 
  geom_point() +
  geom_smooth(method='lm', formula=y~x, se=TRUE, col='darkblue')
# opción se=FALSE o TRUE, muestra el intervalo de confianza de la regresión.


#3. Inferencia (hipotesis para validar si la variable edad es significativa )
# H0: B1  = 0 
# H1: B1 ≠ 0
summary(mod)
# Estadístico T = -9.334 con valor p = 1.58e-08 < alpha=5%, 
#se rechazo H0, la variable Edad (X) sí explica Resistencia (Y) 


#4. Análisis de la varianza
# A cada coeficiente del modelo de Regresión Lineal Simple 
# se le puede asociar un test. 

# H0: No existe regresión 
# H1: existe regresión
anova <- aov(mod)
summary(mod) #está todo solo para 1 variable
#Residual standard error: 126.4 es la raiz del valor de lo que sale en la anova

summary(anova) #Entrega el test F
# valor p=1.58e-08 < alpha = 5%, se rechaza H0, existe regresión.
# 15970 es la varianza estimada para todas las normales

par(mfrow = c(2, 2))
plot(anova)
par(mfrow = c(1, 1))

# Residual vs Fitted: Deberia estar distribuidos aleatoriamente alrededor de
# la linea horizontal que representa un error residual de cero

# Normal Q-Q: deberia sugerir que los errores residuales se distribuyen
# normalmente.

# Scale-Location Muestra la raiz cuadrada de los residuos estandarizados,
# como una funcion de los valores ajustados. No deberia existir una
# tendencia clara en ese trama.

# Residual vs Leverage Las distancias mas grandes que 1 son sospechosos y
# sugieren la presencia de un valor atipico posible y su eliminacion podria
# tener efectos sobre la regresion.


#5. Coeficientes de determinación
# El coeficiente de determinación, es el cuadrado del coeficiente de correlación 
# de Pearson y
# mide la bondad del ajuste de la recta a los datos.
summary(mod)
# Multiple R-squared:  0.821,	Adjusted R-squared:  0.8115 
#Un R^2=82,1% de la variabilidad de Y (Resistencia) puede ser explicada por el modelo


#6.Análisis de supuestos
# Linealidad
plot(mod, 1)

# Normalidad
plot(mod, 2)
# H0: residuos son Normales
# H1: residuos no son Normales
ks.test(mod$residuals, "pnorm", mean(mod$residuals), sd(mod$residuals))
nortest::lillie.test(mod$residuals) #recomendado para n > 50
# p-value = 0.09597 > alpha = 5%, no se rechaza H0. Los residuos son normales
shapiro.test(mod$residuals)

# Homocedasticidad  
# H0: Homocedasticidad
# H1: Heterocedasticidad
lmtest::bptest(mod)
# BP = 1.8626, df = 1, p-value = 0.1723

# Independencia (en casos temporales)
# H0: No hay autocorrelacion
# H1: hay autocorrelacion
lmtest::dwtest(mod)
# DW = 1.7685, p-value = 0.3061 Si hay independencia


########### Anomalías ########
library(car)
outlierTest(mod)
summary(influence.measures(mod))
# h0 es que no hay anomalia , h0 es hipotesis de no cambio

#Podemos sacar el datos número 21 y realizamos el modelo nuevamente.
datos2 <- datos[1:20,]
datos2 <- datos[-21, ]

mod2 <- lm(Resistencia ~ Edad, datos2)
summary(mod2)
# modelo 1: Resistencia = 2596.856 - 33.556 * Edad # con los 21 registros
# R-squared:  0.821
# modelo 2: Resistencia = 2627.822 - 37.154 * Edad # sin el registro "21"
# R-squared:  0.9018 tiene mejor porcentaje aunque se debe invertigar si eliminar o no el registro

g_mod1<- ggplot(datos, aes(x=Edad, y=Resistencia)) + 
  geom_point() +
  geom_smooth(method='lm', formula=y~x, se=TRUE, col='darkblue') +
  labs(title = "Modelo 1")

g_mod2<- ggplot(datos2, aes(x=Edad, y=Resistencia)) + 
  geom_point() +
  geom_smooth(method='lm', formula=y~x, se=TRUE, col='darkred') +
  labs(title = "Modelo 2 sin un dato")

library(gridExtra)
grid.arrange(g_mod1,g_mod2, ncol = 2)



           ########### Predicción ###########

#Podemos crear un diagrama de dispersión con los puntos originales, las estimaciones y los residuales. 
# Primero se debe agregar a los datos originales el vector con las estimaciones  

datos2$predicciones <- predict(mod2)

ggplot(datos2, aes(x=Edad, y=Resistencia)) +
  geom_smooth(method="lm", se=FALSE, color="lightgrey") +
  geom_segment(aes(xend=Edad, yend=predicciones), col='red', lty='dashed') +
  geom_point() +
  geom_point(aes(y=predicciones), col='red')

# los punto negros son los datos originales, los rojos son las estimaciones, 
# los residuales son las lineas de color rojo



