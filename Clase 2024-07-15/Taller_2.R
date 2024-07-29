##### MRLM #####
# La regresión lineal múltiple permite generar un modelo lineal en el que el valor de la variable 
# dependiente o respuesta (Y) se determina a partir de un conjunto de variables independientes 
# llamadas predictores (X1, X2, X3,…). 
# Los modelos de regresión múltiple pueden emplearse para predecir el valor de la variable dependiente 
# o para evaluar la influencia que tienen los predictores sobre ella 
# (esto último se debe que analizar con precaución para no malinterpretar causa-efecto)

#1. Analizar la relación entre variables
#     - graficos de dispersión
#     - coeficiente de correlación
#2. Generar modelos
#     - lm()
#3. Selección de los mejores predictores
#     - step
#4. Validacion de supuestos
#5. Valores átipicos o influyentes
#6. Multicolinealidad

##### EJERCICIO #####
# Obtener el MRLM que permita explicar el salario (en m$)
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
base <- dbGetQuery(con, "SELECT * FROM [dbo].[Salario]")
dbDisconnect(con)
head(base)
attach(base)

mi_modelo <- lm(Salario ~ Antig + Exper)
summary(mi_modelo)
anova(mi_modelo)
#Adjusted R-squared:  0.499

mi_modelo_3 <- lm(Salario ~ Antig + Exper+ Cargo)
summary(mi_modelo_3)
anova(mi_modelo_3)
#Adjusted R-squared:  0.9498 

mi_modelo_2 <- lm(Salario ~ Antig + Cargo)
summary(mi_modelo_2)
anova(mi_modelo_2)
#Adjusted R-squared:  0.9501

#setwd("d:/dev/Estadistica/Bases de datos/")
#base <- readxl::read_excel("Salarios.xlsx")
#head(base)
#attach(base)  


#------------- Descripción por cargo
library(psych)
describeBy(Salario,group=Cargo) 
# otra forma de describir
tapply(Salario,Cargo, summary)  
tapply(Salario,Cargo, sd)  # agrega desviación estándar


#----------- Gráficos de relación entre todas las variables 
library(GGally)
ggpairs(base)
# es equivalente a la función "pairs" (matriz de dispersión, Taller_1), pero permite variables 
# continuas como categóricas
# Muestra: - correlación entre las variables continuas
#          - diagramas de dispersión entre las variables continuas
#          - gráficos de densidad de las variables continuas
#          - histogramas
#          - box plots de la combinación entre variables continuas y categóricas.


#----------- Gráficos dispersión y ajuste: Salario v/s Antig, Exper, Edad
par(mfrow=c(1,3)) 
plot(Salario~Antig)
abline(lm(Salario~Antig)) 
plot(Salario~Exper)
abline(lm(Salario~Exper)) 
plot(Salario~Edad)
abline(lm(Salario~Edad)) 
par(mfrow=c(1,1)) 

library(ggplot2)
g1 <- ggplot(base, aes(x=Antig, y=Salario)) + 
  geom_point() +
  geom_smooth(method='lm', formula=y~x, se=FALSE, col='orange')
g2 <- ggplot(base, aes(x=Exper, y=Salario)) + 
  geom_point() +
  geom_smooth(method='lm', formula=y~x, se=FALSE, col='orange')
g3 <- ggplot(base, aes(x=Edad, y=Salario)) + 
  geom_point() +
  geom_smooth(method='lm', formula=y~x, se=FALSE, col='orange')

library(gridExtra)
grid.arrange(g1,g2,g3, ncol = 3)


#----------- Gráficos de dispersión por sexo
g4<- ggplot(base,aes(Antig,Salario,col=Sexo))+
  geom_point(size=3)
g5<- ggplot(base,aes(Exper,Salario,col=Sexo))+
  geom_point(size=3)
g6<- ggplot(base,aes(Edad,Salario,col=Sexo))+
  geom_point(size=3)
grid.arrange(g4,g5,g6, ncol = 3)


#----------- Gráficos de dispersión por Cargo
g7<- ggplot(base,aes(Antig,Salario,col=Cargo))+
  geom_point(size=3)
g8<- ggplot(base,aes(Exper,Salario,col=Cargo))+
  geom_point(size=3)
g9<- ggplot(base,aes(Edad,Salario,col=Cargo))+
  geom_point(size=3)
grid.arrange(g7, g8, g9, ncol = 3)


#----------- Correlaciones y test para ro
cor(base[,c(6,3:5)]) #correlacion de las filas 6, 3, 4 y 5  (únicas variables númericas) 
#matriz de correlacion

# test para ro  H0: ro = 0 
cor.test(Salario,Antig)
cor.test(Salario,Exper)
cor.test(Salario,Edad) #p-value = 0.001992 No tiene correlación lineal


#------------ Modelos simples
# Salario = B0 +  B1*Antiguedad
Mod1 <- lm(Salario~Antig)
summary(Mod1)    #R2 = 0,384 y significativa

# Salario = B0 +  B1*Exper
Mod2 <- lm(Salario~Exper)
summary(Mod2)    #R2 = 0,4599 y significativa

# Salario = B0 +  B1*Edad
Mod3 <- lm(Salario~Edad)
summary(Mod3)   #R2 = 0,1755 y significativa

# Salario = B0 +  B1*Sexo
Mod4 <- lm(Salario~Sexo)
summary(Mod4)    #R2 = 0,05134 y no significativo


#-------------Modelos múltiples (muchos!!)
# Salario = B0 +  B1 * Sexo + B2 * Cargo
Mod5 <- lm(Salario ~ Sexo + Cargo)
summary(Mod5)   # R2= 0,9261 y sexo no significativo

# Salario = B0 +  B1*Antig +B2*Sexo
Mod6 <- lm(Salario ~ Antig + Sexo)
summary(Mod6)    #R2 = 0.359  y sexo no significativo

# Salario = B0 +  B1*Exper +B2*Sexo
Mod7 <- lm(Salario ~ Exper + Sexo)
summary(Mod7)   #R2 =  0.467 y sexo no significativo

# Salario = B0 +  B1*Exper +B2*Edad
Mod8 <- lm(Salario~Exper+Edad)
summary(Mod8)   #R2 =  0.4562 y edad no significativo

# Salario = B0 +  B1*Antig +B2*Sexo + B3*Cargo
Mod9 <- lm(Salario ~ Antig + Sexo+ Cargo)
summary(Mod9)  #R2 = 0.9495

# Salario = B0 +  B1*Exper +B2*Sexo + B3*Cargo
Mod10 <- lm(Salario ~ Exper + Sexo + Cargo)
summary(Mod10)   #R2 = 0.928

# Salario = B0 +  B1*Antig + B2*Exper + B3*Sexo + B4*Cargo
Mod11 <- lm(Salario ~ Antig + Exper + Sexo + Cargo)
summary(Mod11)   #R2=0.9499

# Salario = B0 +  B1*Antig +B2*Exper + B3*Cargo + B4*Edad
Mod12 <- lm(Salario ~ Antig + Exper + Sexo + Cargo + Edad)
summary(Mod12)   #R2=0.9499


#------------ Comparamos dos modelos (uno incluido en el otro)
anova(Mod6,Mod11)  # diferencia entre dos modelos 
#valor p=2.2e-16. Entonces la diferencia es significativa. Pero no se sabe cual de las dos.


#---------------- Métricas para seleccionar mejor modelo
cbind(AIC(Mod6),AIC(Mod11))  # mínimo AIC es el mejor
cbind(BIC(Mod6),BIC(Mod11))  # mínimo BIC es el mejor


#------------- Selección automática - los tres métodos
# A la hora de seleccionar los predictores que deben formar parte del modelo se pueden seguir varios métodos:
# Método jerárquico: basándose en el criterio del analista, se introducen unos predictores 
#                    determinados en un orden determinado.
# Método de entrada forzada: se introducen todos los predictores simultáneamente.
# Método paso a paso (stepwise): emplea criterios matemáticos para decidir qué predictores 
#                                contribuyen significativamente al modelo y en qué orden se introducen. 
#                                Dentro de este método se diferencias tres estrategias:
#                     - backward
#                     - forward
#                     - mixto


vacio    <- lm(Salario ~ 1, base) # modelo sin variables, solo con intercepto
completo <- lm(Salario ~ ., base) # modelo con todas las variables

# La función step() permite encontrar el mejor modelo basado en AIC 
# utilizando cualquiera de las 3 variantes del método paso a paso

# a) Backward: parte del modelo completo y quita variables
# El modelo se inicia con todas las variables disponibles incluidas como predictores. 
# Se prueba a eliminar una a una cada variable, si se mejora el modelo, queda excluida. 
# Este método permite evaluar cada variable en presencia de las otras
M1<- step(completo,direction="backward")
summary(M1) #Adjusted R-squared:  0.9501

# b)Forward, parte del modelo vacío y agrega
# El modelo inicial no contiene ningún predictor, solo el parámetro B0.
# A partir de este se generan todos los posibles modelos introduciendo una sola variable de entre 
# las disponibles. Aquella variable que mejore en mayor medida el modelo se selecciona.
# En el caso de que varias lo hagan, se selecciona la que incremente en mayor medida la capacidad del modelo. 
# Este proceso se repite hasta llegar al punto en el que ninguna de las variables que quedan por incorporar 
# mejore el modelo.
M2<- step(vacio,direction="forward",scope=formula(completo))
# El argumento scope puede especificar los límites de las variables que pueden entrar o salir del modelo 
# en el proceso de selección. En este caso los límites es el modelo con todas las variables.
summary(M2) #Adjusted R-squared:  0.9501 

# c) paso a paso
# Se trata de una combinación de la selección forward y backward. 
# Se inicia igual que el forward pero tras cada nueva incorporación se realiza un test de extracción de 
# predictores no útiles como en el backward. Presenta la ventaja de que si a medida que se añaden predictores, 
# alguno de los ya presentes deja de contribuir al modelo, se elimina
M3<- step(vacio,direction="both",scope=formula(completo)) 
summary(M3) #Adjusted R-squared:  0.9501 


# Los tres métodos seleccionan Antig y Cargo como predictores.
# Escogemos M1



#------------- Multicolinealidad
car::vif(completo)
# Un VIF > 10 indica multicolinealidad

car::vif(lm(Salario ~ Exper + Antig + Edad, base))  #sirve para variables que son numericas

#------------- Supuestos
summary(M1)   #R2aj = 0.9501 
plot(M1,1)    #linealidad  
plot(M1,2)    #normalidad, Datos anómalos: ¿30, 22, 24?
plot(M1,3)    #homocedasticidad
plot(M1,4)    #Cook (influyente) Datos: ¿15, 21, 24?
plot(M1,5)    #Leverage Datos: ¿15, 21, 24?  datos anomalos

car::residualPlots(M1) #test de linealidad
nortest::lillie.test(M1$residual) #p-value = 0.003552 < alpha = 5%, se rechaza H0. No hay normalidad
lmtest::bptest(M1) #cumple homocedasticidad BP = 5.419, df = 3, p-value = 0.1436

# Anomalías
car::outlierTest(M1) # dato 24
summary(influence.measures(M1)) # Datos: 21, 22, 24

#------------- Opciones para mejorar el modelo de acuerdo a los supuestos
# Podemos agregar la variable Sexo (u otra variable) para probar si los anómalos "desaparecen".
Mod13 <- lm(Salario ~ Antig + Cargo + Sexo)
summary(Mod13) #Adjusted R-squared:  0.9495 

plot(Mod13,2)    #normalidad, Datos anómalos 41, 22, 24?
car::outlierTest(Mod13)
nortest::lillie.test(Mod13$residual) #p-value = 0.03622< alpha = 5%, se rechaza H0. No hay normalidad

# No funciona, entonces sacamos el dato anómalo 24 y vuelvo al modelo anterior, M1
base2 <- base[c(1:23,25:52),] #modelo sin el dato anomalo 24

vacio2 <- lm(base2$Salario~1,base2)    
completo2 <- lm(base2$Salario~.,base2)
M1_nuevo<- step(completo2,direction="backward")
summary(M1_nuevo) # Cargo + Antig + Exper, Adjusted R-squared:  0.9676, Exper no significativa
summary(M1)       # Cargo + Antig,         Adjusted R-squared:  0.9501

# revisamos los supuestos del nueno modelo
plot(M1_nuevo,1)    #linealidad  
plot(M1_nuevo,2)    #normalidad, Datos anómalos 15, 22, 27?
plot(M1_nuevo,3)    #homocedasticidad
plot(M1,4)          #Cook (influyente)  dato 15, 21, 22?
plot(M1_nuevo,5)    #Leverage    dato 15, 21, 22?

car::outlierTest(M1_nuevo) #dato 22 
nortest::lillie.test(M1_nuevo$residual) #p-value = 0.03459 < alpha = 5%, se rechaza H0. No hay normalidad

# Otra opción, log(y) con todos los datos. 
# Al aplicar una transformación logarítmica, 
# las diferencias en magnitudes grandes se reducen, 
# lo que puede ayudar a estabilizar la varianza.
modelo <- lm(log(Salario) ~ Cargo + Antig, base)
summary(modelo)
plot(modelo,2)
car::outlierTest(modelo)
nortest::lillie.test(modelo$residual) #p-value = 0.001157 < alpha = 5%, se rechaza H0. No hay normalidad

#otra opción, log(y) sacando datos anómalo, base2
modelo2 <- lm(log(base2$Salario) ~ base2$Cargo + base2$Antig, base2)
plot(modelo2,2) 
car::outlierTest(modelo2)
nortest::lillie.test(modelo2$residual) #p-value = 0.02389 < alpha = 5%, se rechaza H0. No hay normalidad

# Vamos a sacar un influyente y un anómalo, 22 y 24
base3 <- base[c(1:21,23,25:52),]
modelo3 <- lm(base3$Salario ~ base3$Cargo + base3$Antig, base3)
plot(modelo3,2) 
car::outlierTest(modelo3)
nortest::lillie.test(modelo3$residual) # p-value = 0.0538 > alpha = 5%, No se rechaza H0. Hay normalidad.

car::residualPlots(modelo3) #test de linealidad
lmtest::bptest(modelo3)     # ¿cumple homocedasticidad?
#BP = 9.2221, df = 3, p-value = 0.02648  se rechaza h0
summary(modelo3)






boxplot(base)


