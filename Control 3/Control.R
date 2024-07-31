#library(dplyr)
#library(car)
#setwd("D:/dev/Estadistica/Bases de datos/")
#base <- readxl::read_excel("Contam.xlsx")
#head(base)
#nrow(base)

library(DBI)

# Establece la conexión a la base de datos
con <- dbConnect(odbc::odbc(),
                 Driver = "ODBC Driver 17 for Sql Server", 
                 Server = "localhost",
                 Database = "DataEstadistica",
                 UID = "rodrigo",
                 PWD = "enter",
                 Port = 1433)
base <- dbGetQuery(con, "SELECT * FROM [dbo].[Contam]")
dbDisconnect(con)
head(base)
attach(base)

# Pregunta 1 ####
# Obtenga el mejor modelo de regresión lineal simple basado en las variables meteorológicas.
#Graficos
par(mfrow = c(2,3))
plot(PM2.5 ~ Viento  , data = base, type = "p", pch = 20, bty = "n", las = 1)
plot(PM2.5 ~ TProm   , data = base, type = "p", pch = 20, bty = "n", las = 1)
plot(PM2.5 ~ TMin    , data = base, type = "p", pch = 20, bty = "n", las = 1)
plot(PM2.5 ~ TMax    , data = base, type = "p", pch = 20, bty = "n", las = 1)
plot(PM2.5 ~ Humed   , data = base, type = "p", pch = 20, bty = "n", las = 1)
dev.off()

Modelo1 <- lm(PM2.5 ~ Viento, data = base)
summary(Modelo1)$r.squared
Modelo2 <- lm(PM2.5 ~ TProm, data = base)
summary(Modelo2)$r.squared
Modelo3 <- lm(PM2.5 ~ TMin, data = base)
summary(Modelo3)$r.squared
Modelo4 <- lm(PM2.5 ~ TMax, data = base)
summary(Modelo4)$r.squared
Modelo5 <- lm(PM2.5 ~ Humed, data = base)
summary(Modelo5)$r.squared

#usando transformacion log
Modelo1 <- lm(PM2.5 ~ log(Viento)   , data = base); summary(Modelo1)$r.squared
Modelo2 <- lm(PM2.5 ~ log(TProm)    , data = base); summary(Modelo2)$r.squared
Modelo3 <- lm(PM2.5 ~ log(TMin)     , data = base); summary(Modelo3)$r.squared
Modelo4 <- lm(PM2.5 ~ log(TMax)     , data = base); summary(Modelo4)$r.squared
Modelo5 <- lm(PM2.5 ~ log(Humed)    , data = base); summary(Modelo5)$r.squared
#mejor modelo 0.4466271 log(TMin)

# Correlación entre PM2.5 y Viento
cor.test(base$PM2.5, base$Viento)

# Correlación entre PM2.5 y TProm
cor.test(base$PM2.5, base$TProm)

# Correlación entre PM2.5 y TMin
cor.test(base$PM2.5, base$TMin)

# Correlación entre PM2.5 y TMax
cor.test(base$PM2.5, base$TMax)

# Correlación entre PM2.5 y Humed
cor.test(base$PM2.5, base$Humed)



# Supuestos

# linealidad
plot(Modelo3, 1)
plot(Modelo3$fitted, Modelo3$residuals)
# cumple linealidad

# Normalidad
# H0: residuos son normales
# H1: residuos no son normales

plot(Modelo3, 2)
nortest::lillie.test(Modelo3$residuals)
ks.test(Modelo3$residuals, "pnorm", mean(Modelo3$residuals), sd(Modelo3$residuals))
# No existe normalidad

# Homoceasticidad
# H0: Si existe Homoceasticidad
# H1: No existe Homoceasticidad

lmtest::bptest(Modelo3)
# No existe Homoceasticidad

# Independencia
# H0: No hay autocorrelacion
# H1: hay autocorrelacion

lmtest::dwtest(Modelo3)
# Si existe Independencia

summary(Modelo3)

#Pregunta 2 Obtenga el mejor modelo de regresión lineal simple basado en los contaminantes atmosféricos.

par(mfrow = c(2,2))
plot(PM2.5 ~ NO, data = base, type = "p", pch = 20, bty = "n", las = 1)
plot(PM2.5 ~ NO2, data = base, type = "p", pch = 20, bty = "n", las = 1)
plot(PM2.5 ~ CO, data = base, type = "p", pch = 20, bty = "n", las = 1)
plot(PM2.5 ~ O3, data = base, type = "p", pch = 20, bty = "n", las = 1)
dev.off()

#contaminantes atmosféricos
Modelo1 <- lm(PM2.5 ~ NO, data = base)
summary(Modelo1)$r.squared
Modelo2 <- lm(PM2.5 ~ NO2, data = base)
summary(Modelo2)$r.squared
Modelo3 <- lm(PM2.5 ~ CO, data = base)
summary(Modelo3)$r.squared
Modelo4 <- lm(PM2.5 ~ O3, data = base)
summary(Modelo4)$r.squared

# Correlación entre PM2.5 y NO
cor.test(base$PM2.5, base$NO)
# t = , p-value = 

# Correlación entre PM2.5 y NO2
cor.test(base$PM2.5, base$NO2)
# t = , p-value = 

# Correlación entre PM2.5 y CO
cor.test(base$PM2.5, base$CO)
# t = , p-value = 

# Correlación entre PM2.5 y O3
cor.test(base$PM2.5, base$O3)
# t = , p-value = 




# Supuestos
# linealidad

plot(Modelo2, 1)
plot(Modelo2$fitted, Modelo2$residuals)
# pasa

# Normalidad
# H0: residuos son normales
# H1: residuos no son normales
plot(Modelo2, 2)
nortest::lillie.test(Modelo2$residuals)
ks.test(Modelo2$residuals, "pnorm", mean(Modelo2$residuals), sd(Modelo2$residuals))
# No existe normalidad

# Homoceasticidad
# H0: Si existe Homoceasticidad
# H1: No existe Homoceasticidad

lmtest::bptest(Modelo2)
# No existe Homoceasticidad

# Independencia
# H0: No hay autocorrelacion
# H1: hay autocorrelacion

lmtest::dwtest(Modelo2)
# Si existe Independencia

summary(Modelo2)


#pregunta 3

vacio    <- lm(PM2.5 ~ 1, base) # modelo sin variables, solo con intercepto
completo <- lm(PM2.5 ~ ., base) # modelo con todas las variables
M2<- step(vacio,direction="forward",scope=formula(completo))
summary(M2) #Adjusted R-squared:  0.8196
summary(completo) #Adjusted R-squared:  0.8179 
print(0.8196 - 0.8179) * 100

anova(completo, M2)


# Paso 1: Definir la lista de modelos (fórmulas) generados durante el step
modelos <- list(
  lm(PM2.5 ~ NO2, data = base),
  lm(PM2.5 ~ NO2 + CO, data = base),
  lm(PM2.5 ~ NO2 + CO + Humed, data = base),
  lm(PM2.5 ~ NO2 + CO + Humed + O3, data = base),
  lm(PM2.5 ~ NO2 + CO + Humed + O3 + TMin, data = base),
  lm(PM2.5 ~ NO2 + CO + Humed + O3 + TMin + NO, data = base),
  lm(PM2.5 ~ NO2 + CO + Humed + O3 + TMin + NO + Viento, data = base)
)

# Paso 2: Calcular el R² ajustado para cada modelo
R2_ajustados <- sapply(modelos, function(mod) summary(mod)$adj.r.squared)

# Paso 3: Mostrar los R² ajustados
print(R2_ajustados)

anova(completo,M2)

#PREGUNTA 4:

ModeloEscogido <- lm(PM2.5 ~ TMin + NO2 + CO, data = base)
summary(ModeloEscogido)

# Supuestos

# linealidad
plot(ModeloEscogido, 1)
plot(ModeloEscogido$fitted, ModeloEscogido$residuals)
car::residualPlots(ModeloEscogido)

# Normalidad
# H0: residuos son normales
# H1: residuos no son normales
plot(ModeloEscogido, 2)
nortest::lillie.test(ModeloEscogido$residuals)

ks.test(ModeloEscogido$residuals, "pnorm", mean(ModeloEscogido$residuals), sd(ModeloEscogido$residuals))


# Homoceasticidad
# H0: Si existe Homoceasticidad
# H1: No existe Homoceasticidad

lmtest::bptest(ModeloEscogido)
# No existe Homoceasticidad

# Independencia
# H0: No hay autocorrelacion
# H1: hay autocorrelacion

lmtest::dwtest(ModeloEscogido)
# Si existe Independencia

#busqueda de la multicolinealidad
car::vif(ModeloEscogido)

#Analisis de correlacion
base_filtrada <- base[, c("PM2.5", "TMin", "NO2", "CO")]

# Matriz de dispersión
library(GGally)
pairs(base_filtrada,upper.panel= panel.smooth, lower.panel = panel.smooth)

library(GGally)
ggpairs(base_filtrada)

correlacion <- cor(base_filtrada)
print(correlacion)

# Matriz de correlacion
library(corrplot)
corrplot(correlacion, type="lower", method = "number")

#Cálculo de Correlaciones
# Calcular matriz de correlación
cor(base[, c("PM2.5", "TMin", "NO2", "CO")])

# Correlación entre PM2.5 y TMin
cor.test(base$PM2.5, base$TMin)
# Correlación entre PM2.5 y NO2
cor.test(base$PM2.5, base$NO2)
# Correlación entre PM2.5 y CO
cor.test(base$PM2.5, base$CO)
# Correlación entre TMin y NO2
cor.test(base$TMin, base$NO2)
# Correlación entre TMin y CO
cor.test(base$TMin, base$CO)
# Correlación entre NO2 y CO
cor.test(base$NO2, base$CO)


car::outlierTest(ModeloEscogido)
summary(influence.measures(ModeloEscogido))

# cook
plot(ModeloEscogido,4)

#Leverage
plot(ModeloEscogido,5)

