#library(dplyr)
#library(car)
#setwd("D:/dev/Estadistica/Bases de datos/")
#base <- readxl::read_excel("Contam.xlsx")
#head(base)
#nrow(base)

library(DBI)
#library(odbc)
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

# Supuestos

# linealidad
plot(Modelo3, 1)
plot(Modelo3$fitted, Modelo3$residuals)

# pasa
plot(Modelo3, 2)
# Normalidad
# H0: residuos son normales
# H1: residuos no son normales

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

#Pregunta 2

par(mfrow = c(2,2))
plot(PM2.5 ~ NO, data = base, type = "p", pch = 20, bty = "n", las = 1)
plot(PM2.5 ~ NO2, data = base, type = "p", pch = 20, bty = "n", las = 1)
plot(PM2.5 ~ CO, data = base, type = "p", pch = 20, bty = "n", las = 1)
plot(PM2.5 ~ O3, data = base, type = "p", pch = 20, bty = "n", las = 1)
dev.off()

Modelo1 <- lm(PM2.5 ~ NO, data = base)
summary(Modelo1)$r.squared
Modelo2 <- lm(PM2.5 ~ NO2, data = base)
summary(Modelo2)$r.squared
Modelo3 <- lm(PM2.5 ~ CO, data = base)
summary(Modelo3)$r.squared
Modelo4 <- lm(PM2.5 ~ O3, data = base)
summary(Modelo4)$r.squared


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
