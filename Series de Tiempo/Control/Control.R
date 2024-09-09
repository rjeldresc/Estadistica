#####################################
## Script Control Series de Tiempo ##
## 09/09/2024                      ##
#####################################

## Consultar Carpeta de Trabajo ##
getwd()

setwd("C:/dev/Estadistica/Series de Tiempo/Control")

## Consultar archivos disponible en la carpeta ##
dir()

source("TS.diag.R")

## Importar datos en formato xlsx ##
Base <- rio::import("Demanda_Real.xlsx")

## Vista 1ra filas
head(Base, 10)

## Vista últimas filas
tail(Base, 20)


# Convertir la base de datos en una serie temporal mensual
Y <- ts(Base$DEMANDA_REAL_GWH, start = c(2014, 1), frequency = 12)

# Ver los primeros valores para asegurarte de que esté bien construida
head(Y)

# Graficar la serie temporal
plot(Y, main = "Demanda Real de Energía (GWh)", xlab = "Tiempo", ylab = "Demanda GWh")

PIB <- rio::import("PIB_TENDENCIAL.xlsx")

head(PIB)
PIB$Tiempo <- PIB$YEAR + (PIB$MONTH-1)/12

plot(PIB_NM ~ Tiempo, data = PIB)

## Mensualizar PIB Tendencial
fit.smooth <- smooth.spline(PIB$PIB_NM ~ PIB$Tiempo, spar = 0)
t <- seq(2014,2024,1/12)
lines(predict(fit.smooth, t)$y~t)

xreg <- as.matrix(data.frame(pib = predict(fit.smooth, time(Y))$y/1000))

cor(Y,xreg)
plot(Y,xreg)


mod.01 <- forecast::Arima(y = Y, xreg = xreg)

plot(Y)
lines(mod.01$fitted, col = "red")

lambda <- round(forecast::BoxCox.lambda(x = Y, method = "guerrero", lower = 0, upper = 1), 1)
f.Y <- forecast::BoxCox(x = Y, lambda = lambda)
plot(f.Y)

mod.02 <- forecast::auto.arima(y = Y, xreg = xreg, lambda = lambda, d = 0)
summary(mod.02)
TS.diag(c(mod.02$residuals))

## Predicción ##
pred <- forecast::forecast(mod.02, h = 7, level = 0.95)
par(mfrow = c(1,1))
plot(pred, xlim = c(2020,2025), ylim = c(90,130))
Y. <- ts(PIB$IMACEC_NM, start = c(2014,1), frequency = 12)
lines(Y., col = "red")

