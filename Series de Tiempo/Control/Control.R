######################################
## Script Control Series de Tiempo  ##
## 09/09/2024                       ##
## Alumno: Rodrigo Jeldres Carrasco ##
######################################

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
Y <- ts(Base$DEMANDA_REAL_GWH, start = c(2014, 1), end = c(2023,12), frequency = 12)

# Ver los primeros valores para asegurarte de que esté bien construida
head(Y)

# Graficar la serie temporal
plot(Y, main = "Demanda Real de Energía (GWh)", xlab = "Tiempo", ylab = "Demanda GWh")

## Modelo 0: Y[t] = mu + Z[t]
mod0 <- forecast::Arima(Y)
plot(Y)
lines(mod0$fitted, col = "red")

## Modelo 1: Y[t] = mu + dummy + Z[t]
xreg <- as.matrix(data.frame(dummy = rep(0, length(Y))))
xreg[1:24,1] <- 1
mod1 <- forecast::Arima(Y, xreg = xreg)
plot(Y)
lines(mod1$fitted, col = "red")

## Modelo 2: Y[t] = b0 + b1 * t + dummy + Z[t]
xreg <- as.matrix(data.frame(tiempo = 1:length(Y), dummy = rep(0, length(Y))))
xreg[1:24,2] <- 1
mod2 <- forecast::Arima(Y, xreg = xreg)
plot(Y)
lines(mod2$fitted, col = "red")
MASS::boxcox(Y ~ mod2$fitted)

## Modelo 3: f(Y[t]) = b0 + b1 * t + dummy + Z[t]
mod3 <- forecast::Arima(Y, xreg = xreg, lambda = 0)      ## Realiza tranformación logaritmica
mod3 <- forecast::Arima(Y, xreg = xreg, lambda = 0.5)    ## Realiza tranformación con lambda = 0.5
mod3 <- forecast::Arima(Y, xreg = xreg, lambda = "auto") ## Realiza tranformación con lambda automático
mod3 <- forecast::Arima(Y, xreg = xreg, lambda = NULL)   ## No realiza tranformación
plot(Y)
lines(mod3$fitted, col = "red")

## Modelo 4: Y[t] = b0 + b1 * t + dummy + Z[t], con Z[t] ~ SARIMA(2,0,2)(1,1,1)[12]
mod4 <- forecast::Arima(Y, xreg = xreg, lambda = NULL, order = c(2,0,2), seasonal = c(1,1,1)) 
plot(Y)
lines(mod4$fitted, col = "red")

## ¿Seran los residuos homocedasticos?
# H0: los residuos son homocedásticos
# H1: los residuos no son homocedásticos
lmtest::bptest(lm(mod4$res ~ c(time(mod4$res))))$p.value # 0.09759236
## p=0.3009834 es mayor que alfa =  0.05 , no se rechaza la hipótesis nula
# se puede asumir que los residuos son homocedasticos 

#diferenciadores
forecast::ndiffs(Y, test = "kpss") #1
forecast::ndiffs(Y, test = "adf") #1
forecast::ndiffs(Y, test = "pp") #1

# Diferenciar la serie una vez
Y_differenced <- diff(Y, differences = 1)

# Graficar la serie diferenciada
plot(Y_differenced, 
     main = "Serie Temporal Diferenciada (Primer Orden)", 
     xlab = "Tiempo", ylab = "Demanda GWh")

# Verificar la estacionariedad de la serie diferenciada
forecast::ndiffs(Y_differenced, test = "kpss")  # Devuelve 0
forecast::ndiffs(Y_differenced, test = "adf")   # Devuelve 0
forecast::ndiffs(Y_differenced, test = "pp")    # Devuelve 0

# Modelar la serie diferenciada con un ARIMA
mod_diff <- forecast::Arima(Y, order = c(2, 1, 2), seasonal = c(1, 1, 1))  # d=1 por la diferenciación
plot(Y)
lines(mod_diff$fitted, col = "orange")

# Definir regresores (ejemplo con dummy y tiempo, puedes agregar PIB, IMACEC, etc.)
xreg <- as.matrix(data.frame(tiempo = 1:length(Y), dummy = rep(0, length(Y))))
xreg[1:24,2] <- 1  # Dummy para primeros 24 meses como ejemplo

# Modelo SARIMAX con componentes SARIMA (p,d,q) y regresores
mod_sarimax <- forecast::auto.arima(Y, xreg = xreg, seasonal = TRUE, lambda = "auto")

# Graficar ajuste
plot(Y, main = "Demanda Real de Energía (GWh) y Ajuste SARIMAX")
lines(mod_sarimax$fitted, col = "red")


# Test de homocedasticidad
lmtest::bptest(lm(mod_sarimax$residuals ~ time(mod_sarimax$residuals)))

source("TS.diag.R")
TS.diag(c(mod_sarimax$res))

new_pre <- forecast::forecast(mod_sarimax, h = 36, level = 0.95)




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

