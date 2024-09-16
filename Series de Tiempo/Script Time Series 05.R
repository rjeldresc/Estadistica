######################
## DME - 2024       ##
## Series de Tiempo ##
## Script Sesión 5  ##
######################

## Modelo SARIMA  ##

## Ejemplo: 
## Mauna Loa Atmospheric CO2 Concentration
## Description
## Atmospheric concentrations of CO2 are expressed in parts per 
## million (ppm) and reported in the preliminary 1997 SIO manometric 
## mole fraction scale.

## Importar Data Frame
Data <- rio::import("co2.xlsx")

## Asignar atributo de time serie al vector CO2
Y <- ts(Data$CO2, start = c(1959,1), frequency = 12)
lambda <- round(forecast::BoxCox.lambda(x = Y, method = "guerrero", lower = 0, upper = 1), 1) #0.9
## Preferimos no aplicar transformación

## Largo del vector
n <- length(Y)

## Grafico de la serie
plot(Y)

## Modelo SARIMA:
## supone que la tendencia y estacionalidad se generan
## a partir de incrementos estacionarios 
## Para obtener la serie estacionaria, es necesario 
## diferencias regular (d veces) y estacionalmente (D veces)

## ¿Que valores de "d" y "D" son recomendables?

## d: número de diferenciaciones regulares (elimina tendencia) --> ndiffs(...)
d <- forecast::ndiffs(Y, test = "kpss")
Y.1 <- diff(Y, lag = 1, difference = d)
plot(Y.1)
## si el vector presenta NA's, el acf requiere agregar el argumento na.action = na.pass 
acf(Y.1, lag.max = 200, na.action = na.pass)
## Se observa una fuerte dependencia estacional

## D: número de diferenciaciones estacionales (elimina estacionalidad) --> nsdiffs(...)
s <- frequency(Y.1)
D <- forecast::nsdiffs(Y.1)
Y.1.12 <- diff(Y.1, lag = s, difference = D)
plot(Y.1.12)
acf(Y.1.12, lag.max = 200, na.action = na.pass)
acf(c(Y.1.12), lag.max = 60, na.action = na.pass)
## Obtengamos los valores p, q, P y Q para ajustar ARMA y/o SARMA

## Con ACF obtenemos q y Q
acf(c(Y.1.12), lag.max = 11, na.action = na.pass) #hace un zoom a los primeros 11 lag
q <- 11
acf(c(Y.1.12), lag.max = 84, na.action = na.pass)
Q <- 1

## Con PACF obtenemos p y P
pacf(c(Y.1.12), lag.max = 11, na.action = na.pass, xlim = c(0,11))
p <- 11
pacf(c(Y.1.12), lag.max = 84, na.action = na.pass)
P <- 7

## Modelo preliminar basado en auto.arima
Y.total <- Y
Y <- window(Y.total, start = c(1959,1), end = c(2020,12)) #se elije un segmento
mod.01 <- forecast::auto.arima(y = Y, d = d, D = D, max.p = p, max.q = q, max.P = P, max.Q = Q)

## Raices inversas
plot(mod.01)
## No se observan raices fuera de los circulos unitarios

summary(mod.01)
## MAPE = 0.0715%

source("summary.arima.R")
summary_arima(fit = mod.01, fixed = c(NA,NA,NA,NA))

## Pasa la prueba de la blancura?
LSTS::ts.diag(c(mod.01$residuals))
tsdiag(mod.01)
source("TS.diag.R")
TS.diag(c(mod.01$residuals))

## Normalidad
Z <- mod.01$residuals
ks.test(scale(na.omit(c(Z))), "pnorm")$p.value

## Homocedacidad
lmtest::bptest(lm(Z ~ time(Z)))$p.value
## Potencialmente podríamos evaluar un transformación de 
## Box-Cox

## Predicción
par(mfrow = c(1,1))
pre <- forecast::forecast(mod.01, h = 44)
plot(pre, xlim = c(2018, 2025), ylim = c(400, 430))
lines(Y.total, lwd = 2, col = "red")

mean(abs(pre$mean/Y.total-1))*100 #porcentaje de lo que se equivocó 

######################
## Imacec No Minero ##
######################

## SARIMA vs SARIMAX ##

Data <- rio::import("Imacec_2024_07.xlsx")
head(Data)

Y <- ts(Data$IMACEC_NM, start = c(1996,1), frequency = 12, end = c(2023,12))
plot(Y)

## Transformamos?
lambda <- round(forecast::BoxCox.lambda(x = Y, method = "guerrero", lower = 0, upper = 1), 1)
f.Y <- forecast::BoxCox(x = Y, lambda = lambda)
plot(f.Y)

## Modelo SARIMA
d <- forecast::ndiffs(f.Y)
D <- forecast::nsdiffs(diff(f.Y))

Y.1.12 <- diff(diff(f.Y), lag = 12)
## Valores de p, q, P, y Q
acf(c(Y.1.12), xlim = c(0,11), lag.max = 11)
q <- 6
acf(c(Y.1.12), xlim = c(0,120), lag.max = 120)
Q <- 2

pacf(c(Y.1.12), xlim = c(0,11), lag.max = 11)
p <- 11
pacf(c(Y.1.12), xlim = c(0,120), lag.max = 120)
P <- 4

## Modelo auto.arima(...)
mod.02 <- forecast::auto.arima(y = Y, d = d, D = D, max.p = p, max.q = q, max.P = P, max.Q = Q, lambda = lambda)
plot(mod.02)
source("summary.arima.R")
summary_arima(fit = mod.02, fixed = c(NA,NA,NA))
source("TS.diag.R")
TS.diag(c(mod.02$res))

## Box-Ljung sugiere incorporar estructura en ma4 y ma6
fixed <- c(NA,NA,0,0,NA,0,NA,NA)
mod.03 <- forecast::Arima(y = Y, order = c(1,1,6), seasonal = c(0,1,1), lambda = lambda, fixed = fixed)
summary_arima(fit = mod.03, fixed = fixed)

## Eliminemos el ar1
fixed <- c(0,NA,0,0,NA,0,NA,NA)
mod.04 <- forecast::Arima(y = Y, order = c(1,1,6), seasonal = c(0,1,1), lambda = lambda, fixed = fixed)
summary_arima(fit = mod.04, fixed = fixed)

## Eliminemos el ma1
fixed <- c(0,0,0,0,NA,0,NA,NA)
mod.05 <- forecast::Arima(y = Y, order = c(1,1,6), seasonal = c(0,1,1), lambda = lambda, fixed = fixed)
summary_arima(fit = mod.05, fixed = fixed)

par(mfrow = c(1,1))
plot(mod.05)

TS.diag(c(mod.05$residuals))

## Box-Ljung sugiere incorporar estructura en ma9 y ma13
fixed <- c(0,0,0,0,NA,0,NA,0,0,NA,0,0,0,NA,NA)
mod.06 <- forecast::Arima(y = Y, order = c(1,1,13), seasonal = c(0,1,1), lambda = lambda, fixed = fixed)
summary_arima(fit = mod.06, fixed = fixed)

par(mfrow = c(1,1))
plot(mod.06)

TS.diag(c(mod.06$residuals))

## Predicción ##
pred <- forecast::forecast(mod.06, h = 7, level = 0.95)
par(mfrow = c(1,1))
plot(pred, xlim = c(2020,2025), ylim = c(90,130))
Y. <- ts(Data$IMACEC_NM, start = c(1996,1), frequency = 12)
lines(Y., col = "red")

## Variacion anual real para 2024-07
100*(109.28930/104.66235-1)

## Variacion anual pronosticada para 2024-07
100*(103.8079/104.66235-1)

## MAPE predicción
100*mean(abs(pred$mean/Y.-1))

####################
## Modelo SARIMAX ##
####################

## Requiere variables regresoras
## Un muy buen regresor es el PIB y para las proyecciones
## El Pib Tencial No Minero (ministerio de hacieda)




PIB <- rio::import("PIB_TENDENCIAL.xlsx")
head(PIB)
PIB$Tiempo <- PIB$YEAR + (PIB$MONTH-1)/12

plot(PIB_NM ~ Tiempo, data = PIB)
?smooth.spline
## Mensualizar PIB Tendencial
fit.smooth <- smooth.spline(PIB$PIB_NM ~ PIB$Tiempo, spar = 0)
t <- seq(2003,2030,1/12)
lines(predict(fit.smooth, t)$y~t)

xreg <- as.matrix(data.frame(pib = predict(fit.smooth, time(Y))$y/1000 ))

cor(Y,xreg)
plot(Y,xreg)

mod.01 <- forecast::Arima(y = Y, xreg = xreg) #xreg matriz de diseño

plot(Y)
lines(mod.01$fitted, col = "red")

mod.02 <- forecast::auto.arima(y = Y, xreg = xreg, lambda = lambda, d = 0)
summary(mod.02)
TS.diag(c(mod.02$residuals))
## Box-Ljung pide estructura en ma4, ma6

fixed = c(0,0,0,0,0,NA,0,NA,NA,NA)
mod.03 <- forecast::Arima(y = Y, order = c(2,1,6), seasonal = c(0,1,1), xreg = xreg, lambda = lambda, fixed = fixed)
summary_arima(mod.03, fixed = fixed)
TS.diag(c(mod.03$residuals))

## Predicción ##
t <- seq(2024,2026-1/12,1/12)
newxreg <- as.matrix(data.frame(pib = predict(fit.smooth, t)$y/1000 ))
pred <- forecast::forecast(mod.03, xreg = newxreg, level = 0.95)
par(mfrow = c(1,1))
plot(pred, xlim = c(2020,2025), ylim = c(90,130))
Y. <- ts(Data$IMACEC_NM, start = c(1996,1), frequency = 12)
lines(Y., col = "red")

## MAPE predicción
100*mean(abs(pred$mean/Y.-1))
