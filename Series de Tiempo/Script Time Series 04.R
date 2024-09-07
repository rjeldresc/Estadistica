######################
## DME - 2024       ##
## Series de Tiempo ##
## Script Sesión 4  ##
######################

## Modelo ARMA Estacional ##

## Ejemplo: Accidental Deaths in the US 1973–1978
## Description
## A time series giving the monthly totals of accidental 
## deaths in the USA. The values for the first six months 
## of 1979 are 7798 7406 8363 8460 9217 9316.

X <- USAccDeaths 
plot(X)

## Lag: retrazo de la serie de tiempo


## ¿Proceso MA(q)?
acf(c(X), ylim = c(-1,+1), lag.max = 12)
acf(c(X), ylim = c(-0.5,+0.5), lag.max = 12)
q <- 11

## ¿Proceso SMA(q)?
acf(c(X), ylim = c(-1,+1), lag.max = 72)
Q <- 4

## ¿Proceso AR(p)?
pacf(c(X), ylim = c(-1,+1), lag.max = 12, xlim = c(0,12))
acf(c(X), ylim = c(-0.5,+0.5), lag.max = 12)
p <- 11

## ¿Proceso SAR(p)?
pacf(c(X), ylim = c(-1,+1), lag.max = 72, xlim = c(0,72))
P <- 1

modelo <- forecast::auto.arima(y = X, max.p = p, max.q = q, max.P = P, max.Q = Q)
summary(modelo)
## El software me indica que d = 1 y D = 1, pero no quiero eso
modelo <- forecast::auto.arima(y = X, max.p = p, max.q = q, max.P = P, max.Q = Q, d = 0, D = 0)
summary(modelo)
plot(modelo) ## Se espera en este caso que las 13 raices invesas esten dentro del circulo.

pre <- forecast::forecast(modelo, h = 24, level = 0.95)
pre
plot(pre)

## Se cumplira la blancura?
tsdiag(modelo, 24)
tsdiag(modelo, 36)

LSTS::ts.diag(c(modelo$res), 24)
## Como cumple la blancura --> Valida la banda de predicción

## Seran los residuos Normales?
ks.test(scale(c(modelo$res)), "pnorm")$p.value
## No se rechaza la normalidad

## Seran los residuos homocedasticos?
lmtest::bptest(lm(modelo$res ~ c(time(modelo$res))))$p.value
## No se rechaza la normalidad

source("summary.arima.R")
summary_arima(fit = modelo, fixed = c(NA,NA,NA))

##################
## Modelo ARIMA ##
##################

Data <- rio::import("IPC_2024_07.xlsx")
head(Data)

Y <- ts(Data$IPC, start = c(2009,12), frequency = 12, end = c(2023,07))
IPC <- ts(Data$IPC, start = c(2009,12), frequency = 12)
par(bty = "n", las = 1)
plot(Y, col = "gray", lwd = 2)

## Esta serie presneta una tendencia estocastica 
## Que grado de integración presenta?
forecast::ndiffs(Y)
forecast::ndiffs(Y, test = "kpss")
forecast::ndiffs(Y, test = "adf")
forecast::ndiffs(Y, test = "pp")
## Observamos una discordancia entre los test
Y.1 <- diff(Y, lag = 1, differences = 1) ## (1-B)^1 Y[t]
Y.2 <- diff(Y, lag = 1, differences = 2) ## (1-B)^2 Y[t]
Y.3 <- diff(Y, lag = 1, differences = 3) ## (1-B)^3 Y[t]

## Que pasaría si no diferencio
Z <- forecast::auto.arima(Y, d = 0, D = 0)$res
LSTS::ts.diag(c(Z))
## Obtenemos errores correlacionados
par(mfrow = c(1,3), las = 1)
plot(Y.1, ylim = c(-0.5, 2))
plot(Y.2, ylim = c(-0.5, 2))
plot(Y.3, ylim = c(-0.5, 2))

sd(Y.2)/sd(Y.1)-1 ## Al hacer dos diferenciaciones se observa un incremento en la variabildiad de un 4%
sd(Y.3)/sd(Y.2)-1 ## Aca se observa un incremento del 73%

## Potencialmenet 1 y 2 diferenciones son razobles

par(mfrow = c(1,2))
acf(c(Y.2), lag.max = 60, ylim = c(-1,+1))
pacf(c(Y.2), lag.max = 60, xlim = c(0,60), ylim = c(-1,+1))

p <- 11
q <- 11
Q <- 2
P <- 1

modelo <- forecast::auto.arima(y = Y, d = 2, max.p = 11, max.q = 11, max.P = 1, max.Q = 2)
summary(modelo)
summary_arima(fit = modelo, fixed = c(NA,NA,NA))
## valor-p sma2 > 10% --> Se propone sacarlo del modelo

modelo <- forecast::Arima(y = Y, order = c(0,2,1), seasonal = c(0,0,1))
summary_arima(fit = modelo, fixed = c(NA,NA))
par(mfrow = c(1,1))
plot(modelo)
## Como todas las raices invesas MA estan dentro del circulo vamos a poder predecir

pre <- forecast::forecast(modelo, h = 12)
plot(pre)
## Se cumplio la blancura?
LSTS::ts.diag(c(modelo$residuals), 24)
## Se observa que en lag 6 y 13 hay una caida evidnete en los valores-p de Box-Ljung
## La incorporación de ma6 (o ar6) y ma13 (o ar13) permitirian
## que los valores-p estuvieran más cerca de uno.

modelo_1 <- forecast::Arima(y = Y, order = c(0,2,6), seasonal = c(0,0,1), fixed = c(NA,0,0,0,0,NA,NA))
summary_arima(fit = modelo_1, fixed = c(NA,0,0,0,0,NA,NA))              
LSTS::ts.diag(c(modelo_1$residuals), 24)

modelo_2 <- forecast::Arima(y = Y, order = c(0,2,13), seasonal = c(0,0,1), fixed = c(NA,0,0,0,0,NA,0,0,0,0,0,0,NA,NA))
summary_arima(fit = modelo_2, fixed = c(NA,0,0,0,0,NA,0,0,0,0,0,0,NA,NA))              
LSTS::ts.diag(c(modelo_2$residuals), 24)

modelo_3 <- forecast::Arima(y = Y, order = c(0,2,13), seasonal = c(0,0,1), fixed = c(NA,0,0,0,0,0,0,0,0,0,0,0,NA,NA))
summary_arima(fit = modelo_3, fixed = c(NA,0,0,0,0,0,0,0,0,0,0,0,NA,NA))              
LSTS::ts.diag(c(modelo_3$residuals), 24)
plot(modelo_3)

pre <- forecast::forecast(modelo, h = 12)
plot(pre)
plot(pre, xlim = c(2023,2025), ylim = c(90,110))
lines(IPC, col = "red")

pre <- forecast::forecast(modelo_1, h = 12)
plot(pre)
plot(pre, xlim = c(2023,2025), ylim = c(90,110))
lines(IPC, col = "red")

pre <- forecast::forecast(modelo_2, h = 12)
plot(pre)
plot(pre, xlim = c(2023,2025), ylim = c(90,110))
lines(IPC, col = "red")

pre <- forecast::forecast(modelo_3, h = 12)
plot(pre)
plot(pre, xlim = c(2023,2025), ylim = c(90,110))
lines(IPC, col = "red")

## Vamos a incorporar nueva incormación para ir mejorando la predicción
new.Y <- ts(Data$IPC, start = c(2009,12), frequency = 12, end = c(2023,08))
p <- modelo_3$arma[1]
q <- modelo_3$arma[2]
P <- modelo_3$arma[3]
Q <- modelo_3$arma[4]
s <- modelo_3$arma[5]
d <- modelo_3$arma[6]
D <- modelo_3$arma[7]
new_modelo <- forecast::Arima(y = new.Y, order = c(p,d,q), seasonal = c(P,D,Q), fixed = modelo_3$coef)
new_pre <- forecast::forecast(new_modelo, h = 11)
plot(new_pre, xlim = c(2023,2025), ylim = c(90,110))
lines(IPC, col = "red")

new.Y <- ts(Data$IPC, start = c(2009,12), frequency = 12, end = c(2023,09))
p <- modelo_3$arma[1]
q <- modelo_3$arma[2]
P <- modelo_3$arma[3]
Q <- modelo_3$arma[4]
s <- modelo_3$arma[5]
d <- modelo_3$arma[6]
D <- modelo_3$arma[7]
new_modelo <- forecast::Arima(y = new.Y, order = c(p,d,q), seasonal = c(P,D,Q), fixed = modelo_3$coef)
new_pre <- forecast::forecast(new_modelo, h = 10)
plot(new_pre, xlim = c(2023,2025), ylim = c(90,110))
lines(IPC, col = "red")

mean(abs(new_pre$mean/IPC-1))*100

cbind(new_pre$mean, IPC)



## Recalibración
new.Y <- ts(Data$IPC, start = c(2009,12), frequency = 12, end = c(2023,09))
p <- modelo_3$arma[1]
q <- modelo_3$arma[2]
P <- modelo_3$arma[3]
Q <- modelo_3$arma[4]
s <- modelo_3$arma[5]
d <- modelo_3$arma[6]
D <- modelo_3$arma[7]
new_modelo <- forecast::Arima(y = new.Y, order = c(p,d,q), seasonal = c(P,D,Q), fixed = c(NA,0,0,0,0,0,0,0,0,0,0,0,NA,NA))
new_pre <- forecast::forecast(new_modelo, h = 10)
plot(new_pre, xlim = c(2023,2025), ylim = c(90,110))
lines(IPC, col = "red")

mean(abs(new_pre$mean/IPC-1))*100

cbind(new_pre$mean, IPC)


new.Y <- ts(Data$IPC, start = c(2009,12), frequency = 12, end = c(2024,03))
p <- modelo_3$arma[1]
q <- modelo_3$arma[2]
P <- modelo_3$arma[3]
Q <- modelo_3$arma[4]
s <- modelo_3$arma[5]
d <- modelo_3$arma[6]
D <- modelo_3$arma[7]
new_modelo <- forecast::Arima(y = new.Y, order = c(p,d,q), seasonal = c(P,D,Q), fixed = c(NA,0,0,0,0,0,0,0,0,0,0,0,NA,NA))
new_pre <- forecast::forecast(new_modelo, h = 4)
plot(new_pre, xlim = c(2023,2025), ylim = c(90,110))
lines(IPC, col = "red")

mean(abs(new_pre$mean/IPC-1))*100

cbind(new_pre$mean, IPC)


########

new.Y <- ts(Data$IPC, start = c(2009,12), frequency = 12)
p <- modelo_3$arma[1]
q <- modelo_3$arma[2]
P <- modelo_3$arma[3]
Q <- modelo_3$arma[4]
s <- modelo_3$arma[5]
d <- modelo_3$arma[6]
D <- modelo_3$arma[7]
new_modelo <- forecast::Arima(y = new.Y, order = c(p,d,q), seasonal = c(P,D,Q), fixed = modelo_3$coef)
new_pre <- forecast::forecast(new_modelo, h = 12)
plot(new_pre, xlim = c(2023,2025), ylim = c(90,110))
lines(IPC, col = "red")


new.Y <- ts(Data$IPC, start = c(2009,12), frequency = 12)
p <- modelo_3$arma[1]
q <- modelo_3$arma[2]
P <- modelo_3$arma[3]
Q <- modelo_3$arma[4]
s <- modelo_3$arma[5]
d <- modelo_3$arma[6]
D <- modelo_3$arma[7]
new_modelo <- forecast::Arima(y = new.Y, order = c(p,d,q), seasonal = c(P,D,Q), fixed =c(NA,0,0,0,0,0,0,0,0,0,0,0,NA,NA))
new_pre <- forecast::forecast(new_modelo, h = 12)
plot(new_pre, xlim = c(2023,2025), ylim = c(90,110))
lines(IPC, col = "red")