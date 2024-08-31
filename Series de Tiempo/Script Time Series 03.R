######################
## DME - 2024       ##
## Series de Tiempo ##
## Script Sesión 3  ##
######################

## Modelos de Series de Tiempo Estacionarios: ARMA y SARMA

## Modelo ARMA
## (1 - phi[1] B - ... - phi[p] B^p) (Y[t] - mu) =  (1 + phi[1] B + ... + phi[q] B^q) e[t]
## Supuestos: {e[t]} ~ RB(0, sigma^2)
##            raices invesas delta[i] de los polinomios cumplen con |delta[i]| < 1

## Ejemplo 1: Anillo de Crecimiento 
X <- LSTS::malleco
plot(X)
acf(X, lag.max = 10, ylim = c(-1,+1), lwd = 3)
## El comportamiento es muy similar al teorico de unm AR(1) con 0 < phi < 1.

## ¿Cuál valor de phi sería adecuado? 
rho.1 <- acf(X, lag.max = 10, plot = F)$acf[2,,1]
phi <- rho.1

## En R la función ARMAacf calcula el ACF teórico de un ARMA
ACF <- ARMAacf(ar = c(phi), lag.max = 10)
lines(ACF~c(0:10), type = "p", pch = 20, lwd =5, col = "red")

mean(X)

## Hay varias funciones que estiman los coeficientes ARMA, por ejemplo,
## las siguientes estiman por Máxima Verosimilitud
arima(X, order = c(1,0,0))$coef
forecast::Arima(X, order = c(1,0,0))$coef  

modelo <- forecast::Arima(X, order = c(1,0,0))

## Grafico de raices invesas
plot(modelo)
## Se espera que en ambos casos las raices se encuentre dentro de los circulos unitarios

## Proceo MA(q) --> despues de lag = q el ACF = 0.
## En este ejemplo, una alternativa al AR(1) sería un MA(5)
acf(X, lag.max = 10, ylim = c(-1,+1), lwd = 3)
modelo.ma5 <- forecast::Arima(X, order = c(0,0,5))
ACF <- ARMAacf(ma = modelo.ma5$coef[1:5], lag.max = 10)
lines(ACF~c(0:10), type = "p", pch = 20, lwd =5, col = "blue")
ACF <- ARMAacf(ar = c(phi), lag.max = 10)
lines(ACF~c(0:10), type = "p", pch = 20, lwd =5, col = "red")

#######################
## Modelamiento ARMA ##
#######################

X <- LSTS::malleco
plot(X)
acf(X)  ## MA(19)
pacf(X) ## AR(20)

fit <- forecast::auto.arima(y = X, max.p = 20, max.q = 19, lambda = NULL)
fit
summary(fit)
plot(fit)

source("TS.diag.R")
source("summary.arima.R")

summary_arima(fit, fixed = c(NA,NA))
TS.diag(c(fit$residuals), lag = 12)

pre <- forecast::forecast(fit, h = 30, level = 0.95)
pre
par(mfrow = c(1,1))
plot(pre)
plot(pre, xlim = c(1950, 2030))

modelo_0 <- forecast::Arima(X)
pre_0 <- forecast::forecast(modelo_0, h = 30)
plot(pre_0, xlim = c(1950, 2030))
lines(pre$mean)
lines(pre$upper)
lines(pre$lower)
