######################
## DME - 2024       ##
## Series de Tiempo ##
## Script Sesión 3  ##
######################
setwd("d:/dev/Estadistica/Series de Tiempo/")
## Modelos de Series de Tiempo Estacionarios: ARMA y SARMA

## Modelo ARMA
## (1 - phi[1] B - ... - phi[p] B^p) (Y[t] - mu) =  (1 + phi[1] B + ... + phi[q] B^q) e[t]
## Supuestos: {e[t]} ~ RB(0, sigma^2)
##            raices invesas delta[i] de los polinomios cumplen con |delta[i]| < 1

## Ejemplo 1: Anillo de Crecimiento 
X <- LSTS::malleco
plot(X)
acf(X, lag.max = 10, ylim = c(-1,+1), lwd = 3) #suponer estacionario
## El comportamiento es muy similar al teorico de un AR(1) con 0 < phi < 1.

## ¿Cuál valor de phi sería adecuado? 
rho.1 <- acf(X, lag.max = 10, plot = F)$acf[2,,1]
phi <- rho.1

## En R la función ARMAacf calcula el ACF teórico de un ARMA
ACF <- ARMAacf(ar = c(phi), lag.max = 10)
lines(ACF ~ c(0:10), type = "p", pch = 20, lwd = 5, col = "red") #es un ajuste al acf

#ACF <- ARMAacf(ar = -c(phi), lag.max = 10)
#lines(ACF~c(0:10), type = "p", pch = 20, lwd = 5, col = "red")

mean(X)

## Hay varias funciones que estiman los coeficientes ARMA, por ejemplo,
## las siguientes estiman por Máxima Verosimilitud
arima(X, order = c(1,0,0))$coef
forecast::Arima(X, order = c(1,0,0))$coef  

modelo <- forecast::Arima(X, order = c(1,0,0))

## Grafico de raices invesas
plot(modelo) #se espera que los puntos esten adentro
## Se espera que en ambos casos las raices se encuentre dentro de los circulos unitarios

## Proceso MA(q) --> despues de lag = q el ACF = 0.
## En este ejemplo, una alternativa al AR(1) sería un MA(5)
acf(X, lag.max = 10, ylim = c(-1,+1), lwd = 3)
modelo.ma5 <- forecast::Arima(X, order = c(0,0,5))
ACF <- ARMAacf(ma = modelo.ma5$coef[1:5], lag.max = 10) #modelo.ma5$coef[1:5] obtiene los primeros 5 coeficientes
lines(ACF~c(0:10), type = "p", pch = 20, lwd =5, col = "blue") #es el ma , supone que del 6 en adelante es 0
ACF <- ARMAacf(ar = c(phi), lag.max = 10)
lines(ACF~c(0:10), type = "p", pch = 20, lwd =5, col = "red") #es el ar , decae más lento, cae exponencial

#######################
## Modelamiento ARMA ##
#######################

X <- LSTS::malleco
plot(X)
acf(X)  ## MA(19) se cuentan las barras
pacf(X) ## AR(20)

fit <- forecast::auto.arima(y = X, max.p = 20, max.q = 19, lambda = NULL)
fit #propuesta ar1
summary(fit)
plot(fit) #grafico raices inversas

source("TS.diag.R")
source("summary.arima.R")

summary_arima(fit, fixed = c(NA,NA)) #
TS.diag(c(fit$residuals), lag = 12) #lag hasta 12

pre <- forecast::forecast(fit, h = 30, level = 0.95) #prediccion caso arma da ultimos 30 años
pre
par(mfrow = c(1,1))
plot(pre)
plot(pre, xlim = c(1950, 2030))

modelo_0 <- forecast::Arima(X) #hace el promedio como el modelo 0
pre_0 <- forecast::forecast(modelo_0, h = 30)
plot(pre_0, xlim = c(1950, 2030)) #promedio y la banda
lines(pre$mean)
lines(pre$upper)
lines(pre$lower)
