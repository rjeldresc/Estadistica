######################
## DME - 2024       ##
## Series de Tiempo ##
## Script Sesión 2  ##
######################

###########################
## 1. Dependencia Serial ##
###########################

## Secuencia de variables aleatorias independientes
set.seed(20240826)
n <- 500
mu <- 1
X <- rnorm(n, mean = 0, sd = 0.15) + mu
X <- ts(X) #se le da atributo de series de tiempo
par(bty = "n", las = 1)
plot(X, ylab = "", col = "gray", lwd = 2, xlim =c(0,600))

modelo <- lm(X ~ time(X)) #regresion de datos vs tiempo
summary(modelo)  #valor p = 0.5066469
## No se rechaza supuesto de media constante
lmtest::bptest(modelo)$p.value
## No se rechaza supuesto de homocedasticidad

## Predicción ##
abline(h = mean(X), lty = 2, col = "red")

## ¿Se podrá mejorar la predicción? Si existe dependencia serial si podríamos

## Como se puede obtener una medida de auto dependnecia: acf(...)
acf(X, type = "covariance", lag.max = 10, lwd = 3)
axis(1, 0:10)

acf(X, type = "correlation", lag.max = 10, lwd = 3)
axis(1, 0:10)
?LSTS
## Ejemplo 2: Anchos de anillos de crecimiento
X <- LSTS::malleco
plot(X, xlim = c(1200, 2100))
axis(1, seq(1200,2100,100))

modelo <- lm(X ~ time(X))
summary(modelo) 
## No se rechaza supuesto de media constante
lmtest::bptest(modelo)$p.value
## No se rechaza supuesto de homocedasticidad
## Predicción ##
abline(h = mean(X), lty = 2, col = "red", lwd = 3)

acf(X, type = "correlation", lag.max = 10, lwd = 3) #para ver dependencia
axis(1, 0:10)

############################################
## 2. Tecnicas para lograr estacionaridad ##
############################################

## Estabilización de la Varianza ##
X <- LSTS::malleco
plot(X)
modelo <- lm(X ~ time(X))
lmtest::bptest(modelo)$p.value ## Se sugiere no hacer transformación
?MASS::boxcox
aux <- MASS::boxcox(X ~ time(X), lambda = seq(0.35,1.5,0.0001))
lambda <- aux$x[aux$y == max(aux$y)] #0.8607
axis(1, lambda)
## La presencia del 1 en el IC, sugiere no hacer transformación, confirmado 
## lo que indicaba bptest

Y <- (X^lambda-1)/lambda
plot(Y)
modelo <- lm(Y ~ time(Y))
lmtest::bptest(modelo)$p.value #0.05359631

## Como alternativa tenemos la función BoxCox.lambda(...) de "forecast"
forecast::BoxCox.lambda(X) #valor de la potencia 0.1017395
lambda <- forecast::BoxCox.lambda(X, method = "guerrero") #es ciego a la tendencia
lambda #0.1017395
lambda <- forecast::BoxCox.lambda(X, method = "loglik") #es sensible a la tendencia
lambda #0.85 segundo metodo
Y <- forecast::BoxCox(X, lambda = lambda)
plot(Y)
modelo <- lm(Y ~ time(Y))
lmtest::bptest(modelo)$p.value
## Dado que ahora tenemos un menor valor-p, mejor no transformar.

## Eliminación de Tendencias: Regresión vs Diferenciación

## Ej: Tipo de Cambio
Data <- rio::import("USDCLP=X.csv")
head(Data)
summary(Data)
tail(Data)

X <- ts(Data$`Adj Close`, start = c(2003,12), frequency = 12)
plot(X)

#para ver el diferencial
## Mod 1: Regresión
n <- length(X)
t <- 1:n
fit <- lm(X ~ t)
lines(fit$fitted ~ c(time(X)), col = "red", lwd = 3)
fit <- lm(X ~ I(t) + I(t^2) + I(t^3) ) #ajustar un polinomio
lines(fit$fitted ~ c(time(X)), col = "orange", lwd = 2)
library(splines)
fit <- lm(X ~ bs(t, degree = 5)) #regresion Spline
lines(fit$fitted ~ c(time(X)), col = "blue", lwd = 2, lty = 2)
?bs
#regresion genera errores
Z <- ts(fit$residuals, start = start(X), frequency = frequency(X))
plot(Z)
lines(window(Z, start = c(2024,1)), type = "p", pch = 20, lwd = 3)

abline(h = mean(Z), lty = 2)
acf(Z, lag.max = 10, lwd = 2)
acf(Z, lag.max = 100, lwd = 2)

## Mod 2: Diferenciación
d <- 1 ## Numero de diferenciaciones
k <- 1 ## Lag
W <- diff(X, lag = k, differences = 1)
plot(W)
abline(h = mean(W), lty = 2)
sd(W)
W <- diff(X, lag = k, differences = 2)
plot(W)
sd(W)
abline(h = mean(W), lty = 2)
W <- diff(X, lag = k, differences = 3)
plot(W)
abline(h = mean(W), lty = 2)
sd(W)
## Sobre diferenciar tienen un costo --> aumento en la variabilidad

## Number of differences required for a stationary series
forecast::ndiffs(X, test = "kpss") #1
forecast::ndiffs(X, test = "adf") #1
forecast::ndiffs(X, test = "pp") #1

W <- diff(X, lag = k, differences = 1)
acf(W, lag.max = 10)

###########################
## Prueba de la Blancura ##
###########################

acf(W, lag.max = 10)
Box.test(x = W, lag = 1, type = "Ljung-Box")
## Se rechaza blancura p-value = 0.003928
LSTS::ts.diag(c(W))

## Datos Simulados NO correlacionados
set.seed(2608)
Z <- rnorm(100)
acf(Z)
LSTS::ts.diag(c(Z))
Z <- rnorm(100)
acf(Z)
LSTS::ts.diag(c(Z))
