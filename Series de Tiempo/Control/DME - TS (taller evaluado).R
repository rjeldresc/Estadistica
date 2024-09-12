#########################################
## Taller Evaluado de Series de Tiempo ##
#########################################

Base <- rio::import("Demanda_Real.xlsx")
Y <- ts(Base$DEMANDA_REAL_GWH, start = c(2014,1), frequency = 12, end = c(2023,12))

plot(Y)

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
MASS::boxcox(Y ~ mod2$fitted) ## No es necesario transformar, ya que el uno esta en el IC.

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
