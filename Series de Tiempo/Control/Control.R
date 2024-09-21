######################################
## Script Control Series de Tiempo  ##
## 09/09/2024                       ##
## Alumno: Rodrigo Jeldres Carrasco ##
######################################

library(forecast)
setwd("d:/dev/Estadistica/Series de Tiempo/Control")

Base <- rio::import("Demanda_Real.xlsx")
Y <- ts(Base$DEMANDA_REAL_GWH, start = c(2014,1), frequency = 12, end = c(2023,12))

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

# Definir regresores (ejemplo con dummy y tiempo, agregar PIB, IMACEC, etc.)
xreg <- as.matrix(data.frame(tiempo = 1:length(Y), dummy = rep(0, length(Y))))
xreg[1:24,2] <- 1  # Dummy para primeros 24 meses como ejemplo

# Modelo SARIMAX con componentes SARIMA (p,d,q) y regresores
mod_sarimax <- forecast::auto.arima(Y, xreg = xreg, seasonal = TRUE, lambda = "auto")
LSTS::ts.diag(c(mod_sarimax$residuals), 36)

# Graficar ajuste
plot(Y, main = "Demanda Real de Energía (GWh) y Ajuste SARIMAX")
lines(mod_sarimax$fitted, col = "red")

# Test de homocedasticidad
lmtest::bptest(lm(mod_sarimax$residuals ~ time(mod_sarimax$residuals)))
pacf(mod_sarimax$residuals,12)
source("TS.diag.R")
TS.diag(c(mod_sarimax$res))

#resumen del modelo
summary(mod_sarimax)

# Generar valores futuros de los regresores
# Suponiendo que el regresor 'tiempo' continúa linealmente y que el dummy sigue siendo 0 después del periodo inicial.
future_xreg <- as.matrix(data.frame(
  tiempo = (length(Y) + 1):(length(Y) + 36),
  dummy = rep(0, 36)  # Ajusta si es necesario
))

# Realizar el pronóstico con los nuevos valores de xreg
new_pre <- forecast::forecast(mod_sarimax, h = 36, level = 0.95, xreg = future_xreg)
LSTS::ts.diag(c(new_pre$residuals), 36)
summary(new_pre)
par(mfrow = c(1,1))
# Graficar el pronóstico
plot(new_pre)
#plot(new_pre, xlim = c(2024,2026), ylim = c(4000,9000))
Z <- ts(Base$DEMANDA_REAL_GWH, start = c(2014,1), frequency = 12)
lines(Z, col = "red")
cbind(new_pre$mean, Z)
mean(abs(new_pre$mean/Z-1))*100

# Extraer pronósticos anuales
pred_2024 <- sum(new_pre$mean[1:12])
pred_2025 <- sum(new_pre$mean[13:24])
pred_2026 <- sum(new_pre$mean[25:36])
pred_2024
pred_2025
pred_2026
# Calcular tasas de crecimiento anuales
tasa_2025 <- (pred_2025 - pred_2024) / pred_2024 * 100
tasa_2026 <- (pred_2026 - pred_2025) / pred_2025 * 100
tasa_2025
tasa_2026

###### Caso con regresor IMACEC

Imacec <- rio::import("Imacec_2024_07.xlsx")
Imacec_filtered <- Imacec[Imacec$YEAR >= 2014, ]
head(Imacec_filtered,5)

# Crear la serie de tiempo del regresor IMACEC
X <- ts(Imacec_filtered$IMACEC, start = c(2014, 1), frequency = 12 , end = c(2023,12))

# Ajustar el modelo SARIMAX usando la serie DEMANDA_REAL_GWH y IMACEC como regresor
mod_sarimax <- auto.arima(Y, xreg = X, seasonal = TRUE, lambda = "auto")

# Resumen del modelo ajustado
summary(mod_sarimax)
TS.diag(c(mod_sarimax$res))
# Graficar la serie original y el ajuste
plot(Y, main = "Demanda Real de Energía (GWh) y Ajuste SARIMAX con IMACEC")
lines(fitted(mod_sarimax), col = "red")

# Supongamos que el IMACEC futuro sigue igual a la media histórica de los últimos 12 meses
future_imacec <- rep(mean(Imacec_filtered$IMACEC), 36)

# Si tienes proyecciones del IMACEC, puedes usar esas en lugar de la media
# future_imacec <- c(valor_proyectado_1, valor_proyectado_2, ... , valor_proyectado_12)
# Test de homocedasticidad
lmtest::bptest(lm(mod_sarimax$residuals ~ time(mod_sarimax$residuals)))
# Generar la predicción para los próximos 36 meses
predicciones <- forecast(mod_sarimax, xreg = future_imacec, h = 36)

# Mostrar el resumen de la predicción
summary(predicciones)

# Graficar la predicción junto con la serie original
plot(predicciones, main = "Predicción de Demanda Real de Energía (GWh)")

#comparacion
Z <- ts(Base$DEMANDA_REAL_GWH, start = c(2014,1), frequency = 12)
lines(Z, col = "red")
cbind(predicciones$mean, Z)
mean(abs(predicciones$mean/Z-1))*100



