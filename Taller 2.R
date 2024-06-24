
# Ejercicio 1
# Ejercicio 1.a

# X = Cuenta # de accidentes de transito (exitos) en 24 horas (t). (en fiestas patrias)
# Ante poisson E(X) = lambda = 143
# X ~ Poisson(lambda = 143) 
# X : {0, 1, 2, 3, 4, ...}

# En un próximo 18/9 entre las 7:00 y 9:00 hrs ocurran por lo menos 5 accidentes
# de tránsito en el país

# Y = Cuenta # de accidentes de transito (exitos) en 2 horas (t). (en fiestas patrias)
# Y ~ Poisson(lambda = ?) 
# Y : {0, 1, 2, 3, 4, 5, ...}

# regla de 3
# lambda 143 <-> 24 horas
# lambda ?   <-> 1 hora
# lambda ?   <-> 2 horas

lambda_por_hora <- 143/24 # t: 1 hora
lambda_por_hora
lambda_por_2_horas <- lambda_por_hora*2 # t: 2 horas
lambda_por_2_horas
# Y ~ Poisson(lambda = 11.91667) 
# P(Y >= 5) = 1 - P(Y <= 4)
1 - ppois(4, lambda = lambda_por_2_horas) #es por el contexto de la pregunta
# R: 0.9919447

# Ejercicio 1.b

# Z: tiempo entre dos accidentes en fiestas patrias (unidad = minutos)
# Z ~ Exp(lambda_por_min)

lambda_por_minuto <- lambda_por_hora/60

# Me piden P(Z < 30)
pexp(30, lambda_por_minuto)
# R: 0.9491648

# Ejercicio 2
# Ejercicio 2.a

# X: # de encuestas que no llegan a un destinatario en 8 intentos
# X: {0, 1, 2, 3, 4, ..., 8}
# X ~ Binomial(n = 8, p = 0.05)

n = 8; p = 0.05

# P(X <= 2)
pbinom(2, n, p)
#R: 0.9942118

# Ejercicio 2.b
# Y: # de envios/encuestas hasta recibir el tercer "informe de no recepcion"
# (evento exito: no recibir una encuesta)
# Y: 3, 4, 5, 6, 7, 8, ...
# Y ~ BinNeg(r = 3, p = 0.05)

r = 3; p = 0.05

# E(Y) = r/p
r/p
#R: 60 encuestas de deben enviar para esperar el tercer informe de no recepcion

# Ejercicio 3

# A = Concentracion de plomo en muestras de plaguicida de tipo A
# A ~ Normal(mu = 1.48, var = 0.28^2)

mu = 1.48; sd = 0.28 # var = 0.28^2 o var = 0.28**2

# Ejercicio 3.a
# P(1 <= A <= 1.3) = P(A <= 1.3) - P(A <= 1)

p <- pnorm(1.3, mean = mu, sd = sd) - pnorm(1, mean = mu, sd = sd)
p
#R: 0.2169203

# Ejercicio 3.b

# Y: # de muestras de plaguicida tipo A con concentracion de plomo mayor a 1.3 u.
# evento exito: muestra posea una concentracion de plomo mayor a 1.3 u. (plaguicida tipo A)
# Y: {0, 1, 2, 3, ..., 7}
# Y ~ Binomial(n = 7, p = ?)

n = 7
# p = P(A > 1.3) = 1 - P(A <= 1.3)
p = 1 - pnorm(1.3, mu, sd); p
#R: 0.7398416
# Y ~ Binomial(n = 7, p = 0.7398416)

# Piden P(Y >= 2) = 1 - P(Y < 2) = 1- P(Y <= 1)
1 - pbinom(1, n, p)
#R: 0.9983136

# Ejercicio 3.c

# P(A <= a1) = 0.2 y P(A <= a2) = 0.85

qnorm(0.2, mu, sd)
# R: 1.244346 , Grupo bajo 20%
qnorm(0.85, mu, sd)
# R: 1.770201 , Grupo riesgo

# Ejercicio 4

# MA ~ Normal(38, 9)
# MB ~ Normal(50, 16)

# Ejercicio 4.a

# MT = (MA + MB) ~ Normal(38 + 50, 9 + 16)

mut = 38 + 50 #se suman los dos promedios
sdt =  sqrt(9 + 16) #se suman las dos varianzas, se pasa a desviacion estandar

# Ejercicio 4.b

# P(MT < 88.5)
pnorm(88.5, mut, sdt)
# R: 0.5398278

# Ejercicio 5 ####

# Ejericico 5.a #

# R: 0.2169203

# Ejercicio 5.b # Les he mentido :(, si la tienen que pensar (no es binomial, 
# habla de la concentracion "media")

#X_barra - N(1.48, 0.28 2)

mu = 1.48; sd = 0.09333

#P(X_barra > 2) = 1- P(X_barra <=2)
p <- 1- pnorm(2, 1.48, sd)
p
# R: 1.263295e-08 ~~ 0

#ejemplos teoricos y practicos sobre la normal

#ejemplos teoricos
#ejemplo de una N(0,1)
curve(dnorm(x, mean = 0, sd = 1 ), from = -1 , to = 1, col = "red", lwd = 2)
curve(dnorm(x, mean = 0, sd = 1 ), from = -10 , to = 10, col = "red", lwd =2)

#para limpiar los graficos
dev.off()

#ejemplo de normales
#Normal(0,1)
#Normal(4,1)

curve(dnorm(x, mean = 0, sd = 1 ), from = -3 , to = 7, 
      col = "red", lwd =2) #curva de la N(0,1)

curve(dnorm(x, mean = 4, sd = 1 ), add = T, 
      col = "blue", lwd =2)


dev.off()
curve(dnorm(x, mean = 0, sd = 1 ), from = -3 , to = 10, 
      col = "red", lwd =2) #curva de la N(0,1)
curve(dnorm(x, mean = 4, sd = 2 ), add = T, 
      col = "blue", lwd =2)

#ejemplo practico

set.seed(8888) #definir una semilla
#definiendo 1000 valores aleatorios
X1 <- rnorm(1000, 0,1)
# X2 = X1 * 3 + 2
#X2 - N( 3 * mu +2, 9*sigma cuadrado)
#X2 - N( 2, 9) mu = 2 , var = 9 , se aumento la varianza

#graficos
X2 <- X1 * 3 + 2
hist(X1, freq = F, col = "steelblue", border = "black", xlim = c(-12, 12))
hist(X2, freq = F, col = "coral", border = "black" , add = T, nclass = 30)

#ejemplo de suma/resta de normales
X3 <- rnorm(1000 , 3, 4)
X4 <- X1 - X3
#X4 N( 0 - 3 , 1 + 16) = N (-3 , 17)
dev.off()
hist(X1, freq = F, col = "steelblue", border = "blue", xlim = c(-12, 12))
hist(X2, freq = F, col = "coral", border = "red" , add = T, nclass = 30)
hist(X4, freq = F, col = "gold", border = "yellow" , add = T, nclass = 30)

install.packages("ggplot2")

dev.off()
library(ggplot2)
# Supongamos que X1, X2 y X4 son tus datos
# Crea un data frame combinando las tres variables con una columna indicando el origen
data <- data.frame(
  value = c(X1, X2, X4),
  variable = factor(rep(c("X1", "X2", "X4"), c(length(X1), length(X2), length(X4))))
)

# Crea el histograma con ggplot2
ggplot(data, aes(x = value, fill = variable)) +
  geom_histogram(aes(y = ..density..), position = "identity", alpha = 0.5, bins = 30) +
  scale_fill_manual(values = c("steelblue", "coral", "gold")) +
  xlim(-12, 12) +
  labs(title = "Histograma de X1, X2 y X4",
       x = "Valor",
       y = "Densidad") +
  theme_minimal()

#estimacion puntual
datos <- rnorm(10000, 580 , 60)

#estimacion
library(MASS)

normal <- fitdistr(datos, "normal")
print(normal)
mean_est <- normal$estimate[1]
sd_est <- normal$estimate[2]

weibull <- fitdistr(datos, "weibull")
print(weibull)
shape_est <- weibull$estimate[1]
scale_est <- weibull$estimate[2]

logNormal <- fitdistr(datos, "log-normal")
print(logNormal)
meanlog_est <- logNormal$estimate[1]
sdlog_est <- logNormal$estimate[2]

#graficos
dev.off()
hist(datos, freq = F , col ="steelblue" , border = "blue")
curve(dnorm(x, mean = mean_est , sd = sd_est), add = T , lwd = 2 , col = "red")
curve(dweibull(x, shape = shape_est , scale = scale_est), add = T , lwd = 2 , col = "yellow")
curve(dlnorm(x, meanlog =  meanlog_est , sdlog = sdlog_est), add = T , lwd = 2 , col = "green")
curve(dsn(x, xi = location, omega = scale, alpha = shape), add = TRUE, col = "purple", lwd = 2)


install.packages("ggplot2")
install.packages("dplyr")
library(ggplot2)
library(dplyr)
# Suponiendo que tus datos y estimaciones son los siguientes
# datos: vector de datos
# mean_est: media estimada para la normal
# sd_est: desviación estándar estimada para la normal
# shape_est: parámetro de forma estimado para la Weibull
# scale_est: parámetro de escala estimado para la Weibull
# meanlog_est: media logarítmica estimada para la log-normal
# sdlog_est: desviación estándar logarítmica estimada para la log-normal

# Crear un data frame con los datos
data <- data.frame(value = datos)

# Crear la base del gráfico con ggplot2
p <- ggplot(data, aes(x = value)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "steelblue", color = "blue", alpha = 0.6) +
  labs(title = "Distribuciones ajustadas a los datos",
       x = "Valor",
       y = "Densidad") +
  theme_minimal()

# Añadir la curva normal
p <- p + stat_function(fun = dnorm, args = list(mean = mean_est, sd = sd_est), color = "red", size = 1)

# Añadir la curva Weibull
p <- p + stat_function(fun = dweibull, args = list(shape = shape_est, scale = scale_est), color = "yellow", size = 1)

# Añadir la curva log-normal
p <- p + stat_function(fun = dlnorm, args = list(meanlog = meanlog_est, sdlog = sdlog_est), color = "green", size = 1)

# Mostrar el gráfico
print(p)

#test de bondad de ajuste
#H0: DATOS - "Distribucion"
ks.test(datos , "pnorm" , mean = mean_est , sd = sd_est)
ks.test(datos , "pweibull" , shape = shape_est, scale = scale_est)
ks.test(datos , "plnorm" , meanlog = meanlog_est, sdlog = sdlog_est)

#propagate

library(propagate)

Ajuste <- fitDistr(datos)
print(Ajuste)
print(Ajuste$bestfit)

print(Ajuste$stat)

install.packages("sn")
library(sn)

fit <- selm(datos ~ 1, family = "SN")
# Obtener los parámetros ajustados
params <- coef(fit)

# Parámetros de la distribución Skewed-normal
location <- params[1]  # Location (mu)
scale <- params[2]     # Scale (sigma)
shape <- params[3]     # Shape (alpha)

# Imprimir los parámetros
cat("Location (mu):", location, "\n")
cat("Scale (sigma):", scale, "\n")
cat("Shape (alpha):", shape, "\n")

# Instalar y cargar ggplot2 si aún no lo has hecho
install.packages("ggplot2")
library(ggplot2)

# Crear un data frame con los datos
data <- data.frame(value = datos)

# Crear la base del gráfico con ggplot2
p <- ggplot(data, aes(x = value)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "steelblue", color = "blue", alpha = 0.6) +
  labs(title = "Ajuste de la distribución Skewed-normal",
       x = "Valor",
       y = "Densidad") +
  theme_minimal()

# Añadir la curva Skewed-normal
p <- p + stat_function(fun = dsn, args = list(xi = location, omega = scale, alpha = shape), color = "purple", size = 1)

# Mostrar el gráfico
print(p)
