###################################
## Script Clase Series de Tiempo ##
## 21/08/2024                    ##
###################################

## Consultar Carpeta de Trabajo ##
getwd()

## Consultar archivos disponible en la carpeta ##
dir()

## Importar datos en formato csv ##
Base <- rio::import("TSLA.csv")

## Vista 1ra filas
head(Base, 10)

## Vista últimas filas
tail(Base, 20)

## Consultar objetos disponibles 
ls()

## Resumne de la base de datos
summary(Base)

## Precio de Cierre (Adj)
X <- Base$`Adj Close`

## Grafico de Serie de Tiempo
plot(`Adj Close` ~ Date, data = Base)
par(mfrow = c(1,1), bty = "n", las = 1)
plot(`Adj Close` ~ Date, data = Base, type = "l", lwd = 2, col = "gray")

par(mfrow = c(1,1), bty = "n", las = 1)
plot(log(`Adj Close`) ~ Date, data = Base, type = "l", lwd = 2, col = "gray")


## Construir Rentabilidad Diaria
n <- length(X)
Y <- (X[2:n]/X[1:(n-1)]-1)*100
Z <- diff(log(X))*100
cbind(Y,Z)

plot(Y)
ts.plot(Y)
lines(Z, col = "red")

plot(Y ~ Base$Date[2:n], type = "l", col = "gray", lwd = 2)

## Histograma ##
hist(Y, freq = F, main = "", border = "white", breaks = seq(-25,+25,1))

moments::kurtosis(Y)

aux <- fitdistrplus::fitdist(data = Y, distr = "norm", method = "mle")$estimate
curve(dnorm(x, mean = aux[1], sd = aux[2]), from = -25, to = +25, n = 1000, add = T, col = "red", lwd = 2)

aux <- fitdistrplus::fitdist(data = Y, distr = "logis", method = "mle")$estimate
curve(dlogis(x, location = aux[1], scale = aux[2]), from = -25, to = +25, n = 1000, add = T, col = "blue", lwd = 2)

lines(density(Y), lwd = 2)

## Función Acumulada
Fn <- ecdf(Y)
y <- sort(Y)
plot(Fn(y) ~ y, type="s")
curve(plogis(x, location = aux[1], scale = aux[2]), from = -25, to = +25, n = 1000, add = T, col = "blue", lwd = 2)

ks.test(Y, "plogis", location = aux[1], scale = aux[2])$p.value

