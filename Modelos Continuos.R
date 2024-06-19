#getwd()
#setwd(dir = "d:/dev/Estadistica/")
#git_history()


# Modelo uniforme
# se denota X - Unif(a,b)
x <- seq(from = -0.5, to = 2.5 , by = 0.001)
plot(x, dunif(x = x, min = 0 , max =2), 
     type = "l" , 
     ylab = "F(x)", 
     lwd = 2, 
     col = "red")

# Modelo Log-Normal
x <- seq(from = 0.0001, to = 100 , by = 0.0001)
plot(x, dlnorm(x = x, meanlog = 2 , sdlog = 0.5), 
     type = "l" , lwd = 1.5, col = "blue")
lines(x, dlnorm(x, meanlog = 3 , sdlog = 0.5), 
     type = "l" , lwd = 1.5, col = "orange" )
lines(x, dlnorm(x, meanlog = 3 , sdlog = 1), 
      type = "l" , lwd = 1.5, col = "green" )


# Modelo exponencial , es solamente positiva porque depende del tiempo
x <- seq(from = 0,to = 80,0.001)
plot(x, dexp(x = x , rate = 1/12), type = "l", lwd = 2 , col = "red")
lines(x, dexp(x = x , rate = 1/8), type = "l", lwd = 2 , col = "blue")
lines(x, dexp(x = x , rate = 1/20), type = "l", lwd = 2 , col = "green")
# rate es el tiempo entre clientes
# rate = 1/12  , ej llegan 12 clientes por minuto
#modelo chi cuadrado
# qchisq()
# dchisq()

