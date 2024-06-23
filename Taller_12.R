####### ESTIMACIÓN Y AJUSTE #######
library(ggplot2)
#library(MASS)         #fitdistr : estimador de momentos (mas antiguo)
library(fitdistrplus) #fitdist  : estimador de momentos y EMV 
library(propagate)    #fitDistr : estimador y evaluador


####### EJEMPLO 1 #######
# Genera 800 datos normales
set.seed(2024)  # semilla para generar siempre la misma secuencia aleatoria
Puntaje <- rnorm(800, 580, 90)  # simulacion de datos

# Ajustar distribición NORMAL a los datos
#1. Buscar parámetros
nor <- fitdist(Puntaje, "norm", method="mle") # estimadores para parámetros de Normal y su error estandar
#?fitdist
print(nor)
#fitdist(Puntaje,"norm", method="mme") #estimador de momentos
#fitdist(Puntaje,"norm", method="mle") #estimador EMV
typeof(nor)
param1 <- nor$estimate[1]   # mean
param2 <- nor$estimate[2]   # sd

confint(nor) #proporciona además intervalos de confianza para los estimadores

ggplot(data.frame(Puntaje), aes(x = Puntaje)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "skyblue", color = "blue") +
  geom_function(fun = dnorm, args = list(mean = param1, sd = param2), aes(color = "Curva Normal")) +
  scale_color_manual(name = "",values = c("Curva Normal" = "red")) +
  labs(title = "Histograma de Puntajes",
       x = "Puntajes",
       y = "Densidad") +
  theme_minimal()

#2. Bondad del ajuste 
ks.test(Puntaje,"pnorm",param1,param2)  
#p-value = 0.8204 --> no rechaza el ajuste

# Ajustar distribición WEIBULL a los datos
#1. Buscar parámetros
weib <- fitdist(Puntaje, "weibull", method="mle")
param3 <- weib$estimate[1] # shape
param4 <- weib$estimate[2] # scale

ggplot(data.frame(Puntaje), aes(x = Puntaje)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "skyblue", color = "blue") +
  geom_function(fun = dweibull, args = list(shape = param3, scale = param4), aes(color = "Curva Weibull")) +
  scale_color_manual(name = "Ajuste",values = c("Curva Weibull" = "red")) +
  labs(title = "Histograma de Puntajes",
       x = "Puntajes",
       y = "Densidad") +
  theme_minimal()

#2. Bondad del ajuste
ks.test(Puntaje,"pweibull",param3,param4)
# no rechazó el ajuste p-value = 0.09084
#p_value_referencia = 0.05

# histograma con las dos curvas
ggplot(data.frame(Puntaje), aes(x = Puntaje)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "skyblue", color = "blue") +
  geom_function(fun = dnorm, args = list(mean = param1, sd = param2), aes(color = "Curva Normal")) +
  geom_function(fun = dweibull, args = list(shape = param3, scale = param4), aes(color = "Curva Weibull")) +
  scale_color_manual(name = "Ajustes",values = c("Curva Normal"="black", "Curva Weibull" = "red")) +
  labs(title = "Histograma de Puntajes",
       x = "Puntajes",
       y = "Densidad") +
  theme_minimal()
?fitdist
# Ajustar distribición LOG-NORMAL a los datos
log_nor <- fitdist(Puntaje, "lnorm", method="mle")
param5 <- log_nor$estimate[1] # meanlog
param6 <- log_nor$estimate[2] # sdlog  

ggplot(data.frame(Puntaje), aes(x = Puntaje)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "skyblue", color = "blue") +
  geom_function(fun = dlnorm, args = list(meanlog=param5, sdlog=param6), aes(color = "Curva Log-Normal")) +
  scale_color_manual(name = "Ajustes",values = c("Curva Log-Normal" = "red")) +
  labs(title = "Histograma de Puntajes",
       x = "Puntajes",
       y = "Densidad") +
  theme_minimal()

#2. Bondad del ajuste 
ks.test(Puntaje,"plnorm",param5,param6) 
# no rechazó el ajuste, p-value = 0.1862

# Histograma con las tres curvas
ggplot(data.frame(Puntaje), aes(x = Puntaje)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "skyblue", color = "blue") +
  geom_function(fun = dnorm, args = list(mean = param1, sd = param2), aes(color = "Curva Normal"),lwd = 1) +
  geom_function(fun = dweibull, args = list(shape = param3, scale = param4), aes(color = "Curva Weibull"),lwd = 1) +
  geom_function(fun = dlnorm, args = list(meanlog=param5, sdlog=param6), aes(color = "Curva Log-Normal"),lwd = 1) +
  scale_color_manual(name = "Ajustes",values = c("Curva Normal"="black", "Curva Weibull" = "red","Curva Log-Normal"="purple")) +
  labs(title = "Histograma de Puntajes",
       x = "Puntajes",
       y = "Densidad") +
  theme_minimal()

# Ajustar distribición CAUCHY (o Lorentz) a los datos
cau <- fitdist(Puntaje, "cauchy",method="mle")
param7 <- cau$estimate[1]   # location
param8 <- cau$estimate[2]   # scale

ggplot(data.frame(Puntaje), aes(x = Puntaje)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "skyblue", color = "blue") +
  geom_function(fun = dcauchy, args = list(location=param7, scale=param8), aes(color = "Curva Cauchy")) +
  scale_color_manual(name = "Ajustes",values = c("Curva Cauchy" = "red")) +
  labs(title = "Histograma de Puntajes",
       x = "Puntajes",
       y = "Densidad") +
  theme_minimal()

#2. Bondad del ajuste 
ks.test(Puntaje,"pcauchy",param7,param8) # rechazó el ajuste, p-value = 0.0000

# Histograma con las cuatro curvas
ggplot(data.frame(Puntaje), aes(x = Puntaje)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "skyblue", color = "blue") +
  geom_function(fun = dnorm, args = list(mean = param1, sd = param2), aes(color = "Curva Normal"),lwd = 1) +
  geom_function(fun = dweibull, args = list(shape = param3, scale = param4), aes(color = "Curva Weibull"),lwd = 1) +
  geom_function(fun = dlnorm, args = list(meanlog=param5, sdlog=param6), aes(color = "Curva Log-Normal"),lwd = 1) +
  geom_function(fun = dcauchy, args = list(location=param7, scale=param8), aes(color = "Curva Cauchy"),lwd = 1) +
  scale_color_manual(name = "Ajustes",values = c("Curva Normal"="black", "Curva Weibull" = "red","Curva Log-Normal"="purple","Curva Cauchy"= "yellow")) +
  labs(title = "Histograma de Puntajes",
       x = "Puntajes",
       y = "Densidad") +
  theme_minimal()

#Buscamos el mejor ajuste
fitDistr(Puntaje, plot = c("qq"))
fitDistr(Puntaje, plot = c("hist"))
# Best fit is Normal Distribution.
# Parameters:
#   mean        sd 
# 580.96967  87.94474 
# Standard errors:
#   mean       sd 
# 3.252305 2.656226 
# Goodness of fit:
#   BIC = -1344.981
#BIC: Bayesian information criterion
#métricas que permiten cuantificar cómo de bien se ajusta una distribución a los datos observados
#mas pequeño

####### EJEMPLO 2 #######
# Genera 1000 datos exponenciales
set.seed(1910)  
datos <- rexp(10000000, rate = 0.15)   
class(datos)
ggplot(data.frame(datos), aes(x = datos)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "blue") +
  labs(title = "Histograma",
       x = "Datos",
       y = "Densidad") +
  theme_minimal()

#Ajustes
#a) Exponencial
expon <- fitdist(datos,"exp",method="mle")
p1 <- expon$estimate[1]  # rate
ks.test(datos,"pexp",p1) # no rechazó el ajuste, p-value = 0.6847

#b) Gamma 
gam <- fitdist(datos,"gamma",method="mle")
p2 <- gam$estimate[1]  # shape 
p3 <- gam$estimate[2]  # rate 
ks.test(datos,"pgamma",p2, p3) # rechazó el ajuste, p-value = 0.766

#c) Weibull 
weib <- fitdist(datos, "weibull",method="mle")
p4 <- weib$estimate[1] # shape
p5 <- weib$estimate[2] # scale
ks.test(datos,"pweibull",p4,p5) # rechazó el ajuste p-value = 0.7291

#d) Log-Normal
log_nor <- fitdist(datos, "lnorm",method="mle")
p6 <- log_nor$estimate[1] #mean
p7 <- log_nor$estimate[2] #sd
ks.test(datos,"plnorm",p6,p7) # rechazó el ajuste, p-value = 0.000


ggplot(data.frame(datos), aes(x = datos)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "skyblue", color = "blue") +
  geom_function(fun = dexp, args = list(rate=p1), aes(color = "Curva Exponencial"),lwd = 1) +
  geom_function(fun = dgamma, args = list(shape = p2, rate=p3), aes(color = "Curva Gamma"),lwd = 0.7,lty = 2) +
  geom_function(fun = dweibull, args = list(shape=p4,scale=p5), aes(color = "Curva Weibull"),lwd = 0.7,lty = 2) +
  geom_function(fun = dlnorm, args = list(mean=p6,sd=p7), aes(color = "Curva Log-Normal"),lwd = 0.7,lty = 2) +
  scale_color_manual(name = "Ajustes",values = c("Curva Exponencial"="black", "Curva Gamma" = "red","Curva Weibull"="purple","Curva Log-Normal"= "yellow")) +
  labs(title = "Histograma",
       x = "Datos",
       y = "Densidad") +
  theme_minimal()

#mejor ajuste
fitDistr(datos)







   