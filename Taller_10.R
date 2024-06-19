##############  MODELO UNIFORME  ############## 

#runif  : función de la distribución uniforme continua para generar datos aleatorios 
#dunif  : función de densidad de probabilidad 
#punif  : función de distribución acumulada uniforme continua
#qunif  : función cuantil de la distribución uniforme continua

#Ejemplo
x <- seq(1,5,0.01) 

prob <- dunif(x, min=2, max=4)
prob_acum <- punif(x,min=2, max=4)
proba_acum2 <- 1 - punif(x,min=2, max=4, lower.tail = FALSE) #equivalente 
cbind(x,prob,prob_acum,proba_acum2)

#graf1

library(ggfortify)

ggdistribution(dunif, x, min=2,max=4 , colour = "blue")

#P(X<3)
ggdistribution(dunif, x, min=2,max=4 , colour = "blue",
               p = ggdistribution(dunif, seq(3,4,0.01), min=2,max=4 , colour = "red", fill = "yellow"))

#graf2
df <- data.frame(x, prob)

ggplot(df, aes(x, prob)) +
  geom_step(direction = "vh")+ #vertical luego horizontal
  labs(title = "Distribución Uniforme Continua",
       x = "x",
       y = "Densidad de Probabilidad (PDF)") +
  theme_minimal()

#P(X>3)
ggplot(df, aes(x, prob)) +
  geom_area(data = df[df$x > 3, ], fill = "lightblue") +  
  geom_step(direction = "vh") +
  labs(title = "Distribución Uniforme Continua",subtitle = "P(X>3)",
       x = "x",
       y = "Densidad de Probabilidad") +
  theme_minimal()

#graf3
ggplot(df, aes(x, prob)) +
  geom_line()

##############  MODELO LOG NORMAL  ############## 

#rlnorm  : función de la distribución log normal para generar datos aleatorios 
#dlnorm  : función de densidad de probabilidad 
#plnorm  : función de distribución acumulada uniforme continua
#qlnorm  : función cuantil de la distribución log normal

#Ejemplo
x <- seq(0, 10, length.out = 1000)

prob <- dlnorm(x, meanlog=0.5, sdlog=1)
prob_acum <- plnorm(x,meanlog=0.5, sdlog=1)
proba_acum2 <- 1 - plnorm(x,meanlog=0.5, sdlog=1, lower.tail = FALSE) #equivalente 
cbind(x,prob,prob_acum,proba_acum2)

#graf1
ggdistribution(dlnorm, x, meanlog=0.5, sdlog=1, colour = "blue")
#P(1<X<2.5)
ggdistribution(dlnorm, x, meanlog=0.5, sdlog=1, colour = "blue",
               p = ggdistribution(dlnorm, seq(1,2.5,length.out = 1000), meanlog=0.5, sdlog=1, colour = "red", fill = "yellow"))

#graf2
df <- data.frame(x, prob)

ggplot(df,aes(x,prob)) +
  geom_line()+
  labs(title = "Distribución Log Normal",
     x = "x",
     y = "Densidad de Probabilidad") +
  theme_minimal()

#P(1<X<2.5)
ggplot(df,aes(x,prob)) +
  geom_line()+
  geom_area(data=df[df$x >= 1 & df$x <= 2.5, ], fill = "lightblue") + #para pintar el rango bajo la curva
  labs(title = "Distribución Log Normal",subtitle = "P(1<X<2.5)",
       x = "x",
       y = "Densidad de Probabilidad") +
  theme_minimal()

##############  MODELO EXPONENCIAL  ############## 

#rexp  : función de la distribución exponencial para generar datos aleatorios 
#dexp  : función de densidad de probabilidad 
#pexp  : función de distribución acumulada exponencial
#qexp  : función cuantil de la distribución exponencial

#Ejemplo
x <- seq(0,8,0.001)

prob <- dexp(x, rate = 1)
prob_acum <- pexp(x,rate = 1)
proba_acum2 <- 1 - pexp(x,rate = 1, lower.tail = FALSE) #equivalente 

#graf1
ggdistribution(dexp, x, rate=1, colour = "blue")

#P(X>3)
ggdistribution(dexp, x, rate=1, colour = "blue", 
               p = ggdistribution(dexp , seq(3,8,0.001), rate = 1 , fill ="pink"))

#graf2
df <- data.frame(x, prob)

ggplot(df,aes(x,prob)) +
  geom_line()+
  labs(title = "Distribución Exponencial",
       x = "x",
       y = "Densidad de Probabilidad") +
  theme_minimal()

#P(X<1.5)
ggplot(df,aes(x,prob)) +
  geom_line()+
  geom_area(data=df[df$x <= 1.5, ], fill = "lightblue")+
  labs(title = "Distribución Exponencial",subtitle = "P(X<1.5)",
       x = "x",
       y = "Densidad de Probabilidad") +
  theme_minimal()

##############  MODELO GAMMA  ############## 

#rgamma  : función de la distribución gamma para generar datos aleatorios 
#dgamma  : función de densidad de probabilidad 
#pgamma  : función de distribución acumulada gamma
#qgamma  : función cuantil de la distribución gamma

#Ejemplo
x <- seq(0,10,length.out = 100)

prob <- dgamma(x, shape=2,rate=1)
prob_acum <- pgamma(x, shape=2,rate=1)
proba_acum2 <- 1 - pgamma(x, shape=2,rate=1, lower.tail = FALSE) #equivalente 
cbind(x,prob,prob_acum,proba_acum2)

#graf1
ggdistribution(dgamma, x, shape=2,rate=1, colour = "blue")
ggdistribution(dgamma, x, shape=10,rate=1, colour = "blue")
#graf2
df <- data.frame(x, prob)

ggplot(df,aes(x,prob)) +
  geom_line()+
  labs(title = "Distribución Gamma",
       x = "x",
       y = "Densidad de Probabilidad") +
  theme_minimal()

#P(X<1.5)
ggplot(df,aes(x,prob)) +
  geom_line()+
  geom_area(data=df[df$x <= 1.5, ], fill = "lightblue")+
  labs(title = "Distribución Gamma",subtitle = "P(X<1.5)",
       x = "x",
       y = "Densidad de Probabilidad") +
  theme_minimal()

##############  MODELO WEIBULL  ############## 

#rweibull  : función de la distribución weibull para generar datos aleatorios 
#dweibull  : función de densidad de probabilidad 
#pweibull  : función de distribución acumulada weibull
#qweibull  : función cuantil de la distribución weibull

#Ejemplo
x_weibull <- seq(0,4,length.out = 100)

prob <- dweibull(x_weibull,shape=2,scale=1)
prob_acum <- pweibull(x_weibull,shape=2,scale=1)
proba_acum2 <- 1 - pweibull(x_weibull,shape=2,scale=1, lower.tail = FALSE) #equivalente 

#graf1
ggdistribution(dweibull, x_weibull, shape=2,scale=1, colour = "blue")
ggdistribution(dweibull, x_weibull, shape=10,scale=1, colour = "blue")

#graf2
ggplot(data.frame(x_weibull, prob),aes(x_weibull,prob)) +
  geom_line()+
  labs(title = "Distribución Weibull",
       x_weibull = "x_weibull",
       y = "Densidad de Probabilidad") +
  theme_minimal()

##############  MODELO T-STUDENT  ############## 

#rt  : función de la distribución t-student para generar datos aleatorios 
#dt  : función de densidad de probabilidad 
#pt  : función de distribución acumulada t-student
#qt  : función cuantil de la distribución t-student

#Ejemplo
x <- seq(-4, 4, 0.01)

prob <-dt(x, df=20) 
prob_acum <- pt(x,df=20)
prob_acum2 <- 1-pt(x,df=20,lower.tail = FALSE)

#graf1
ggdistribution(dt, x, df=20, colour = "blue") #df grados de libertad

#graf2
ggplot(data.frame(x, prob),aes(x,prob)) +
  geom_line()+
  labs(title = "Distribución T - Student",
       x = "x",
       y = "Densidad de Probabilidad") +
  theme_minimal()


##############  MODELO Chi-cuadrado  ############## 

#rchisq  : función de la distribución chi-cuadrado para generar datos aleatorios 
#dchisq  : función de densidad de probabilidad 
#pchisq  : función de distribución acumulada chi-cuadrado
#qchisq  : función cuantil de la distribución chi-cuadrado

#Ejemplo
x <- seq(0, 20, 0.1)

prob <-dchisq(x, df=5) 
prob_acum <- pchisq(x,df=5)
prob_acum2 <- 1-pchisq(x,df=5,lower.tail = FALSE)

#graf1
ggdistribution(dchisq, x, df=5, colour = "blue")

#graf2
ggplot(data.frame(x, prob),aes(x,prob)) +
  geom_line()+
  labs(title = "Distribución chi - cuadrado",
       x = "x",
       y = "Densidad de Probabilidad") +
  theme_minimal()


##############  MODELO F  ############## 

#rf  : función de la distribución F para generar datos aleatorios 
#df  : función de densidad de probabilidad 
#pf  : función de distribución acumulada F
#qf  : función cuantil de la distribución F

#Ejemplo
x <- seq(0, 20, 0.1)

prob <- df( x, df1=3, df2=2)
prob_acum <- pf(x,df1=3, df2=2)
prob_acum2 <- 1-pf(x,df1=3, df2=2,lower.tail = FALSE)

#graf1
ggdistribution(df, x, df1=3, df2=2, colour = "blue")
?ggdistribution
#graf2
ggplot(data.frame(x, prob),aes(x,prob)) +
  geom_line()+
  labs(title = "Distribución F",
       x = "x",
       y = "Densidad de Probabilidad") +
  theme_minimal()
