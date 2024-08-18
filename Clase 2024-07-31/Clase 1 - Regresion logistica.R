install.packages("faraway")
library(faraway) #contiene los datos

## visualizar los datos
faraway::wcgs
data(wcgs)
head(wcgs)
?faraway::wcgs

#variable respuesta = chd
#covariable = cigs

## analisis descriptivo
boxplot(wcgs$cigs ~ wcgs$chd)
tapply(wcgs$cigs , wcgs$chd , mean) #media por grupo
tapply(wcgs$cigs , wcgs$chd , sd)   #desviacion estandar por grupo

var.test(wcgs$cigs ~ wcgs$chd)

#prueba t para validar si las medias son iguales o distintas
t.test(wcgs$cigs ~ wcgs$chd , var.equal = F ) #prueba de diferencia de medias
#se rechaza que las medias son iguales t = -5.4472, df = 295.31, p-value = 1.079e-07

## modelo regresion logistica
#chd es categorica
?glm
reglog1 <- glm(chd ~ cigs , data = wcgs , family = binomial(link = "logit")) #chd depende de la cantidad de cigs
reglog1

summary(reglog1)

## odd
reglog1$coefficients
exp(reglog1$coefficients)

## odd para x=1 exp(b0 + b1*X) x es la cantidad de cigarrillos, en este caso 1
sum(reglog1$coefficients)
o1 = exp(sum(reglog1$coefficients))
o1 #0.06594464

## odd para x=0 exp(b0 + b1*X) caso no fuma cigarrillos
sum(reglog1$coefficients[1])
o0 = exp(sum(reglog1$coefficients[1]))
o0 #0.06443106

## odd ratio x = 1 vs x = 0
o1/o0 # 1.023491
exp(sum(reglog1$coefficients[2]))


## odd ratio x = 10 vs x = 0
exp(sum(reglog1$coefficients[2] * 10))
#resp :  1.26137

#Calcular la probabilidad de tener un infarto si x = 10
       exp(reglog1$coefficients[1] + reglog1$coefficients[2] * 10 ) /
  (1 + exp(reglog1$coefficients[1] + reglog1$coefficients[2] * 10))
#0.07516279

#para predecir
newdata <- data.frame(cigs = 0:200)
pred.prod1 <- predict(reglog1, newdata, type = "response" ) #calcula la probabilidad por cada x , por cada cigarrillo
predict(reglog1, newdata, type = "link" ) #funcion enlace calcula xbeta

plot(newdata$cigs , pred.prod1 , type = "l")
abline(h=0.5)

# Segundo modelo
reglog2 <- glm(chd ~ cigs + age , data = wcgs , family = binomial(link = "logit"))
reglog2
summary(reglog2)

newdata2 <- expand.grid(cigs = seq(0,100, by = 10 ), age = seq(39,50, by = 2))
pred.prod2 <- predict(reglog2, newdata2, type = "response" )
cbind(newdata2, pred.prod2)
interaction.plot(newdata2$cigs, newdata2$age,pred.prod2 )

