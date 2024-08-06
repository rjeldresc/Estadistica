library(faraway) #contiene los datos

## visualizar los datos
faraway::wcgs
data(wcgs)
head(wcgs)
?faraway::wcgs
  
## analisis descriptivo
boxplot(wcgs$cigs ~ wcgs$chd )
tapply(wcgs$cigs , wcgs$chd , mean) #media por grupo
tapply(wcgs$cigs , wcgs$chd , sd)   #sd por grupo

var.test(wcgs$cigs ~ wcgs$chd)
#prueba t para validar si las medias son iguales o distintas
t.test(wcgs$cigs ~ wcgs$chd , var.equal = F ) #prueba de diferencia de medias
#se rechaza que las medias son iguales

## modelo regresion logistica
reglog1 <- glm(chd ~ cigs , data = wcgs , family = binomial(link = "logit"))
reglog1

summary(reglog1)

## odd
reglog1$coefficients
exp(reglog1$coefficients)

## odd para x=1 exp(b0 + b1*X)
sum(reglog1$coefficients)
o1 = exp(sum(reglog1$coefficients))
o1

## odd para x=0 exp(b0 + b1*X)
sum(reglog1$coefficients[1])
o0 = exp(sum(reglog1$coefficients[1]))
o0

## odd ratio x = 1 vs x = 0
o1/o0 #1.023491
exp(sum(reglog1$coefficients[2]))


## odd ratio x = 10 vs x = 0
exp(sum(reglog1$coefficients[2] * 10)) 
#resp :  1.26137

#calcular la probabilidad de tener un infarto si x = 10
exp(reglog1$coefficients[1] + reglog1$coefficients[2]  * 10 ) /
  (1 + exp(reglog1$coefficients[1] + reglog1$coefficients[2] * 10))
#0.07516279

newdata <- data.frame( cigs = 0:200)
pred.prod1 <- predict(reglog1, newdata, type = "response" )
predict(reglog1, newdata, type = "link" ) #funcion enlace calcula xbeta

plot(newdata$cigs , pred.prod1 , type = "l")

#segundo modelo

reglog2 <- glm(chd ~ cigs + age , data = wcgs , family = binomial(link = "logit"))
reglog2
summary(reglog2)

newdata2 <- expand.grid(cigs = seq(0,100, by = 10 ), age = seq(39,50, by = 2))
pred.prod2 <- predict(reglog2, newdata2, type = "response" )
cbind(newdata2, pred.prod2)
interaction.plot(newdata2$cigs, newdata2$age,pred.prod2 )

