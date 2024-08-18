####################################################
## librerias Necesarias: Multinomial

library(faraway)
library(nnet)
library(VGAM)
library(MASS)

####################################################
## Lectura de la data y verificación de formato

?nes96
data(nes96)
summary(nes96)

# Se recategirza PID en 3 niveles
# La variable income se transforma a numerica

levels(nes96$PID)
nes96$income
party <- nes96$PID
levels(party) <- c("Democrat","Democrat","Independent","Independent",
                   "Independent","Republican","Republican")
data.frame(nes96$PID, party)

inca <- c(1.5,4,6,8,9.5,10.5,11.5,12.5,13.5,14.5,16,18.5,21,23.5,
          27.5,32.5,37.5,42.5,47.5,55,67.5,82.5,97.5,115)
income <- inca[unclass(nes96$income)]
data.frame(nes96$income, income)

rnes96 <- data.frame(party, income, education=nes96$educ, age=nes96$age)
summary(rnes96)
head(rnes96)    # la data viene desagregada

## Multicolinealidad
cor(rnes96[c("income","age")])^2
faraway::vif(rnes96[c("income","age")])

####################################################
## Se ajusta un modelo multinomial nominal

?multinom
multi0 <- multinom(party ~ 1, data=rnes96)
multi1 <- multinom(party ~ age + education + income, data=rnes96)
multi0
multi1
summary(multi0)
summary(multi1)
anova(multi0, multi1)
AIC(multi0, multi1)
BIC(multi0, multi1)
qchisq(0.95,16)


## Calidad del ajuste por separado
n=nrow(rnes96)
deviance(multi1); qchisq(0.95, n-multi1$edf)  # Nivel de ajuste modelo 1
1-pchisq(deviance(multi1), n-multi1$edf)

deviance(multi0); qchisq(0.95, n-multi0$edf)  # Nivel de ajuste modelo 0
1-pchisq(deviance(multi0), n-multi0$edf)

# Calculo manual de la diferencia de deviance (equivalente al anova)
deviance(multi0)-deviance(multi1); qchisq(0.95, multi1$edf-multi0$edf)
1-pchisq(deviance(multi0)-deviance(multi1), multi1$edf-multi0$edf)

## Mejorar el modelo quitando una variable
multi1.1 <- multinom(party ~ education + income, data=rnes96)
multi1.2 <- multinom(party ~ age + income, data=rnes96)
multi1.3 <- multinom(party ~ age + education, data=rnes96)

anova(multi1.1, multi1)
anova(multi1.2, multi1)
anova(multi1.3, multi1)

AIC(multi1.1 , multi1.2, multi1.3 )
BIC(multi1.1 , multi1.2, multi1.3 )

# nos quedamos con el modelo 1.1
multi2 <- multinom(party ~ education + income, data=rnes96)
multi2.1 <- multinom(party ~ income, data=rnes96)
multi2.2 <- multinom(party ~ education, data=rnes96)

anova(multi2.1, multi2)
anova(multi2.2, multi2)

AIC(multi2.1 , multi2.2, multi2, multi1)
BIC(multi2.1 , multi2.2, multi2, multi1)

anova(multi2.1, multi2, multi1)
anova(multi2.1, multi1)

# nos quedamos con el modelo 2.1
multi3 <- multinom(party ~ income, data=rnes96)
deviance(multi3); qchisq(0.95, n-multi3$edf)  # Nivel de ajuste modelo 3
1-pchisq(deviance(multi3), n-multi3$edf) 

plot(rnes96$income~rnes96$party)
hist(rnes96$income)
quantile(rnes96$income)

# Busqueda por AIC y BIC
multi4 <- step(multi1)
multi4
multi3

## ODD-RATIO
exp(coef(multi3)[,2]*1)  
exp(coef(multi3)[,2]*10)
exp(coef(multi3)[,2]*100)


## Predicciones
predict(multi3)
predict(multi3, type="probs")
data.frame(income, party, predict(multi3), predict(multi3, type="probs"))

table(party, predict(multi3))  # Predicciones con el mejor modelo
table(party, predict(multi1))  # Predicciones con el modelo con todas las covariables

(ac1=sum(diag(table(party, predict(multi1))))/n)*100
(ac3=sum(diag(table(party, predict(multi3))))/n)*100

cbind(inca, predict(multi3, type="probs", newdata=data.frame(income=inca)))


#######################################################################
## Modelos Multinomiales Ordinales

# El supuesto en los modelos siguientes es el orden en las categorias
# Se asume que independientes esta entre democratas y republicanos

# Modelos ordinales proporcional

polr0 = polr(party ~ 1, data=rnes96)  # Logistico
polr1 = polr(party ~ age + income, data=rnes96)  # Logistico
polr2 = polr(party ~ age + income, method="probit", data=rnes96) # Probit
polr3 = polr(party ~ age + income, method="cloglog", data=rnes96) # hazard o cloglog

p0=table(rnes96$party)/nrow(rnes96)
p0
p0[-1]/p0[1]
log(p0[-1]/p0[1])
log(p0[1]/sum(p0[2:3]))
polr0

polr1
polr2
polr3

summary(polr1)
summary(polr2)
summary(polr3)
  

# Alternativas para Logistico Proporcional y no proporcional

vglm1 = vglm(party ~ age + income, family=cumulative(parallel = TRUE), data=rnes96)  # proporcional
vglm2 = vglm(party ~ age + income, family=cumulative(parallel = FALSE), data=rnes96) # No proporcional

vglm1
vglm2

summary(vglm1)
summary(vglm2)

# la funcion vglm con la opcion  family=cumulative(parallel = TRUE) es el mismo modelo que polr1
# la diferencia esta en al parametrizacion: C+xb, C-xb 

############################################
# Comparaci?n entre los diferentes modelos

multi1
summary(multi1)
(1-pnorm(abs(summary(multi1)$coeff)/summary(multi1)$standard.errors))*2 # Estad?stico de Wald
1-pchisq(deviance(multi1), multi1$edf)   # No hay un buen ajuste. (falta comparar con el modelo 0)

polr1
summary(polr1)  
deviance(polr1); deviance(multi1)

deviance(polr1)-deviance(multi1); qchisq(0.95, 2)
1-pchisq(deviance(polr1)-deviance(multi1), 2)     # Se rechaza H0 (el modelo con menos par?metros)

polr2
summary(polr2)
deviance(polr2); deviance(polr1); deviance(multi1)

polr3
summary(polr3)
deviance(polr3); deviance(polr2); deviance(polr1); deviance(multi1)

## Hipotesis sobre proporcionalidad 

vglm1 # modelo proporcional
vglm2 # modelo no proporcional

# Compración de deviance
deviance(vglm1)-deviance(vglm2)
qchisq(0.95, 2)

1-pchisq(deviance(vglm1)-deviance(vglm2),2)   # p-valor
# Evalua la proporcionalidad, se debería rechazar

# Nos quedamos el con mult1, pero tampoco es la mejor opción ya que tiene una covariable no significativa

####################################
## Algunas predicciones

summary(rnes96)

new1=data.frame(age=seq(18, 85, by=1), income=50, education="HS")

cbind(new1, predict(multi1, newdata=new1, type="probs") )
cbind(new1, predict(polr1,  newdata=new1, type="probs") )
cbind(new1, predict(polr2,  newdata=new1, type="probs") )
cbind(new1, predict(polr3,  newdata=new1, type="probs") )
cbind(new1, predict(vglm1,  newdata=new1, type="response") )
cbind(new1, predict(vglm2,  newdata=new1, type="response") )

par(mfrow=c(1,1))
plot(new1$age, predict(multi1, newdata=new1, type="probs")[,1], pch=21, bg=2, type="o", lty=3, ylim=c(0.25,0.41), cex=0.75 )
points(new1$age, predict(multi1, newdata=new1, type="probs")[,2], pch=21, bg=3, type="o", lty=3, cex=0.75 )
points(new1$age, predict(multi1, newdata=new1, type="probs")[,3], pch=21, bg=4, type="o", lty=3, cex=0.75 )

points(new1$age, predict(vglm2, newdata=new1, type="response")[,1], pch=21, bg=5, type="o", lty=3, ylim=c(0.25,0.41) , cex=0.75)
points(new1$age, predict(vglm2, newdata=new1, type="response")[,2], pch=21, bg=6, type="o", lty=3, cex=0.75 )
points(new1$age, predict(vglm2, newdata=new1, type="response")[,3], pch=21, bg=7, type="o", lty=3, cex=0.75 )

###

new2=data.frame(age=70, income=seq(1,100, by=1), education="HS" )

cbind(new2, predict(multi1, newdata=new2, type="probs") )
cbind(new2, predict(polr1,  newdata=new2, type="probs") )
cbind(new2, predict(polr2,  newdata=new2, type="probs") )
cbind(new2, predict(polr3,  newdata=new2, type="probs") )
cbind(new2, predict(vglm1,  newdata=new2, type="response") )
cbind(new2, predict(vglm2,  newdata=new2, type="response") )

par(mfrow=c(1,1))
plot(new2$income, predict(multi1, newdata=new2, type="probs")[,1], pch=21, bg=2, type="o", lty=3, ylim=c(0.15,0.6), cex=0.75 )
points(new2$income, predict(multi1, newdata=new2, type="probs")[,2], pch=21, bg=3, type="o", lty=3, cex=0.75 )
points(new2$income, predict(multi1, newdata=new2, type="probs")[,3], pch=21, bg=4, type="o", lty=3, cex=0.75 )

points(new2$income, predict(vglm2, newdata=new2, type="response")[,1], pch=21, bg=5, type="o", lty=3, cex=0.75 )
points(new2$income, predict(vglm2, newdata=new2, type="response")[,2], pch=21, bg=6, type="o", lty=3, cex=0.75 )
points(new2$income, predict(vglm2, newdata=new2, type="response")[,3], pch=21, bg=7, type="o", lty=3, cex=0.75 )

## Antes de decidir cual modelo usar, se debe tener presente para que se requiere 
## así como también realizar un diagnóstico del modelo
