## librerias necesarias

# install.packages('faraway')
library(faraway) # con tiene los datos

## visualizar los datos
?faraway::wcgs
data(wcgs)
head(wcgs)
dim(wcgs)
table(wcgs$chd, wcgs$typechd)

is.na(wcgs)
apply(is.na(wcgs),2,sum)
wcgs2=na.omit(wcgs[-c(11,12)]) # se quitan las variables dependientes de chd
dim(wcgs2)



## Modelos

reglog00 <- glm(chd~1, data=wcgs2, family=binomial(link = "logit"))
reglog01 <- glm(chd~cigs, data=wcgs2, family=binomial(link = "logit"))
reglog02 <- glm(chd~., data=wcgs2, family=binomial(link = "logit"))
summary(reglog00)
summary(reglog01)
summary(reglog02)

# Comparación de modelos anidados
# H0: el modelo menor es adecuado
anova(reglog01, reglog02, test="Chisq")
qchisq(0.95, 10)
1-pchisq(167, 10)

plot(wcgs2$age~wcgs2$chd)
plot(wcgs2$height~wcgs2$chd)
reglog03 <- glm(chd~age+weight+sdp+chol+cigs, 
                data=wcgs2, family=binomial(link = "logit"))

summary(reglog03)

anova(reglog01, reglog03, reglog02, test="Chisq")
anova(reglog01, reglog03,  test="Chisq")
anova(reglog03, reglog02, test="Chisq")
qchisq(0.95, 6)

# Comparar mediante AIC y BIC
AIC(reglog01); AIC(reglog02); AIC(reglog03) # se escoge el modelo 2
BIC(reglog01); BIC(reglog02); BIC(reglog03) # se escoge el modelo 3

## Ejercicio: Proponer un modelo

## Busqueda por AIC
?step
reglog04 <- step(reglog02, scope=list(lower=reglog01, upper=reglog02), 
                 direction="both")
reglog04

reglog05 <- step(reglog02, scope=list(lower=reglog01, upper=reglog02), 
                 direction="both", k=log(nrow(wcgs2)))
reglog05

log(nrow(wcgs2))
# Comparar mediante AIC y BIC
AIC(reglog01); AIC(reglog02); AIC(reglog03); AIC(reglog04); AIC(reglog05) # se escoge el modelo 2
BIC(reglog01); BIC(reglog02); BIC(reglog03); BIC(reglog04); BIC(reglog05)  # se escoge el modelo 3


anova(reglog01, reglog03, reglog04, reglog02, test="Chisq")
anova(reglog03, reglog04, test="Chisq")
anova(reglog05, reglog04, test="Chisq") # No aplica, ya que los modelos no estan anidados

reglog04 # mejor modelo por AIC
reglog05 # mejor modelo por BIC


## Analisis del Modelo mejor según AIC y diferencia Deviance

exp(reglog01$coef)[-1]
exp(reglog04$coef)[-1] # odd ratios
reglog04$formula

pred04 <- predict(reglog04, type="response") # ajusta en los datos
cbind(wcgs2[c("age", "weight", "sdp", "chol", "behave", "cigs", "arcus", "chd")], pred04)

summary(wcgs2)

# se define valores de referencia
newdata4 <- expand.grid(
  age=50,
  weight=200,
  sdp=150,
  chol= seq(100,600,l=11),
  behave=c("A1", "A2", "B3", "B4"),
  cigs=10,
  arcus="absent")


pred04_v2 <- predict(reglog04, newdat=newdata4, type="response")

interaction.plot(newdata4$chol, newdata4$behave, pred04_v2)


# calcula el Pseudo R2 de Nagelkerke(1991)
PseudoR2<-function(mod){
  n=nrow(mod$data)
  num=1-exp((mod$deviance-mod$null.deviance)/n)
  den=1-exp(-mod$null.deviance/n)
  return(num/den)
}

PseudoR2(reglog04)


#################################################
## Modelo Binomial

library(faraway)
?orings
orings
n=nrow(orings)
n*6


# Dos formas de ajustar un modelo binomial

#1. gml( presencia/total~covariables, family=binomial, weights=total )
#2. gml( cbind(presencia, ausencia)~covariables, family=binomial)

glm0.ori=glm(cbind(damage, 6-damage)~1, data=orings, family=binomial)
glm1.ori=glm(damage/6~temp, data=orings, family=binomial, weights=rep(6,n) )
glm2.ori=glm(cbind(damage, 6-damage)~temp, data=orings, family=binomial)
summary(glm1.ori)
summary(glm2.ori)

  # Inferencia
drop1(glm2.ori, test="Chisq")             # Esta funcion quita una a una las covariables
anova(glm0.ori, glm2.ori, test="Chisq")   # Compara dos modelos 

deviance(glm2.ori)

# Pseudo R2
PseudoR2(glm2.ori)


# Residuos
res.P=residuals(glm2.ori, "pearson")
res.D=residuals(glm2.ori) 

sum(res.D^2);  deviance(glm2.ori)
sum(res.P^2)                        # estadistico de X^2-Pearson
qchisq(0.975, n-2)
1-pchisq(sum(res.P^2), n-2)         # p-valor del ajuste

plot(res.P)
plot(res.D)
qqnorm(res.P); qqline(res.P)
shapiro.test(res.P)

# versiones estandarizadas
res.std.P=rstandard(glm2.ori, type="pearson")
res.std.D=rstandard(glm2.ori, type="deviance")

qqnorm(res.std.P); qqline(res.std.P)

## Predicciones

summary(orings)
summary(glm2.ori)

predict(glm2.ori, newdata=data.frame(temp=31), "link", se.fit=TRUE)       # entrega el valor de x*beta
predict(glm2.ori, newdata=data.frame(temp=31), "response", se.fit=TRUE)   # entrega el valor de la probabilidad

# Perfil de probabildiades
new=data.frame(temp=0:100)
pred=predict(glm2.ori, newdata=new, "response", se.fit=TRUE) 

plot(cbind(new, pred$fit), type="l", col=2, lwd=3)
points(orings$temp, orings$damage/6, pch=21, bg="gray")

# Perfil de probabildiades
plot(cbind(new, 6*pred$fit), type="l", col=2, lwd=3)
points(orings$temp, orings$damage, pch=21, bg="gray") 

## Interpretacion de los ODD y ODD ratios

temp1=data.frame(temp=c(31,50,60,70))

odd=exp(predict(glm2.ori, newdata=temp1))
odd[1]  # las chances de daño en comparacion de no da?o para una temperatura de 31F
odd[2]  # las chances de daño en comparacion de no da?o para una temperatura de 50F
odd[3]  # las chances de daño en comparacion de no da?o para una temperatura de 60F
odd[4]  # las chances de daño en comparacion de no da?o para una temperatura de 70F

(OR=odd[2]/odd[3])
(odd[3]/odd[4])

exp(coef(glm2.ori)[2]*(10))
exp(coef(glm2.ori)[2]*(-1))   #Por cada grado que se disminuye 
#la temperatura aumentan en un 24% las chances de un da?o
exp(coef(glm2.ori)[2]*(-2)) 


### Residuos de los datos del modelo 1.
wcgs2
plot(residuals(reglog04, "pearson"))
qqnorm(residuals(reglog04, "pearson"))
qqline(residuals(reglog04, "pearson"))

