## librerias necesarias

# install.packages('faraway')
library(faraway) # contiene los datos

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
?glm
reglog00 <- glm(chd~1, data=wcgs2, family=binomial(link = "logit")) #modelo nulo , solo con el intercepto
reglog01 <- glm(chd~cigs, data=wcgs2, family=binomial(link = "logit"))
reglog02 <- glm(chd~., data=wcgs2, family=binomial(link = "logit")) #calcula con todas las covariables
summary(reglog00)
summary(reglog01)
summary(reglog02)

# Comparación de modelos anidados
# H0: el modelo menor es adecuado vs H1 modelo con más variables es más adecuado
anova(reglog01, reglog02, test="Chisq") #el segundo modelo ajusta 10 parametros más que el primer modelo
qchisq(0.95, 10) # 18.30704 // 167,84 es mayor a 18,31 , se rechaza h0
1-pchisq(167, 10) #probabilidad = 0

plot(wcgs2$age~wcgs2$chd) #comparando la edad
plot(wcgs2$height~wcgs2$chd) #comparando estatura

reglog03 <- glm(chd ~ age + weight + sdp + chol + cigs, 
                data=wcgs2, family=binomial(link = "logit"))
summary(reglog03)

anova(reglog01, reglog03, reglog02, test="Chisq") #tienen que estar contenidos
anova(reglog01, reglog03,  test="Chisq") #Deviance 143.8; p-value = 2.2e-16 ***
anova(reglog03, reglog02, test="Chisq") #Deviance 24.039; p-value = 0.0005137 ***
qchisq(0.95, 6) #punto de comparacion 12.59159  < 24.039

# Comparar mediante AIC y BIC (cuando usar aic o bic: cuando modelos no son alineados)
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

#validar primero que esten anidados
anova(reglog01, reglog03, reglog04, reglog02, test="Chisq") #modelo 3 es bueno
anova(reglog03, reglog04, test="Chisq")
anova(reglog05, reglog04, test="Chisq") # No aplica, ya que los modelos no estan anidados

reglog04 # mejor modelo por AIC
reglog05 # mejor modelo por BIC

## Analisis del Modelo mejor según AIC y diferencia Deviance

reglog04$coef #rescatar coeficientes
exp(reglog04$coef)[-1] #odd ratios
reglog04$formula

exp(reglog01$coef)[-1] #cigs 1.023954

#calcula las probabilidades
pred04 <- predict(reglog04, type="response") # ajusta en los datos
cbind(wcgs2[c("age", "weight", "sdp", "chol", "behave", "cigs", "arcus", "chd")], pred04)
cbind(wcgs2, pred04)
summary(wcgs2)

# se define valores de referencia
newdata4 <- expand.grid(
  age = 50,
  weight = 200,
  sdp = 150,
  chol= seq(100, 600, l=11),
  behave = c("A1", "A2", "B3", "B4"),
  cigs = 10,
  arcus = "absent")

pred04_v2 <- predict(reglog04, newdat=newdata4, type="response")

#perfiles de probabilidad
interaction.plot(newdata4$chol, newdata4$behave, pred04_v2)


# calcula el Pseudo R2 de Nagelkerke(1991)
PseudoR2<-function(mod){
  n = nrow(mod$data)
  num = 1 - exp((mod$deviance-mod$null.deviance)/n)
  den = 1 - exp(-mod$null.deviance/n)
  return(num/den)
}

PseudoR2(reglog04) #0.1432563 para datos binarios


#################################################
## Modelo Binomial

library(faraway)
?orings
orings
n=nrow(orings)
n * 6 #es cuando se expande la cantidad de toma de experimentos
data(orings)

# Dos formas de ajustar un modelo binomial

#1. gml( presencia/total ~ covariables, family = binomial, weights=total )
#2. gml( cbind(presencia, ausencia) ~ covariables, family = binomial)

glm0.ori = glm(cbind(damage, 6 - damage) ~ 1, data=orings, family=binomial)
glm1.ori = glm(damage / 6 ~ temp, data = orings, family=binomial, weights = rep(6,n) )
glm2.ori = glm(cbind(damage, 6 - damage) ~ temp, data=orings, family=binomial)
summary(glm1.ori)
summary(glm2.ori)

  # Inferencia
drop1(glm2.ori, test="Chisq")             # Esta funcion quita una a una las covariables
anova(glm0.ori, glm2.ori, test="Chisq")   # Compara dos modelos 

deviance(glm2.ori) #16.91228
glm2.ori$null.deviance #38.89766
glm2.ori$deviance #16.91228

# Pseudo R2
PseudoR2(glm2.ori) #0.7545997


# Residuos
res.P=residuals(glm2.ori, "pearson")
res.D=residuals(glm2.ori) 

sum(res.D^2); deviance(glm2.ori)
sum(res.P^2)                        # estadistico de X^2-Pearson
qchisq(0.975, n-2) #35.47888 , 21  degrees of freedom
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

