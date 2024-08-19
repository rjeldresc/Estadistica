################################################################
## Modelos no lineales 
#7684390 ambientes pre prod@bancoestado.cl
# Modelo de Michaelis-Menten

MM <- function(b, x){ # b: vector de parámetros
  b[1]*x/(x+b[2])
}

b0=c(2,4)
n=101
x=seq(0,10,l=n)
er=rnorm(n,0,0.05)

y0=MM(b=b0, x=x)
y=y0+er
plot(x,y, pch=20)
lines(x,y0, col=2, lwd=2)


f.obj <- function(b){ # suma de los errores al cuadrado, x e y los datos
  yh=MM(b=b, x=x)
 sum((y-yh)^2)  
}

b0
f.obj(b=b0)
f.obj(b=c(1,1))
f.obj(b=c(0,1))

res.opt1=optim(par=c(1,1), fn=f.obj)
res.opt2=optim(par=c(1,1), fn=f.obj)

res.opt1$par
f.obj(b=b0)
res.opt1$value

res.opt2$par; b0
f.obj(b=b0)
res.opt2$value

plot(x,y, pch=20)
lines(x,y0, col=2, lwd=2)
lines(x, MM(b=res.opt2$par, x=x), col=3, lwd=2)

lm1 <- lm(y~poly(x, 3, raw=TRUE))
lm2 <- lm(y~poly(x, 5, raw=TRUE))
lm1
summary(lm1)
summary(lm2)

faraway::vif(poly(x, 3, raw=TRUE))
faraway::vif(poly(x, 5, raw=TRUE))


plot(x,y, pch=20)
lines(x,y0, col=2, lwd=2)
lines(x, MM(b=res.opt2$par, x=x), col=3, lwd=2)
lines(x, lm1$fitted.values, col=4, lwd=2)
lines(x, lm2$fitted.values, col=5, lwd=2)

##-----------------------------------------------------------------##
## Validación cruzada

library(DAAG)
summary(spam7)
nrow(spam7)

# 70-30 train-test
0.7*4601
id=sample.int(4601, 3221)
spam.train <- spam7[id,]
spam.test <- spam7[-id,]

summary(spam.train)
summary(spam.test)

table(spam7$yesno)/nrow(spam7)*100 # todos
table(spam.train$yesno)/nrow(spam.train)*100 # entrenamiento
table(spam.test$yesno)/nrow(spam.test)*100 # test

## ajustar un modelo logistico


library(caret)
?train
head(spam.train)
mod1=train(yesno~crl.tot+bang, data=spam.train, method="glm", family=binomial,
      trControl = trainControl(method = "cv",  number=10))

mod2=train(yesno~crl.tot+bang+dollar, data=spam.train, method="glm", family=binomial,
           trControl = trainControl(method = "cv",  number=10))

mod3=train(yesno~., data=spam.train, method="glm", family=binomial,
           trControl = trainControl(method = "cv",  number=10))

# modelo escogido mediante inferencia, son 2^6=64 modelo diferentes, sin considerar interacciones.
mod4=train(yesno~crl.tot+dollar+bang+money+n000, data=spam.train, method="glm", family=binomial,
           trControl = trainControl(method = "cv",  number=10))

mod1$results
mod2$results
mod3$results
mod4$results

# resultados interno de las carpetas
mod1$resample
mod2$resample
mod3$resample
mod4$resample


glm1=glm(yesno~crl.tot+bang, data=spam.train, family=binomial)
glm2=glm(yesno~crl.tot+bang+dollar, data=spam.train, family=binomial)
glm3=glm(yesno~., data=spam.train, family=binomial)
glm4=glm(yesno~crl.tot+dollar+bang+money+n000, data=spam.train, family=binomial)

summary(glm3)
summary(glm2)
summary(glm1)
anova(glm1,glm2, test="Chisq")
anova(glm1, glm2, glm3, test="Chisq")

# otras medidas de la matriz de confusión

predict(glm4, spam.test, type="response")
ifelse(predict(glm4, spam.test, type="response")>=0.5,"y","n")

?confusionMatrix
caret::confusionMatrix(predict(mod4, spam.test), spam.test$yesno, mode = "everything")
caret::confusionMatrix(factor(ifelse(predict(glm4, spam.test, type="response")>=0.5,"y","n")), 
                       spam.test$yesno, mode = "everything")

##---------------------------------------------------------------------------##
## Actividad en Clases

library(faraway)
?amlxray
head(amlxray)
summary(amlxray)
reg.log.logit1 <- glm(disease~., data=amlxray[,-1], family=binomial(link="logit"))
summary(reg.log.logit1)

# 2
# AIC para seleccionar
reg.log.logit2 <- step(reg.log.logit1)

# 3
summary(reg.log.logit2)
reg.log.logit3 <- glm(disease~CnRay, data=amlxray[,-1], family=binomial(link="logit"))
summary(reg.log.logit3)

anova(reg.log.logit3, reg.log.logit2, test="Chisq")

reg.log.logit4 <- glm(disease~CnRay+Fray, data=amlxray[,-1], family=binomial(link="logit"))
summary(reg.log.logit4)


anova(reg.log.logit4, reg.log.logit2, test="Chisq")

reg.log.logit5 <- glm(disease~CnRay+Fray+MupRay, data=amlxray[,-1], family=binomial(link="logit"))
summary(reg.log.logit5)

anova(reg.log.logit5, reg.log.logit2, test="Chisq")

# 4
round(exp(coef(reg.log.logit2)[-1]),2)

# 5

