## Librerias

 library(DAAG)
 library(ggplot2)
 
## datos y analisis exploratorio
 
 ?spam7 
 summary(spam7)
 dim(spam7)
 faraway::vif(spam7[,-7])
  
 names(spam7)

 
 ggplot(spam7, aes(x=yesno, y=crl.tot, fill=yesno))+
   geom_violin(alpha=0.5)+
   geom_boxplot(fill=c("lightsalmon","skyblue"), alpha=0.7, width=0.1)+
   ylim(0,1000) # se trunca a 1000
 
 ggplot(spam7, aes(x=yesno, y=dollar, fill=yesno))+
   geom_violin(alpha=0.5)+
   geom_boxplot(fill=c("lightsalmon","skyblue"), alpha=0.7, width=0.1)+
   ylim(0,0.25) # se trunca a 0.25

 ggplot(spam7, aes(x=yesno, y=bang, fill=yesno))+
   geom_violin(alpha=0.5)+
   geom_boxplot(fill=c("lightsalmon","skyblue"), alpha=0.7, width=0.1)+
   ylim(0,0.5) # se trunca a 0.5
 
 ggplot(spam7, aes(x=yesno, y=money, fill=yesno))+
   geom_violin(alpha=0.5)+
   geom_boxplot(fill=c("lightsalmon","skyblue"), alpha=0.7, width=0.1)+
   ylim(0,0.1) # se trunca a 0.1
 
 ggplot(spam7, aes(x=yesno, y=n000, fill=yesno))+
   geom_violin(alpha=0.5)+
   geom_boxplot(fill=c("lightsalmon","skyblue"), alpha=0.7, width=0.1)+
   ylim(0,0.1) # se trunca a 0.1

 ggplot(spam7, aes(x=yesno, y=make, fill=yesno))+
   geom_violin(alpha=0.5)+
   geom_boxplot(fill=c("lightsalmon","skyblue"), alpha=0.7, width=0.1)+
   ylim(0,0.1) # se trunca a 0.1
 
 
## Regresión Logística
 RegLog_logit1 <- glm(yesno~., data=spam7, family = binomial(link="logit"))
 summary(RegLog_logit1)
 RegLog_logit2 <- glm(yesno~., data=spam7[,which(names(spam7)!="make")], 
                      family = binomial(link="logit"))
 summary(RegLog_logit2)
 RegLog_logit3 <- glm(yesno~crl.tot+dollar+bang+money, data=spam7, 
                      family = binomial(link="logit"))
 summary(RegLog_logit3)
 
 anova(RegLog_logit3, RegLog_logit2, test="Chisq")
 anova(RegLog_logit3, RegLog_logit1, test="Chisq")
 anova(RegLog_logit2, RegLog_logit1, test="Chisq")
 
 # busqueda por BIC
 RegLog_logit4 <- step(RegLog_logit1, k=log(nrow(spam7))) # se recupera modelo 2

## Regresión Logística: Probit
 RegLog_probit1 <- glm(yesno~., data=spam7, family = binomial(link="probit"))
 summary(RegLog_probit1)
 car::Anova(RegLog_probit1, type="II")
 
 RegLog_probit2 <- glm(yesno~., data=spam7[,which(names(spam7)!="make")], 
                      family = binomial(link="probit"))
 summary(RegLog_probit2)
 car::Anova(RegLog_probit2, type="II")
 
 RegLog_probit3 <- glm(yesno~crl.tot+dollar+bang+n000, data=spam7, 
                       family = binomial(link="probit"))
 summary(RegLog_probit3)
 car::Anova(RegLog_probit3, type="II")
 
 RegLog_probit4 <- glm(yesno~crl.tot+dollar+bang+money, data=spam7, 
                      family = binomial(link="probit"))
 summary(RegLog_probit4)
 
 anova(RegLog_probit2, RegLog_probit1, test="Chisq")
 anova(RegLog_probit3, RegLog_probit1, test="Chisq")
 anova(RegLog_probit4, RegLog_probit1, test="Chisq")
 
 anova(RegLog_probit3, RegLog_probit2, test="Chisq")
 
 # busqueda por BIC
 RegLog_probit5 <- step(RegLog_probit1, k=log(nrow(spam7))) # se recupera modelo 1
 
 
 ##############################################################
 ## Ejemplo Poisson
 
 ##############################################
 ## Librerias adecuada
 
 install.packages("GLMsData")
 library(GLMsData)
 
 #############################################
 ## cargar la Data y analisis descriptivo
 
 data(danishlc)
 ?danishlc
 summary(danishlc)
 head(danishlc)
 n=nrow(danishlc)
 
 table(danishlc$Age, danishlc$City)
 
 plot(Cases/Pop ~ City, data=danishlc)
 plot(Cases ~ City, data=danishlc)
 plot(Cases ~ Age, data=danishlc)  # no se encuentra correctamente ordenado
 plot(Cases/Pop ~ Age, data=danishlc)  # no se encuentra correctamente ordenado
 
 danishlc$Age <- ordered(danishlc$Age,
                         levels=c("40-54", "55-59", "60-64",
                                  "65-69", "70-74", ">74") )
 
 plot(Cases/Pop ~ Age, data=danishlc)  # Ahora si se encuentra correctamente ordenado
 
 # se agrega la tasa de enfermos en la data por 1000 habitantes
 danishlc$Rate <- danishlc$Cases / danishlc$Pop * 1000
 interaction.plot(danishlc$Age, danishlc$City, danishlc$Rate, col=1:4, lwd=2,
                  xlab="Age", ylab="Rate", trace.label = "City")
 
 ##############################################################
 ## Ajuste de modelo GLM Poisson
 
 m1 <- glm( Cases ~ offset( log(Pop) ) + City + Age,
            family=poisson, data=danishlc)
 summary(m1)
 anova(m1, test="Chisq")
 drop1(m1, test="Chisq") 
 step.m1=step(m1)        #Deviance y AIC recomiendan remover City
 summary(step.m1)
 
 ## Bondad de Ajuste
 names(step.m1)
 (step.m1$null.deviance-deviance(step.m1))/step.m1$null.deviance # Pseudo-R^2
 
 (D=deviance(step.m1))  # Deviance del modelo
 (X2=sum(residuals(step.m1,"pearson")^2))  # Estad?stico de X2-Pearson
 
 1-pchisq(D, step.m1$df.residual)  # p-valor Deviance
 1-pchisq(X2, step.m1$df.residual) # p-valor X2-Pearson  
 # En ambos no se rechaza H0 pero estan bien al limite  
 
 ## Analisis gr?fico de residuos de Pearson
 
 plot(step.m1) # El modelo parece ser adecuado
 
 # Analisis individual de distancia de Cook y Leverages
 plot( cooks.distance(step.m1), type="h", las=1, ylab="Distancia de Cook")
 abline(h=4/n, lty=2)
 plot( hatvalues(step.m1), type="h", las=1, ylab="Working Leverages", ylim=c(0,0.6))
 abline(h=2*(n-step.m1$df.residual)/n, lty=2)
 
 # Grafica de correlacion de los residuos
 rP=rstandard(step.m1,type="pearson")
 plot(rP)  
 plot(rP[-1]~rP[-n])  
 abline(lm(rP[-1]~rP[-n]), col=2)
 
 ## Los residuos se comportan bien  
 
 #################################################################
 ## Ajuste sin offset
 
 danishlc$Rate
 
 m2 <- glm( Rate ~ Age,  family=poisson, data=danishlc)  # se producen warning por no ser entero (no es apropiado)
 m3 <- glm( Cases ~ Age,  family=poisson, data=danishlc) # no considera la poblacion total   
 m4 <- glm( Cases ~ Pop+Age,  family=poisson, data=danishlc)  # Estima un par?metro para la poblaci?n
 
 summary(m2) # mal especificado
 
 summary(m4)
 drop1(m4, test="Chisq")
 
 summary(m3)
 drop1(m3, test="Chisq")
 
 deviance(step.m1)
 deviance(m2)
 deviance(m3)
 deviance(m4)
 
 AIC(step.m1)
 AIC(m2)
 AIC(m3)
 AIC(m4)
 
 ################################################################
 ## Predicciones
 
 new=data.frame(Age=levels(danishlc$Age), Pop=5000)
 
 data.frame(Age=new$Age,
            m1=predict(step.m1, newdata=new),
            m2=predict(m2, newdata=new),
            m3=predict(m3, newdata=new),
            m4=predict(m4, newdata=new) ) # prediccion de link
 
 data.frame(Age=new$Age,
            m1=predict(step.m1, newdata=new, "response"),
            m2=predict(m2, newdata=new, "response"),
            m3=predict(m3, newdata=new, "response"),
            m4=predict(m4, newdata=new, "response") ) # prediccion de la media
 
 
 data.frame(Age=new$Age,
            m1=predict(step.m1, newdata=new, "response")/new$Pop,
            m2=predict(m2, newdata=new, "response")/new$Pop,
            m3=predict(m3, newdata=new, "response")/new$Pop,
            m4=predict(m4, newdata=new, "response")/new$Pop ) # prediccion de la proporcion
 
 # De todos estos modelos el mejor especificado es el modelo 1
 
 # Interpretaciones del Riesgo Relativo
 
 coef(step.m1)
 coef(m3)
 coef(m4)
 
 exp(coef(step.m1)) 
 exp(coef(m3)) 
 exp(coef(m4))
 
 ############################################################################
 ## Ejemplo Gamma 
 
 ##################################
 ## Cargar los datos
 
 data(motorins, package = "faraway")
 attach(motorins)
 ?motorins
 
 ###############################
 ### Descripcion
 
 names(motorins)
 summary(motorins)
 hist(motorins$Payment)
 
 plot(motorins[,5:8])
 plot(cbind(log(Payment), log(Claims)) )
 plot(cbind(log(Payment), log(Insured)) )
 
 boxplot(Payment~Kilometres)
 boxplot(Payment~Zone)
 boxplot(Payment~Bonus)
 boxplot(Payment~Make)
 
 table(Zone,Kilometres)
 
tapply(Payment, Zone, sd)/tapply(Payment, Zone, mean)
 
 split(Payment, list(Zone,Kilometres))
 
 y=log(sapply(split(Payment, list(Zone,Kilometres)), var))
 x=log(sapply(split(Payment, list(Zone,Kilometres)), mean))
 
 plot(y~x, pch=21, bg="blue")
 summary(lm(y~x))
 abline(lm(y~x), col=2, lwd=2)
 abline(a=lm(y-2*x~1)$coef, b=2, col=3, lty=2, lwd=2)
 
 2.22114-qt(0.975, 33)*0.03581
 2.22114+qt(0.975, 33)*0.03581
 2*pt(-abs(2.22114-2)/0.03581,33)
 
 # No hay evidencia para rechazar una espicificacion gamma o lognormal
 
 #######################################
 ### Ajustar GLM-Gamma y Log-normal
 
 gamma1=glm(Payment~log(Insured)+log(Claims)+Make, family=Gamma(link="log") )
 summary(gamma1)
 anova(gamma1, test="LRT")
 AIC(gamma1)

 
 ## Calculo de la Deviance Residual
 deviance(gamma1)

 ## Calculo de la Deviance Residual del modelo Nulo
 anova(gamma1)[1,4]

 ## Pseudo R^2
 1-deviance(gamma1)/anova(gamma1)[1,4]     

 
 ##################################
 ### Diagnostico
 
 plot(gamma1)   # hay problemas de Homocedasticidad
 
 # Diferentes tipos residuos raw
 
 residuals(gamma1) # residuos de deviance
 residuals(gamma1, type="pearson")  # residuos de Pearson
 residuals(gamma1, type="working")  # residuos de trabajo
 residuals(gamma1, type="response") # residuos de respuesta
 
 rstandard(gamma1) # residuos de deviance standarizados
 rstandard(gamma1, type="pearson") # residuos de Pearson standarizados
 
 # graficar residuos stand. deviance o pearson vs respuesta
 # graficar residuos stand. deviance o pearson vs link
 # graficar residuos respuesta vs link
 
 ##################################
 ### Prediccion (ajuste)
 
 pred1.gam1=predict(gamma1) # prediccion de el link
 pred2.gam1=predict(gamma1, type="response")
 
 hist(pred1.gam1)
 hist(pred2.gam1)
 hist(Payment)
 plot(pred2.gam1~Payment, pch=20)
 abline(a=0, b=1, lty=2)
 
 cor(pred2.gam1,Payment)^2
 
 