# Alumno: Rodrigo Jeldres Carrasco

library(faraway)
data(prostate)

prostate$svi <- factor(prostate$svi)
prostate$gleason <- factor(prostate$gleason)

head(prostate)
?prostate

# se separa el conjunto de datos
set.seed(1)
id=sample.int(nrow(prostate), ceiling(nrow(prostate)*0.9))

prostate.train <- prostate[id,]
prostate.test <- prostate[-id,]

# resumen de los datos
summary(prostate)
summary(prostate.train)
summary(prostate.test)

table(prostate$svi)/nrow(prostate)*100 # todos
table(prostate.train$svi)/nrow(prostate.train)*100 # entrenamiento
table(prostate.test$svi)/nrow(prostate.test)*100 # test


# 1 Realice un analisis de inflacion de varianzas (VIF) y de ser necesario 
#   elimine todas aquellas necesarias de modo de garantizar un vif menor a 8.
names(prostate)
faraway::vif(prostate.train[,-5]) #se le quita la variable respuesta para el analisis
#factor de varianzas
#Resp: Son todos menores a 8, no se elimina ninguna covariable
# lcavol  lweight      age     lbph      lcp  gleason    pgg45     lpsa 
# 2.906170 1.451973 1.317708 1.361815 2.481346 2.272620 2.787296 2.503384 

# 2  Utilizando la data ‘prostate.train’ realice una grafica apropiada entre la variable respuesta y 
# cada una de las covariables. Segun esta perpespectiva grafica, 
# ¿Existe alguna de ellas que pueda explicar la variable respuesta?

library(ggplot2)

#analisis descriptivo para la lcavol
ggplot(prostate.train, aes(x=svi, y=lcavol, fill=svi))+
  geom_violin(alpha=0.5)+
  geom_boxplot(fill=c("lightsalmon","skyblue"), alpha=0.7, width=0.1)
#graficamente la caja con '1' está más arriba que la caja con '0'
#parece ser que lcavol influye en la variable respuesta


#analisis descriptivo para la lweight
ggplot(prostate.train, aes(x=svi, y=lweight, fill=svi))+
  geom_violin(alpha=0.5)+
  geom_boxplot(fill=c("lightsalmon","skyblue"), alpha=0.7, width=0.1)
#graficamente , las graficas de las cajas están casi al mismo nivel
#parece ser que la variable lweight no influye en la variable respuesta

#analisis descriptivo para la age
ggplot(prostate.train, aes(x=svi, y=age, fill=svi))+
  geom_violin(alpha=0.5)+
  geom_boxplot(fill=c("lightsalmon","skyblue"), alpha=0.7, width=0.1)
#graficamente , las graficas de las cajas están casi al mismo nivel
#parece ser que la variable age no influye en la variable respuesta

#analisis descriptivo para lbph
ggplot(prostate.train, aes(x=svi, y=lbph, fill=svi))+
  geom_violin(alpha=0.5)+
  geom_boxplot(fill=c("lightsalmon","skyblue"), alpha=0.7, width=0.1)
#graficamente la caja con '1' está un poco más abajo que la caja con '0'
#parece ser que lbph influye en la variable respuesta

#analisis descriptivo para lcp
ggplot(prostate.train, aes(x=svi, y=lcp, fill=svi))+
  geom_violin(alpha=0.5)+
  geom_boxplot(fill=c("lightsalmon","skyblue"), alpha=0.7, width=0.1)
#graficamente la caja con '1' está más arriba que la caja con '0'
#parece ser que lcp influye en la variable respuesta

#analisis descriptivo para gleason
ggplot(prostate.train, aes(x=svi, y=gleason, fill=svi))+
  geom_violin(alpha=0.5)+
  geom_boxplot(fill=c("lightsalmon","skyblue"), alpha=0.7, width=0.1)
#graficamente la caja con '1' está más arriba que la caja con '0'
#parece ser que gleason influye en la variable respuesta

#analisis descriptivo para pgg45
ggplot(prostate.train, aes(x=svi, y=pgg45, fill=svi))+
  geom_violin(alpha=0.5)+
  geom_boxplot(fill=c("lightsalmon","skyblue"), alpha=0.7, width=0.1)
#graficamente la caja con '1' está más arriba que la caja con '0'
#parece ser que pgg45 influye en la variable respuesta

#analisis descriptivo para lpsa
ggplot(prostate.train, aes(x=svi, y=lpsa, fill=svi))+
  geom_violin(alpha=0.5)+
  geom_boxplot(fill=c("lightsalmon","skyblue"), alpha=0.7, width=0.1)
#graficamente la caja con '1' está más arriba que la caja con '0'
#parece ser que lpsa influye en la variable respuesta

#Resp: las covariables que graficamente indican una tendencia, es decir
#que podrían incidir en la variable respuesta son: lcavol, lbph, lcp, gleason, pgg45, lpsa

# 3 Utilizando el criterios de Akaike (AIC), la metodologıa stepwise (puede ser backward, forward o both,
# indique explıcitamente cual utilizara) y la funcion de enlace ‘logit’ para determinar 
# el modelo de regresion logıstica que mejor ajusta a la variable respuesta.

# Resp: se usará stepwise con backward
full_model_logit <- glm(svi ~ . , data = prostate.train, family = binomial(link = "logit")) #calcula con todas las covariables

# metodo backward stepwise para la selección de variables
stepwise_model_logit <- step(full_model_logit, direction = "backward")

# Resumen del modelo final
summary(stepwise_model_logit)

#resumen del AIC
AIC(stepwise_model_logit) # AIC = 43.50773

# modelo = svi ~ lcp + lpsa

#Resumen de los coeficientes
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  -8.1044     2.3739  -3.414 0.000640 ***
#   lcp           1.2931     0.3911   3.306 0.000946 ***
#   lpsa          2.0844     0.7166   2.909 0.003630 ** 
# todos los coeficientes son significativos, Con p-value menores a un alfa = 0.05

# 4 Analice la significancia del modelo obtenido luego del proceso de seleccion, y responda si

# a)  ¿Es el modelo obtenido significativo? 

# Resumen del modelo final
summary(stepwise_model_logit)

#Resumen de los coeficientes
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  -8.1044     2.3739  -3.414 0.000640 ***
#   lcp           1.2931     0.3911   3.306 0.000946 ***
#   lpsa          2.0844     0.7166   2.909 0.003630 ** 
# todos los coeficientes son significativos, Con p-value menores a un alfa = 0.05

# Prueba de razon de verosimilitud entre el modelo nulo y el modelo ajustado
null_model_logit <- glm(svi ~ 1, data = prostate.train, family = binomial(link = "logit"))

# Comparación de modelos anidados
# H0: el modelo menor es adecuado vs H1 modelo con más variables es más adecuado
anova(null_model_logit, stepwise_model_logit, test = "Chisq")
qchisq(0.95, 2) # 5.991465  , deviance obtenido es 54.308 , se rechaza H0
1-pchisq(54, 10) #Probabilidad = 0  (4.852262e-08)

# Model 1: svi ~ 1
# Model 2: svi ~ lcp + lpsa
# Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
# 1        87     91.816                          
# 2        85     37.508  2   54.308 1.611e-12 ***
# para p-value = 1.611e-12 < alfa = 0.05 , se rechaza H0 , indica que H1 explica mejor que modelo nulo

PseudoR2(stepwise_model_logit) # 0.7109639

AIC(null_model_logit, stepwise_model_logit) #El menor AIC es stepwise_model 43.50773
BIC(null_model_logit, stepwise_model_logit) #El mejor modelo es modelo obtenido con BIC de 50.93974

# Resp: el modelo svi ~ lcp + lpsa es significativo

#b) ¿Existe alguna covariable no significativa?

# Modelelo obtenido anteriormente: svi ~ lcp + lpsa
summary(stepwise_model_logit)
# Resp: No, no existe ninguna covariable no significativa en el modelo ajustado stepwise_model. 
# Todos los coeficientes son significativos, ya que los valores de p son menores que 0.05. 
# En particular, los p-value son:
  
  # (Intercept): 0.000640
  #   lcp: 0.000946
  #   lpsa: 0.003630
  # Esto indica que todas las variables predictoras en el modelo tienen un efecto significativo 
  # sobre la variable respuesta svi para un alfa = 0.05
  
# c) ¿En caso de existir alguna covariable no significativa, la quitaria del modelo?. Fundamente.
# Resp: por el analisis anterior, tanto de anova , como de R2 , los p-value de 
# las covariables, se observa que todas las covariables sí son significativas para la variable explicativa svi  , 
# por lo tanto no eliminaria ninguna

# d) Utilice los odd-ratios para interprete las variables del modelo fi nal

# Ajustar el modelo
mi_modelo <- glm(svi ~ lcp + lpsa, data = prostate.train, family = binomial(link = "logit"))

# Calcular los odds ratios
odds_ratios <- exp(coef(mi_modelo))
odds_ratios

# (Intercept)          lcp         lpsa 
# 0.0003022039 3.6441505395 8.0394821861

#Resp: 
# lcp: Un odds ratio de 3.64 significa que por cada unidad de aumento en lcp, 
# las probabilidades de que svi sea igual a 1 aumentan aproximadamente 3.64 veces

# lpsa: Un odds ratio de 8.04 significa que por cada unidad de aumento en lpsa, 
# las probabilidades de que svi sea igual a 1 aumentan aproximadamente 8.04 vece

# 5. Repita el proceso de las preguntas 3 y 4 con la funcion de enlace ‘probit’.

# 5.3 usando funcion de enlace probit

# Resp: se usará stepwise con backward
full_model_probit <- glm(svi ~ . , data = prostate.train, family = binomial(link = "probit")) #calcula con todas las covariables

# metodo backward stepwise para la selección de variables
stepwise_model_probit <- step(full_model_probit, direction = "backward")

# Resumen del modelo final
summary(stepwise_model_probit)

#resumen del AIC
AIC(stepwise_model_probit) #AIC = 42.79557

#MODELO = svi ~ lcp + lpsa

# Resumen de los coeficientes
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  -4.6431     1.2628  -3.677 0.000236 ***
#   lcp           0.7247     0.2075   3.493 0.000478 ***
#   lpsa          1.1888     0.3868   3.073 0.002116 ** 

# todos los coeficientes son significativos, Con p-value menores a un alfa = 0.05


#  (a) ¿Es el modelo obtenido significativo?

# Resumen del modelo final
summary(stepwise_model_probit)

#Resumen de los coeficientes
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  -4.6431     1.2628  -3.677 0.000236 ***
#   lcp           0.7247     0.2075   3.493 0.000478 ***
#   lpsa          1.1888     0.3868   3.073 0.002116 ** 
# todos los coeficientes son significativos, Con p-value menores a un alfa = 0.05

# Prueba de razon de verosimilitud entre el modelo nulo y el modelo ajustado
null_model_probit <- glm(svi ~ 1, data = prostate.train, family = binomial(link = "probit"))

# Comparación de modelos anidados
# H0: el modelo menor es adecuado vs H1 modelo con más variables es más adecuado
anova(null_model_probit, stepwise_model_probit, test = "Chisq") #deviance obtenido es 55.02 con 
# p-value 1.128e-12 < alfa = 0.05 , se rechaza H0 , modelo con más variables explica mejor
qchisq(0.95, 2) # 5.991465  Deviance = 55.02 es mayor a 5.99
1-pchisq(55, 10) #Probabilidad = 0  (4.852262e-08)

# Model 1: svi ~ 1
# Model 2: svi ~ lcp + lpsa
# Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
# 1        87     91.816                          
# 2        85     36.796  2    55.02 1.128e-12 ***
# para p-value = 1.128e-12 < alfa = 0.05 , se rechaza H0 , indica que H1 explica mejor que modelo nulo

PseudoR2(stepwise_model_probit) # 0.717677

AIC(null_model_probit, stepwise_model_probit) #El menor AIC es stepwise_model_probit 42.79557
BIC(null_model_probit, stepwise_model_probit) #El mejor modelo es stepwise_model_probit obtenido con BIC de 50.22758

# Resp: el modelo svi ~ lcp + lpsa es significativo


#  (b) ¿Existe alguna covariable no significativa?


# Modelelo obtenido anteriormente: svi ~ lcp + lpsa
summary(stepwise_model_probit)
# Resp: no existe ninguna covariable no significativa en el modelo ajustado stepwise_model_probit 
# Todos los coeficientes son significativos, ya que los valores de p son menores que 0.05. 
# En particular, los p-value son:

# (Intercept): 0.000236
#   lcp: 0.000478
#   lpsa: 0.002116
# Esto indica que todas las variables predictoras en el modelo tienen un efecto significativo 
# sobre la variable respuesta svi para un alfa = 0.05

# c) ¿En caso de existir alguna covariable no significativa, la quitaria del modelo?. Fundamente.
# Resp: por el analisis anterior, tanto de anova , como de R2 , los p-value de 
# las covariables, se observa que todas las covariables sí son significativas para la variable explicativa svi  , 
# por lo tanto no eliminaria ninguna

#6. Utilice algun criterio apropiado para definir cual de los dos modelos finales (logit o probit) es el mejor.

AIC(stepwise_model_logit, stepwise_model_probit)
# df      AIC
# stepwise_model_logit   3 43.50773
# stepwise_model_probit  3 42.79557

#Resp: mejor modelo por AIC 42.79557 , Modelo usando probit

BIC(stepwise_model_logit, stepwise_model_probit)
# df      BIC
# stepwise_model_logit   3 50.93974
# stepwise_model_probit  3 50.22758

#Resp: mejor modelo por AIC 50.22758 , Modelo usando probit

#Tanto con AIC como BIC, mejor modelo es con probit

# 7. Realice la prediccion para los datos de la muestra ‘prostate.test’ con ambos modelos. Incluya un intervalo
# de confianza para las predicciones.

# Ajustar el modelo logit para prostate.train
logit_model <- glm(svi ~ lcp + lpsa, data = prostate.train, family = binomial(link = "logit"))

# Ajustar el modelo probit para prostate.train
probit_model <- glm(svi ~ lcp + lpsa, data = prostate.train, family = binomial(link = "probit"))

#Predicciones con el modelo logit
logit_predictions <- predict(logit_model, newdata = prostate.test, type = "response", se.fit = TRUE)

#Predicciones con el modelo probit
probit_predictions <- predict(probit_model, newdata = prostate.test, type = "response", se.fit = TRUE)

#Intervalos de Confianza Para el modelo logit
logit_upper <- logit_predictions$fit + qnorm(0.975) * logit_predictions$se.fit
logit_lower <- logit_predictions$fit - qnorm(0.975) * logit_predictions$se.fit

logit_intervals <- data.frame(Prediction = logit_predictions$fit, 
                              Lower = logit_lower, 
                              Upper = logit_upper)
logit_intervals
# > logit_intervals
# Prediction         Lower        Upper
# 4  3.586176e-05 -0.0001569708 0.0002286943
# 5  1.091601e-04 -0.0004013407 0.0006196609
# 8  2.985933e-04 -0.0009131889 0.0015103755
# 15 3.185303e-03 -0.0061303216 0.0125009272
# 31 5.904365e-03 -0.0085686601 0.0203773891
# 63 4.646543e-01  0.2130750374 0.7162335122
# 75 7.670447e-01  0.5441821869 0.9899072020
# 76 8.789048e-01  0.7093884033 1.0484212243
# 78 6.088169e-02 -0.0419904621 0.1637538365


# Intervalos de Confianza Para el modelo probit
probit_upper <- probit_predictions$fit + qnorm(0.975) * probit_predictions$se.fit
probit_lower <- probit_predictions$fit - qnorm(0.975) * probit_predictions$se.fit

probit_intervals <- data.frame(Prediction = probit_predictions$fit, 
                               Lower = probit_lower, 
                               Upper = probit_upper)

probit_intervals
# > probit_intervals
# Prediction         Lower        Upper
# 4  2.596271e-09 -4.138874e-08 4.658129e-08
# 5  9.649642e-08 -1.174495e-06 1.367488e-06
# 8  1.811311e-06 -1.668894e-05 2.031156e-05
# 15 4.966225e-04 -2.216881e-03 3.210126e-03
# 31 1.672233e-03 -5.211592e-03 8.556059e-03
# 63 4.516273e-01  2.223657e-01 6.808889e-01
# 75 7.370850e-01  5.101975e-01 9.639725e-01
# 76 8.596217e-01  6.647885e-01 1.054455e+00
# 78 5.897790e-02 -5.480250e-02 1.727583e-01


# 8. Utilice el punto de corte 0.5 para realizar la clasificacion. 
# Reporte las dos matrices de confusion (una de cada modelo). 
# Utilice la exactitud (accuracy) para indicar cual modelo es mejor. 
# ¿El modelo mejor es el mismo indicado en la pregunta 6?

# Clasificación para el modelo logit
logit_classifications <- ifelse(logit_predictions$fit >= 0.5, 1, 0)

# Clasificación para el modelo probit
probit_classifications <- ifelse(probit_predictions$fit >= 0.5, 1, 0)

library(caret)

# Convertir las clasificaciones a factor
logit_classifications_factor <- factor(logit_classifications, levels = c(1, 0))
probit_classifications_factor <- factor(probit_classifications, levels = c(1, 0))

# Matriz de confusión para el modelo logit
logit_conf_matrix <- caret::confusionMatrix(logit_classifications_factor, 
                                            prostate.test$svi,
                                            mode = "everything")

# Matriz de confusión para el modelo probit
probit_conf_matrix <- caret::confusionMatrix(probit_classifications_factor, 
                                             prostate.test$svi,
                                             mode = "everything")


# Mostrar la matriz de confusión y la exactitud para el modelo logit
print("Matriz de Confusión - Modelo Logit")
print(logit_conf_matrix)

# Imprimir exactitud
logit_accuracy <- logit_conf_matrix$overall['Accuracy']
print(paste("Exactitud - Modelo Logit:", logit_accuracy))

# Mostrar la matriz de confusión y la exactitud para el modelo probit
print("Matriz de Confusión - Modelo Probit")
print(probit_conf_matrix)

# Imprimir exactitud
probit_accuracy <- probit_conf_matrix$overall['Accuracy']
print(paste("Exactitud - Modelo Probit:", probit_accuracy))





# calcula el Pseudo R2 de Nagelkerke(1991)
PseudoR2<-function(mod){
  n = nrow(mod$data)
  num = 1 - exp((mod$deviance-mod$null.deviance)/n)
  den = 1 - exp(-mod$null.deviance/n)
  return(num/den)
}





