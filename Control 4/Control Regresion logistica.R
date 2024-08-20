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


#1. Realice un analisis de inflacion de varianzas (VIF) y de ser necesario elimine todas aquellas necesarias de
#   modo de garantizar un vif menor a 8.

names(prostate) 

faraway::vif(prostate[,-5]) #se le quita la variable respuesta 
# lcavol  lweight      age     lbph      lcp  gleason    pgg45     lpsa 
# 3.081874 1.473906 1.357345 1.369858 2.557869 2.437837 2.988007 2.605324 

# Ajustar un modelo de regresión logística con 'svi' como variable respuesta
initial_model <- glm(svi ~ ., data = prostate, family = binomial(link="logit"))
# Ajustar el modelo de regresión lineal con todas las variables predictoras
linear_model <- glm(svi ~ lcavol + lweight + age + lbph + lcp + gleason + pgg45 + lpsa, data = prostate.train, family = binomial(link="logit"))


# Calcular los VIFs
vif_values <- vif(initial_model)
print(vif_values)
print(vif(initial_model))
# Cargar el paquete car
library(car)

# Calcular los VIF
vif_values <- vif(linear_model)
print(vif_values)


#2

#analisis descriptivo para la lcavol
ggplot(prostate.train, aes(x=svi, y=lcavol, fill=svi))+
  geom_violin(alpha=0.5)+
  geom_boxplot(fill=c("lightsalmon","skyblue"), alpha=0.7, width=0.1)

#analisis descriptivo para la lweight
ggplot(prostate.train, aes(x=svi, y=lweight, fill=svi))+
  geom_violin(alpha=0.5)+
  geom_boxplot(fill=c("lightsalmon","skyblue"), alpha=0.7, width=0.1)

#analisis descriptivo para la age
ggplot(prostate.train, aes(x=svi, y=age, fill=svi))+
  geom_violin(alpha=0.5)+
  geom_boxplot(fill=c("lightsalmon","skyblue"), alpha=0.7, width=0.1)

#analisis descriptivo para lbph
ggplot(prostate.train, aes(x=svi, y=lbph, fill=svi))+
  geom_violin(alpha=0.5)+
  geom_boxplot(fill=c("lightsalmon","skyblue"), alpha=0.7, width=0.1)

#analisis descriptivo para lcp * 
ggplot(prostate.train, aes(x=svi, y=lcp, fill=svi))+
  geom_violin(alpha=0.5)+
  geom_boxplot(fill=c("lightsalmon","skyblue"), alpha=0.7, width=0.1)

#analisis descriptivo para gleason
ggplot(prostate.train, aes(x=svi, y=gleason, fill=svi))+
  geom_violin(alpha=0.5)+
  geom_boxplot(fill=c("lightsalmon","skyblue"), alpha=0.7, width=0.1)

#analisis descriptivo para pgg45 *
ggplot(prostate.train, aes(x=svi, y=pgg45, fill=svi))+
  geom_violin(alpha=0.5)+
  geom_boxplot(fill=c("lightsalmon","skyblue"), alpha=0.7, width=0.1)

#analisis descriptivo para lpsa *
ggplot(prostate.train, aes(x=svi, y=lpsa, fill=svi))+
  geom_violin(alpha=0.5)+
  geom_boxplot(fill=c("lightsalmon","skyblue"), alpha=0.7, width=0.1)

#3


# Ajustar el modelo completo con la función de enlace 'logit'
full_model <- glm(svi ~ ., data = prostate.train, family = binomial(link = "logit")) #calcula con todas las covariables

# Aplicar el método backward stepwise para la selección de variables
stepwise_model <- step(full_model, direction = "backward")

# Resumen del modelo final
summary(stepwise_model)

#resumen del AIC
AIC(stepwise_model)


#Pregunta 4

#a) 


# Resumen del modelo final
summary(stepwise_model)

# Prueba de razón de verosimilitud entre el modelo nulo y el modelo ajustado
null_model <- glm(svi ~ 1, data = prostate.train, family = binomial(link = "logit"))

anova(null_model, stepwise_model, test = "Chisq")
# Model 1: svi ~ 1
# Model 2: svi ~ lcp + lpsa
# Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
# 1        87     91.816                          
# 2        85     37.508  2   54.308 1.611e-12 ***

PseudoR2(stepwise_model) #0.6995633
AIC(null_model, stepwise_model) #EL DE MENOR AIC ES stepwise_model
BIC(null_model, stepwise_model)

#b) ¿Existe alguna covariable no signifi cativa?

summary(stepwise_model)

#todas son significativas aunque el lpsa esta muy cerca de un alfa = 0.05

#lpsa  contraste = 2.909 p-value 0.003630 

mod_sin_lpsa <- glm(svi ~ lcp, data = prostate.train, family = binomial(link = "logit"))

summary(mod_sin_lpsa)

# c)

anova(mod_sin_lpsa, stepwise_model)
AIC(mod_sin_lpsa, stepwise_model) #sigue con bajo aic stepwise_model

#Resp: si bien quité del modelo la covariable lpsa , queda con mayor AIC que el modelo stepwise_model
# por eso mejor la dejo

# d) 

# Resumen del modelo final
model_summary <- summary(stepwise_model)

# Extraer los coeficientes
coefficients <- model_summary$coefficients

# Calcular los odds ratios
odds_ratios <- exp(coefficients[, "Estimate"])

# 
# Intercepto (0.0003022):
#   
#   Este valor indica que cuando todas las covariables son cero, las probabilidades de que la variable respuesta svi = 1 son muy bajas (prácticamente despreciables). Sin embargo, el intercepto no es directamente interpretable en la práctica, ya que no es común que todas las variables sean cero.
# Variable lcp (3.6442):
#   
#   Un odds ratio de 3.6442 significa que por cada unidad adicional en lcp, las probabilidades de que svi = 1 aumentan en un 264.42% (es decir, 
#                                                                                                                                 %
#                                                                                                                                     264.42%).
# Esto sugiere que lcp es un predictor positivo y significativo para la variable respuesta. En otras palabras, a medida que aumenta lcp, la probabilidad de tener la enfermedad (svi = 1) también aumenta considerablemente.
# Variable lpsa (8.0395):
#   
#   Un odds ratio de 8.0395 indica que por cada unidad adicional en lpsa, las probabilidades de que svi = 1 aumentan en un 703.95% (es decir, 
#                                                                                                                           %
#                                                                                                                                   703.95%).
# Esto sugiere que lpsa también es un predictor positivo para svi, y su efecto es muy fuerte.

# pregunta 5

modelo01 <- glm(svi ~ ., data = prostate.train, family = binomial(link = "probit")) 
# Aplicar el método backward stepwise para la selección de variables
stepwise_modelo01 <- step(modelo01, direction = "backward")







#pregunta 














# calcula el Pseudo R2 de Nagelkerke(1991)
PseudoR2<-function(mod){
  n = nrow(mod$data)
  num = 1 - exp((mod$deviance-mod$null.deviance)/n)
  den = 1 - exp(-mod$null.deviance/n)
  return(num/den)
}





