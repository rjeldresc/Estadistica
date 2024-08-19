library(faraway)
prostate$svi <- factor(prostate$svi)
head(prostate)

# se separa el conjunto de datos
set.seed(1)
id=sample.int(nrow(prostate), ceiling(nrow(prostate)*0.9))

prostate.train <- prostate[id,]
prostate.test <- prostate[-id,]

?prostate
summary(prostate)

summary(prostate.train)
summary(prostate.test)

table(prostate$svi)/nrow(prostate)*100 # todos
table(prostate.train$svi)/nrow(prostate.train)*100 # entrenamiento
table(prostate.test$svi)/nrow(prostate.test)*100 # test


#1. Realice un analisis de inflacion de varianzas (VIF) y de ser necesario elimine todas aquellas necesarias de
#   modo de garantizar un vif menor a 8.

dim(prostate) # 97 x 9

names(prostate) 

faraway::vif(prostate[,-5]) #se le quita la variable respuesta 
# lcavol  lweight      age     lbph      lcp  gleason    pgg45     lpsa 
# 3.081874 1.473906 1.357345 1.369858 2.557869 2.437837 2.988007 2.605324 

# Ajustar un modelo de regresión logística con 'svi' como variable respuesta
initial_model <- glm(svi ~ ., data = prostate, family = binomial(link="logit"))

# Calcular los VIFs
vif_values <- vif(initial_model)
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

PseudoR2(stepwise_model) #0.6995633














































# calcula el Pseudo R2 de Nagelkerke(1991)
PseudoR2<-function(mod){
  n = nrow(mod$data)
  num = 1 - exp((mod$deviance-mod$null.deviance)/n)
  den = 1 - exp(-mod$null.deviance/n)
  return(num/den)
}





