library(faraway)
library(car) 

prostate$svi <- factor(prostate$svi)
head(prostate)
?prostate
summary(prostate)

library(caret)
?train

# 90 - 10 train-test

# Determinar el número de observaciones
n <- nrow(prostate) # n = 97

indices <- sample.int(n, floor(0.9 * n))  # da 87.3 , con floor queda en 87

prostate.train <- prostate[indices, ]
prostate.test <- prostate[-indices, ]

summary(prostate.train)
summary(prostate.test)


table(prostate$svi)/nrow(prostate)*100 # todos
table(prostate.train$svi)/nrow(prostate.train)*100 # entrenamiento
table(prostate.test$svi)/nrow(prostate.test)*100 # test


#1. Realice un an´alisis de infl aci´on de varianzas (VIF) y de ser necesario elimine todas aquellas necesarias de
#   modo de garantizar un vif menor a 8.


