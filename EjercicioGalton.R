# ejercicio clase 2024-07-03

setwd("d:/dev/Estadistica/Bases de datos/")
bd <- readxl::read_excel("Galton1.xlsx")
plot(bd)
m1 <- lm(bd$T.Hijo ~ bd$T.Padre , data = bd ) #variable hijo "Y" explicada por X
abline(lm(bd$T.Hijo ~ bd$T.Padre , data = bd), col = "red")
print(m1)
summary(m1)
anova(m1) # desglose de las sumas cuadradas
?lm
# Coefficients:
#   (Intercept)   bd$T.Padre  
# 84.9550       0.4961 

# Y = 84.9550 + 0.4961 X

#Yi = Beta0 + Beta1*Xi + epsilon

cor(bd$T.Padre, bd$T.Hijo, method ="pearson") #R  coeficiente de correlacion
cor.test(bd$T.Padre, bd$T.Hijo)
?cor
coef_correlacion <- 0.7245802
coef_determinacion <- coef_correlacion^2
print(coef_determinacion)
#El coeficiente de determinación (R²) es aproximadamente 0.525, lo que significa que aproximadamente el 52.5% de la variación en la estatura de los hijos puede explicarse por la variación en la estatura de los padres.

qt(0.025,12)
#-2.178813

plot(bd$T.Padre, bd$T.Hijo, xlab = "Talla Padre", ylab = "Talla Hijo")
tallas <- data.frame(T.Padre = seq(150,200))

#valor medio
ic <- predict(m1, tallas, interval = "confidence")
lines(tallas$T.Padre, ic[,1], lty = 1, col = "black")

#para calcular la probabilidad
pnorm(170, 172.5, 4.202)

#Test
lmtest::bptest(m1)
#BP = 0.034032, df = 1, p-value = 0.8536 > alfa = 0,05
lmtest::dwtest(m1, alt = "t")

setwd("d:/dev/Estadistica/Bases de datos/")
bd <- readxl::read_excel("Regresion1.xlsx", sheet = "Ptje")
cor(bd$NEM, bd$PTJE) #0.8208836
cor(bd$HRS, bd$PTJE) #0.9367613

cor.test(bd$PTJE, bd$NEM)
cor.test(bd$NEM, bd$PTJE)

# modelos simples
lm(bd$PTJE ~ bd$HRS , data = bd )
lm(bd$PTJE ~ bd$NEM , data = bd )

m2 <- lm(bd$PTJE ~ bd$NEM , data = bd )
summary(m2)
#bd$NEM        0.1655     0.0470   3.521   0.0125 *

m3 <- lm(bd$PTJE ~ bd$HRS , data = bd )
summary(m3)
#bd$HRS        3.7427     0.5708   6.557 0.000603 ***
#para el caso de la variable Hrs tiene mayor R2 , Multiple R-squared:  0.8775

plot(bd$HRS, bd$PTJE)
abline(m3, col = "red")

# Test de valores outlier de Bonferroni
car::outlierTest(m3)
# No Studentized residuals with Bonferroni p < 0.05
# Largest |rstudent|:
#   rstudent unadjusted p-value Bonferroni p
# 4 -2.065432           0.093783      0.75026
#dato 4 es posible anomalia 

#sacando el dato 4
bd_sin_4 <- bd[-4, ]
lm(bd_sin_4$PTJE ~ bd_sin_4$HRS , data = bd_sin_4 )

#dato influyente

summary(influence.measures(m3))
# Potentially influential observations of
# lm(formula = bd$PTJE ~ bd$HRS, data = bd) :
#   
#   dfb.1_ dfb.b$HR dffit cov.r   cook.d hat  
# 2  0.03  -0.06    -0.08  2.01_*  0.00   0.29
# 5  0.22  -0.34    -0.39  2.86_*  0.09   0.52
# 6 -0.33   0.29    -0.34  2.51_*  0.07   0.46

