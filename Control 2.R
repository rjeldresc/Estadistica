#alumno: Rodrigo Jeldres

setwd("d:/dev/Estadistica/")

base1 <- readxl::read_excel("control2.xlsx")
head(base1)
attach(base1)
data <- data.frame(base1)

#a)
# H0: mu >= 9 km/lts
# H1: mu < 9 km/lts
t.test(data$Rendimiento, mu = 9, alternative = "less")
#p-value = 0.07927 <  alfa = 0.10
#R: Se rechaza la hipótesis nula, es decir
# se concluye que hay evidencia para afirmar que el rendimiento medio es inferior a 9 km/lts

#B) ¿Se puede afirmar que menos de un tercio de los camiones son de tamaño pequeño?
table(data$Tamaño)
total <- sum(table(data$Tamaño))
print(total) # 32
prop.test( x = 7 , n = 32, p = 1/3, alternative = "less")
#p-value = 0.1175  > alfa = 0.10
#R: no hay suficiente evidencia para rechazar la hipótesis nula H0
#  es decir, no se puede afirmar que menos de un tercio de los camiones son de tamaño pequeño.

# C)
Totalautom_8cil <- sum(data$Cilindro == "8cil" & data$Transm == "autom")
Totalmanual_8cil <- sum(data$Cilindro == "8cil" & data$Transm == "manual")
autom_total <- sum(data$Transm == "autom")
manual_total <- sum(data$Transm == "manual")
prop.test(
  x = c(Totalautom_8cil, Totalmanual_8cil),
  n = c(autom_total, manual_total),  
  alternative = "greater")
#p-value = 0.09424 < alfa = 10%
#Se rechaza H0, hay evidencia para afirmar que la proporción de camiones de 8 cilindros es mayor en vehículos automaticos en comparacion con los vehículos con transmisipn manual.

# d) ¿Existe diferencia significativa respecto a los rendimientos medios según tipo de transmisión?

anova <- aov(Rendimiento ~ Transm, data = data)
summary(anova)
plot(anova,1)
plot(anova$residuals)
par(mfrow = c(2,2))
plot(anova)
#P = 3.66e-05  < ALFA = 10% 
#que hay una diferencia significativa en los rendimientos medios entre al menos dos tipos de transmisión evaluados en tu estudio.

# Comprobación de los supuestos

# - NORMALIDAD
#     H0: residuos distribuyen normales
#     H1: residuos no normales
ks.test(anova$residuals,"pnorm",mean=mean(anova$residuals),sd= sd(anova$residuals))
#p-value = 0.8459
shapiro.test(anova$residual)
#p-value = 0.6955
qqnorm(anova$residuals)
qqline(anova$residuals)

# - HOMOCEDASTICIDAD (varianzas iguales)
plot(fitted(anova), anova$residual)
library(car)
leveneTest(anova$residuals ~ data$Transm)
#P = 0.489

# - INDEPENDENCIA
# H0: residiuos son independientes
# H1: no son independientes
plot(anova$residuals)










# e)

anova_E <- aov(Rendimiento ~ Tamaño, data = data)
summary(anova_E)
#P=9.55e-11
#independencia
plot(anova_E$residuals)

#normalidad 
#H0: Residuos distribuyen normal
shapiro.test(anova_E$residuals) 
#p-value = 0.04701

#homocedasticidad (varianzas iguales)
#H0: varianza iguales
library(lmtest)
bptest(anova_E)
# p-value = 0.2806

#4. Comparaciones
TukeyHSD(anova_E)
plot(TukeyHSD(anova_E))



#f)
tabla_contingencia <- table(data$Transm, data$Tamaño)
tabla_contingencia <- table(data$Tamaño , data$Transm)
chisq.test(tabla_contingencia)
#p-value = 0.006628 < alfa = 10%



