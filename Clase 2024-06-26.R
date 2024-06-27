setwd("D:/dev/Estadistica/")
base1 <- readxl::read_excel("RepasoDIPLO.xlsx", sheet = "PCR")
head(base1)
attach(base1)
table(EXTRANJERO)
prop.test(x= 49, n=112, p=1/3, alternative = "greater")

table(PCR)
prop.test(x = 25 , n = 112 , p = 0.3 , alternative = "less")

var.test(EDAD~EXTRANJERO, alternative = "two.sided")

t.test(EDAD~EXTRANJERO, alternative = "less", var.equals = F)
#p-value = 0.001156 < alpha = 5%

#estimar las edades promedio en años

ex <- base1[EXTRANJERO ==1, "EDAD"]
noex <- base1[EXTRANJERO ==0, "EDAD"]

library(misty)
ci.mean(noex$EDAD)

ci.mean(ex$EDAD)


base2 <- readxl::read_excel("RepasoDIPLO.xlsx", sheet = "Abalon")
attach(base2)

#1 estudio de los datos
par(mfrow = c(1,3))
boxplot(largo~centro)
boxplot(diametro~centro)
boxplot(peso~centro)
par(mfrow = c(1,1))

library(gridExtra)

anova <- aov(diametro~centro)
summary(anova)
# se rechaza H0

#3 SUPUESTOS independencia

plot(anova$residuals)

shapiro.test(anova$residuals)

library(lmtest)
bptest(anova)

#4 comparaciones
TukeyHSD(anova)
plot(TukeyHSD(anova) , las = 1)

base3 <- readxl::read_excel("RepasoDIPLO.xlsx", sheet = "Marketing")
attach(base3)

summary(base3)
par(mfrow = c(1,3))
hist(Youtube)
hist(Facebook)
hist(Periodico)
hist(Ventas)
par(mfrow = c(1,1))

library(fitdistrplus)

gam <- fitdistr(Periodico, "gamma")
