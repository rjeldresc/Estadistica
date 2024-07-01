####### REPASO CONTROL 2 #######

# Paso 1: Desadjuntar todas las instancias de 'base1'
while("base1" %in% search()) {
  detach("base1")
}

# Verificar que ya no hay 'base1' adjuntado
search()

setwd("d:/dev/Estadistica/")

#------------------------------------
base1 <- readxl::read_excel("RepasoDIPLO.xlsx", sheet="PCR")
head(base1)
attach(base1)
data <- data.frame(base1)
data$EXTRANJERO <- as.factor(data$EXTRANJERO)


###PCR
#Taller_13

#a) ¿Más de 1/3 de los pacientes que concurren son extranjeros residentes?
# 1. Parametro: P=proporcion de extranjeros
# 2. H0: P <= 1/3
#    H1: P > 1/3
table(EXTRANJERO)
total <- sum(table(EXTRANJERO))
print(total) # 112
prop.test( x = 49 , n = 112, p = 1/3, alternative = "greater")
# p-value = 0.0126 < alpha=5%
# Se rechaza H0, Hay más de 1/3.
# Podemos concluir que hay evidencia suficiente para afirmar que más de un 
# tercio de los pacientes que concurren son extranjeros residentes.

#b) ¿Es posible afirmar que la positividad es inferior a un 30%?
# 1. Parametro: P = proporcion de positividad
# 2. H0: P >= 0.3
#    H1: P < 0.3
table(PCR)
total <- sum(table(PCR))
print(total) # 112
prop.test( x = 25, n = 112, p = 0.3 , alternative = "less")
# p-value = 0.04744 < alpha=5%
# Se rechaza H0. La positividad es inferior a un 30%.

#c)	¿existe evidencia que permita afirmar que los extranjeros que concurren al 
#   CESFAM son de mayor edad en comparación con los no extranjeros?
# Parametro: μ_1= edad promedio en no extranjeros
#            μ_2= edad promedio en extranjeros
# H0:  μ_1 ≥ μ_2
# H1:  μ_1 < μ_2 (no extranjeros son de menor edad que los extranjeros)

# Parametro:  Var1 = varianza de la edad en no extranjeros
#              Var2 = varianza de la edad en extranjeros
# H0: var1 = var2
# H1: var1 ≠  var2
var.test(EDAD ~ EXTRANJERO, alternative = "two.sided" )
# p-value = 0.01362 < alpha=5% se rechaza h0, entonces var.equal= FALSE
t.test(data$EDAD ~ data$EXTRANJERO ,data = data, var.equal= FALSE, alternative = "less")
t.test( EDAD~EXTRANJERO , alternative = "less" , var.equal= FALSE) #var.equal va el resultado anterior
# p-value = 0.001156 < alpha=5% 
# Se rechaza H0, extranjeros con de mayor edad que los nos extranjeros

# Estimar las edades promedio (en años)
Ex <- base1[EXTRANJERO==1,"EDAD"] #caso extranjeros
Noex <- base1[EXTRANJERO==0,"EDAD"] #caso no extranjeros

library(misty)
ci.mean(Noex$EDAD) #44.02   49.19
ci.mean(Ex$EDAD)   #49.69   53.86

#------------------------------------
base2 <- readxl::read_excel("RepasoDIPLO.xlsx", sheet="Abalon")
attach(base2) 

### ABALON
#Taller_14

# Mediante un análisis de la varianza debe determinar si las medias respectivas de los diametros difieren 
# (usar alfa = 5%) y si es así, usando Tukey determine cual o cuales difieren. 

#1. Estudio de los datos muestrales
par(mfrow = c(1,3))
boxplot(largo~centro)
boxplot(diametro~centro)
boxplot(peso~centro)
par(mfrow = c(1,1))

library(ggplot2)
graf1 <- ggplot(base2, aes(x = centro, y = largo)) +
  geom_boxplot()
graf2 <- ggplot(base2, aes(x = centro, y = diametro)) +
  geom_boxplot()
graf3 <- ggplot(base2, aes(x = centro, y = peso)) +
  geom_boxplot()

library(gridExtra)
grid.arrange(graf1,graf2,graf3, ncol = 3)

#2. ANOVA
# Parámetros: μ_A = diámetro promedio del abalon en centro A
#             μ_B = diámetro promedio del abalon en centro B
#             μ_C = diámetro promedio del abalon en centro C
#H0: μ_A=μ_B=μ_C
#H1: alguna media es diferente
anova <- aov(diametro~centro)
summary(anova)
# p-valor = 0.0121 < alpha=5%.
# Se rechaza H0.

#3. Supuestos
#independencia
plot(anova$residuals)

#normalidad 
#H0: Residuos distribuyen normal
shapiro.test(anova$residuals) #Se usa para tamaños de muestras menor a 50 datos
#p-value = 0.1021

#homocedasticidad (varianzas iguales)
#H0: varianza iguales
library(lmtest)
bptest(anova)
#p-value = 0.2272

#4. Comparaciones
TukeyHSD(anova)
plot(TukeyHSD(anova), las=1)
#Entre los centros C y A hay diferencias en las medias de los diametros y es menor en el C.
#es menor en centro C

#------------------------------------
base3 <- readxl::read_excel("RepasoDIPLO.xlsx", sheet="Marketing")
attach(base3)

### MARKETING
#Taller_12

# a) Analisis descriptivo y grafico de las inversiones
summary(base3)

par(mfrow = c(1,3)) 
hist(Youtube,freq=FALSE)
hist(Facebook,freq=FALSE)
hist(Periodico,freq=FALSE)
par(mfrow = c(1,1))

par(mfrow = c(1,3))
boxplot(Youtube, main="Youtube", horizontal=TRUE)
boxplot(Facebook, main="Facebook",horizontal=TRUE)
boxplot(Periodico, main="Periodico",horizontal=TRUE)
par(mfrow = c(1,1))

g1<- ggplot(base3, aes(x = Youtube)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "skyblue", color = "blue")
g2<- ggplot(base3, aes(x = Facebook)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "skyblue", color = "blue")
g3<- ggplot(base3, aes(x = Periodico)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "skyblue", color = "blue")
grid.arrange(g1,g2,g3, ncol = 3)

g4<- ggplot(base3, aes(x = Youtube)) +
  geom_boxplot()
g5<- ggplot(base3, aes(x = Facebook)) +
  geom_boxplot()
g6<- ggplot(base3, aes(x = Periodico)) +
  geom_boxplot()
grid.arrange(g4,g5,g6, ncol = 3)

# b)	¿Existe evidencia para afirmar que las empresas con menos de 16 mil unidades vendidas 
# presentan una menor inversión en publicidad en Periódicos que las empresas con 16 mil unidades 
# o más vendidas? (use alfa=5%)

menores <- base3[Ventas<16, ] 
mayores <- base3[Ventas >=16, ] 

var.test(menores$Periodico,mayores$Periodico, alternative="two.sided")
#p-value = 0.3578. Vaianzas iguales
t.test(menores$Periodico,mayores$Periodico, alternative="less",var.equal = TRUE)
#p-value = 0.02234

# c) ¿La inversión publicitaria en Periódico distribuye Gamma? 
# (indicación: utilice estimador de EMV y test KS para un nivel de significancia del 10%)
library(fitdistrplus)
gam <- fitdist(Periodico, "gamma", method="mle")
parametro1 <- gam$estimate[1]   
parametro2 <- gam$estimate[2]

#H0: Periodico ditribuye gamma
ks.test(Periodico,"pgamma",parametro1, parametro2)
#p-value = 0.6604 > alpha, entonces distribuye gamma

hist(Periodico, freq=FALSE, ylim=c(0,0.03))
curve(dgamma(x, parametro1, parametro2),col = "red", lwd = 2, add = TRUE)

ggplot(base3, aes(x = Periodico)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "skyblue", color = "blue") +
  geom_function(fun = dgamma, args = list(shape = parametro1, rate = parametro2), aes(color = "Curva Gamma")) 

#d)	Determine la probabilidad que una empresa invierta menos de 40 mil dólares en Periódico (probabilidad teórica y empírica)
#X = inversión en Periódico para empresas con venta menor a 16 mil unidades

pgamma(40,parametro1, parametro2)

per <- base3[Periodico<40,] 
nrow(per)/200

#------------------------------------
base4 <- readxl::read_excel("RepasoDIPLO.xlsx", sheet="Vehículos")
attach(base4) 

### VEHÍCULOS
# H0: las dos variables son independientes   
# H1: las variables están asociadas

tabla <- table(Tipo,Procedencia) 
chisq.test(tabla)
#p-value = 0.2522.No se rechaza H0, enronces la evidencia indica independencia entre las dos variables