#### ANALISIS FACTORIAL ####

#PASOS:
#0. Cargar y verificar los datos
#1. Verificar que la matriz de datos sea factorizable
#2. Determinar el número correcto de factores 
#3. Extraer los Factores
#4. Rotar los factores
#5. Interpretar los resultados


# Se realizó una encuesta entre 388 solicitantes del subsidio de cesantía. La encuesta contiene 16 preguntas 
# sobre la satisfacción del cliente
# RUT	– Identificador
# Sexo– 1:Femen 0:Masc
# Total– Nivel de satisfacción con el procedimiento
# P1 	– El proceso cuenta con la privacidad adecuada
# P2 	– He recibido información clara sobre el subsidio
# P3 	– El personal de recepción fue muy amable
# P4 	– Los acuerdos se han respetado
# P5 	– Siento que todos me toman en serio
# P6 	– Mi “asesor” logra motivarme
# P7 	– Mi “asesor” se toma su tiempo conmigo
# P8 	– Mi “asesor” lleva cuidadosamente la entrevista 
# P9 	– Me queda claro cuales son mis derechos
# P10 	– Mi “asesor” señala las oportunidades de trabajo adecuadas
# P11	– Tengo indicaciones claras respecto a lo que sigue
# P12 	– Es fácil encontrar información respecto al subsidio
# P13 	– Mi “asesor” siempre hace lo que promete
# P14 	– Me han informado claramente como continua el proceso
# P15 	– Se quien debe responder mis dudas sobre el subsidio
# P16 	– Toda la información escrita recibida es clara.
# Cada una de las preguntas y el total son evaluadas con notas  de 1 a 7 
# (de muy malo a muy bueno, NA=no responde)
setwd("d:/dev/estadistica/Multivariado/Clase 2024-10-23/")
datos <- readxl::read_excel("Desemp.xlsx")
names(datos)

########################
# Paso 0.
# Quitaremos de la base RUT y la dejaremos como "etiqueta de la fila"
desemp <- data.frame(datos[,-1], row.names = datos$"RUT")
head(desemp)
summary(desemp)
# Tenemos "caracteres" por la presencia de NA - transformamos todas las columnas a numerica
desemp1 <- as.data.frame(apply(desemp,2,as.numeric)) # 2 es para las columnas
# apply() es una función que aplica una función a las filas o columnas de una matriz o data frame
# argumento, 2, indica que la función debe aplicarse a las columnas (si fuera 1, se aplicaría a las filas)
# as.numeric, convierte los valores de cada columna en números
# -> convierte todas las columnas de datos en vectores numéricos
# as.data.frame: Convierte el resultado de apply() en un data frame nuevamente

# Otra opción....
#library(tidyverse)
#desemp1 <- desemp %>% mutate_if(is.character, as.numeric)

# contemos los NA
colSums(is.na(desemp1)) #cuenta el número de datos faltantes (NA) por columna
sum(is.na(desemp1)) #cuenta el número total de datos faltantes (NA)

# quitaremos las filas con NA, pero mantenemos los datos originales
desemp_all <- desemp1 
# eliminamos SEXO y TOTAL (dejamos solo Preg) y los registro con NA
desemp1 <- na.omit(desemp1[,-c(1,2)])

head(desemp1)
nrow(desemp1) # Nos quedamos con 244 filas

# correlaciones
corr <- round(cor(desemp1), 2)
corr    

library(corrplot)  
corrplot(corr)
corrplot(corr, order = "hclust")# organizar con grupos de correlaciones

# ACP sobre matriz de covarianza
ACP <- princomp(desemp1)
summary(ACP)
loadings(ACP)
biplot(ACP)
# Es adecuada cuando las variables están en la misma escala (tienen unidades comparables). 
# Si las variables tienen diferentes escalas, las que tienen mayor varianza pueden dominar el análisis

# ACP sobre matriz de correlacion
ACP <- princomp(desemp1, cor = TRUE)
summary(ACP)
loadings(ACP)
biplot(ACP)
# Es preferido cuando las variables tienen distintas escalas, ya que pone todas las variables en una escala 
# comparable

# graficos
#install.packages("factoextra")
library(factoextra)
fviz_screeplot(ACP, 
               addlabels = TRUE, 
               ylim = c(0,100)) 

fviz_pca_var(ACP, 
             col.var = "contrib", 
             axes = c(1,2))

# No es facil de leer.

########################
#Paso 1: Verificar que la matriz de datos sea factorizable

library(psych)
KMO(desemp1)

#   1 >= KMO >= 0.9 muy bueno
# 0.9 >= KMO >= 0.8 meritorio
# 0.8 >= KMO >= 0.7 bueno
# 0.7 >= KMO >= 0.6 mediocre
# 0.6 >= KMO >= 0.5 bajo 
# 0.5 >= KMO        inaceptable

########################
#Paso 2: Determinar el número correcto de factores 

# H0: k factores son suficientes 
# H1: k factores no son suficientes
factanal(desemp1, factors = 1)  #p-value is 1.67e-46
factanal(desemp1, factors = 2)  #p-value is 6.08e-22
factanal(desemp1, factors = 3)  #p-value is 0.000791
factanal(desemp1, factors = 4)  #p-value is 0.596

#otra alternativa
fa.parallel(desemp1)

########################
#Paso 3 y 4: Extraer los Factores y Rotar los factores

AF<-factanal(desemp1, factors = 4, rotation = "varimax")
loadings(AF) #pesos de las variables por cada factor

## determinar punto de corte para mejorar interpretacion
print(loadings(AF), cutoff = 0.10, sort = TRUE, digits = 3)
print(loadings(AF), cutoff = 0.17, sort = TRUE, digits = 3)
# cutoff: muestra solo las cargas cuyo valor absoluto es mayor que 0.10 o 0.17, 
# sort = TRUE: Ordena las cargas dentro de cada componente para mostrar los valores más grandes primero
# digits = 3: Redondea los valores de las cargas a tres decimales

# otra alternativa
AF2 <- fa(desemp1,nfactors = 4,rotate = "varimax",fm="ml")
fa.diagram(AF2)

########################
#Paso 5:Interpretar los resultados 

## Factor 1: "Mi asesor", su comportamiento
## Factor 2: Claridad de la informacion y del proceso
## Factor 3: Confianza y adecuacion del proceso
## Factor 4: Fiablidad  

# Agrego grafico manual, muy similar al hecho con el ejercicio Ranking mujer
# Grafico manual
plot(AF$loadings[,1], 
     AF$loadings[,2],
     xlab = "Factor 1", 
     ylab = "Factor 2",
     ylim = c(-0.2,1),
     xlim = c(-0.5,1))
text(AF$loadings[,1]-0.03,
     AF$loadings[,2]+0.03,
     colnames(desemp1),
     col="blue",
     cex=0.5)
abline(h = 0, v = 0, lty = 2)

# obtenemos scores de cada rut en cuanto a los 4 factores
ptjes <- factanal(desemp1, factors = 4, rotation = "varimax",
                  scores = "regression") 
# scores = "regression": calcula las puntuaciones factoriales utilizando la regresión. 
# Las puntuaciones factoriales representan los valores de los factores para cada observación.
# Indican la relación de cada variable original con los factores extraídos. 
# Un valor alto sugiere que la variable tiene una fuerte relación con el factor.
ptjes <- ptjes$scores
ptjes

# Regresion lineal y AF - "volvemos a la base original SEXO y TOTAL

# los 4 factores “explican” la valoración total?
Desemp <- merge(desemp_all, ptjes, by = "row.names")
head(Desemp)

fit <- lm(TOTAL ~ Factor1 + Factor2 + Factor3 + Factor4,
          data = Desemp)
summary(fit)

# hay diferencia en el factor "asesor" por sexo?
t.test(Desemp$Factor1 ~ Desemp$SEXO) # No hay diferencia
# hay diferencia en el factor "claridad de la informacion" por sexo?
t.test(Desemp$Factor2 ~ Desemp$SEXO) # Si hay diferencia

