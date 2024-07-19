######### Ejemplo con galton.xlsx
setwd("d:/dev/Estadistica/Bases de datos/")
# Cargar los paquetes necesarios
library(readxl)
library(dplyr)
# Leer la hoja "pearson" completa
bd <- read_excel("galton.xlsx", sheet = "pearson")
# Seleccionar solo las columnas A y B (suponiendo que tienen nombres de columna adecuados)
bd <- bd %>% select(1:2)
bd$Fathers <- bd$Fathers*2.54
bd$Sons <- bd$Sons*2.54
attach(bd)
# Leer solo las columnas A y B de la hoja "pearson" del archivo Excel
#bd <- read_excel("galton.xlsx", sheet = "pearson", range = "A:B")
#bd <- readxl::read_excel("galton.xlsx")
plot(bd)
m1 <- lm(Sons ~ Fathers , data = bd )  # es Y Hijo explicada por X Padre
abline(lm(Sons ~ Fathers , data = bd), col = "red")
summary(m1)
anova(m1)
plot(m1$fitted.values, m1$residuals)
# Calcular el coeficiente de correlación entre las estaturas de los padres y los hijos
correlation <- cor(bd$Fathers, bd$Sons)

# Mostrar el coeficiente de correlación
print(correlation)

# Cargar el paquete ggplot2 para visualización
library(ggplot2)

# Crear un gráfico de dispersión con una línea de regresión
ggplot(bd, aes(x = Fathers, y = Sons)) +
  geom_point() + # Agregar puntos de datos
  geom_smooth(method = "lm", col = "red") + # Agregar línea de regresión
  labs(title = "Relación entre las estaturas de Padres e Hijos",
       x = "Estatura de los Padres (cm)",
       y = "Estatura de los Hijos (cm)")

cor.test(Fathers, Sons)
plot(m1, 2)


##### Ejemplo Con base Galton1.xlsx
setwd("d:/dev/Estadistica/Bases de datos/")
bd <- readxl::read_excel("Galton1.xlsx")
library(readxl)
bd <- read_excel("Galton1.xlsx")
class(bd)
plot(bd)
m1 <- lm(bd$T.Hijo ~ bd$T.Padre , data = bd )
summary(m1)
abline(lm(bd$T.Hijo ~ bd$T.Padre , data = bd), col = "red")
# Calcular el coeficiente de correlación entre las estaturas de los padres y los hijos
correlation <- cor(bd$T.Hijo , bd$T.Padre)

# Mostrar el coeficiente de correlación
print(correlation)

# Ejemplo Con base Datos.xlsx
setwd("d:/dev/Estadistica/Bases de datos/")
library(readxl)
datos <- read_excel("Datos.xlsx")
head(datos)
attach(datos)

#se desea ajustar un modelo de regresion lineal simple para explicar la resistencia
# de una soldadura

# correlacion

library(ggplot2)
ggplot(datos, aes(x=Edad, y=Resistencia)) + 
  geom_point() + theme_light()

#matriz de dispersion
library(GGally)
pairs(datos, upper.panel = panel.smooth , lower.panel = panel.smooth )

correlacion <- cor(datos) #por defecto es pearson (method = 'pearson'), otros posibles kensall y spearman
print(correlacion)

#matriz de correlacion
library(corrplot)
corrplot(correlacion , type = "lower")
corrplot(correlacion , type = "lower", method = "square")
corrplot(correlacion , type = "lower" , method = "number")

# A medida que aumenta la edad de la soldadura, la resistencia que ella ofrece disminuye.
# Este indicador nos muestra la asociación entre las variables pero no la causalidad.


########### Modelo de regresión lineal simple (MRLS) ###########
# El Análisis de Regresión se usa cuando se sabe que existe una relación lineal entre las variables.
#1. Especificación del modelo
#2. Estimación del modelo
#3. Inferencia
#4. Análisis de la varianza
#5. Coeficiente de determinación
#6. Análisis de supuestos: linealidad, homocedasticidad, independencia, norrmalidad


#1.Especificación del modelo 
# El modelo que se va a ajustar es: 
# Resistencia = B0 + B1*Edad
# Supuestos: - los residuos distribuyen normal
#            - los residuos son independientes
#            - los residuos tienen varianza constante (Homocedasticidad)

#2. Estimación del modelo
# Para obtener las estimaciones de los parámetros del modelo anterior se usa el comando lm
mod <- lm(Resistencia ~ Edad, data=datos) #hay que poner el Y y el X
# Resistencia ~ Edad indica que Resistencia es la variable respuesta y que Edad es la variable explicativa
summary(mod)

#resistencia = 2596.856  - 33.556 * Edad

# - Por cada semana que envejezca la soldadura, se espera que la resistencia promedio disminuya en 33.556 psi.
# - Si la soldadura es nueva (Edad=0), se espera que la resistencia promedio sea de 2596.856 psi.

# Podemos obtener, si quisieramos , los IC para los parametros
confint(mod)

# Incluyamos la recta de regresión que representa el modelo ajustado anterior
ggplot(datos, aes(x=Edad, y=Resistencia)) + 
  geom_point() +
  geom_smooth(method='lm', formula=y~x, se=TRUE, col='lightblue') 
# opción se=FALSE o TRUE, muestra el intervalo de confianza de la regresión.


