##### Ejemplo con galton.xlsx
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
