setwd("d:/dev/Estadistica/")
# Cargar los paquetes necesarios
library(readxl)
library(dplyr)
# Leer la hoja "pearson" completa
bd <- read_excel("galton.xlsx", sheet = "pearson")
# Seleccionar solo las columnas A y B (suponiendo que tienen nombres de columna adecuados)
bd <- bd %>% select(1:2)
bd$Fathers <- bd$Fathers*2.54
bd$Sons <- bd$Sons*2.54
# Leer solo las columnas A y B de la hoja "pearson" del archivo Excel
#bd <- read_excel("galton.xlsx", sheet = "pearson", range = "A:B")
#bd <- readxl::read_excel("galton.xlsx")
plot(bd)
lm(Sons ~ Fathers , data = bd )  # es Y Hijo explicada por X Padre
abline(lm(Sons ~ Fathers , data = bd), col = "red")
