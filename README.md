# Diplomado en Estadística

Este repositorio contiene el material del "Diplomado en Estadística", utilizando el lenguaje R y el entorno de desarrollo RStudio.

## Descripción

El "Diplomado en Estadística" está diseñado para proporcionar una comprensión profunda de los métodos estadísticos y su aplicación utilizando R. Este repositorio incluye:

- **Clases**: Material y scripts utilizados durante las clases.
- **Ejemplos**: Ejemplos prácticos que ilustran los conceptos enseñados.
- **Ejercicios**: Conjuntos de ejercicios para practicar y reforzar el aprendizaje.

## Instalación

Para utilizar los scripts y materiales de este repositorio, sigue estos pasos:

1. **Clonar el repositorio**:
    ```bash
    git clone https://github.com/rjeldresc/Estadistica.git
    ```

2. **Navegar al directorio del proyecto**:
    ```bash
    cd diplomado_en_estadistica
    ```

3. **Instalar las dependencias necesarias**:
    Abre RStudio y ejecuta:
    ```r
    # Instalar los paquetes necesarios
    install.packages(c("dplyr", "ggplot2", "tidyr"))
    ```

## Uso

Ejemplos de cómo utilizar los scripts y materiales del repositorio.

### Ejemplo de Regresión Lineal

```r
# Cargar los datos
bd <- read.csv("data/datos.csv")

# Ajustar el modelo de regresión
m3 <- lm(bd$PTJE ~ bd$HRS, data = bd)

# Crear el gráfico
plot(bd$HRS, bd$PTJE, xlab = "Horas de Estudio", ylab = "Puntaje", main = "Regresión Lineal: Puntaje vs Horas de Estudio")

# Añadir la línea de regresión
abline(m3, col = "red")
