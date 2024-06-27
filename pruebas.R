library(MESS)

power_binom_test(n=900 , p0 = 0.4, pa = 0.45, sig.level = 0.05 , alternative = "greater")
# Datos de ejemplo
datos <- c(5.1, 3.5, 4.0, 4.4, 5.0, 4.6, 3.7, 4.2)

# Realizar el test de Shapiro-Wilk
shapiro.test(datos)


# Datos de ejemplo
grupoA <- c(2.1, 3.5, 4.0, 2.4, 3.0)
grupoB <- c(5.1, 4.5, 6.0, 5.4, 6.0)
grupoC <- c(7.1, 6.5, 8.0, 7.4, 8.0)

# Crear un dataframe
datos <- data.frame(
  valor = c(grupoA, grupoB, grupoC),
  grupo = factor(rep(c("A", "B", "C"), each=5))
)

# Realizar el ANOVA
anova_result <- aov(valor ~ grupo, data = datos)
summary(anova_result)


# Datos de ejemplo
antes <- c(5.1, 3.5, 4.0, 4.4, 5.0, 4.6, 3.7, 4.2)
despues <- c(6.1, 4.5, 5.0, 5.4, 6.0, 5.6, 4.7, 5.2)

# Realizar el test t para muestras apareadas
t.test(antes, despues, paired = TRUE)
