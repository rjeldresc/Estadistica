#### Test de HIPÓTESIS ####

#### Función prop.test ####

# La función prop.test de R se utiliza para probar la hipótesis nula de que las proporciones de dos variables 
# aleatorias independientes X e Y son iguales (prueba de proporción de dos muestras) o 
# para examinar una única proporción frente a un valor hipotético (prueba de proporción de una muestra).

prop.test(x, n, 
          p = NULL,
          alternative = c("two.sided", "less", "greater"),
          conf.level = 0.95, 
          correct = TRUE)

# x: Un vector numérico o una matriz de dos columnas. 
#    En el caso del vector, representa el número de aciertos; en el caso de la matriz, 
#    la primera columna indica el número de aciertos y la segunda, el número de fallos.
# n: El número de ensayos para cada proporción.
# p: Un vector de probabilidades o un único valor de probabilidad bajo la hipótesis nula. 
#    Si no se especifica es p = 0.5.
# alternative: Especifica la hipótesis alternativa H1
#              Los posibles valores son: "two.sided" (cuando H1 es "distinto a"), 
#                                         "less" (para el caso "<"), o 
#                                         "greater" (para el caso ">").
# . Los valores disponibles son "two.sided" cuando la hipótesis alterna es  
# conf.level: Nivel de confianza para el intervalo de confianza devuelto. El valor predeterminado es 0.95.
# correct: Valor lógico que indica si se aplica la corrección de continuidad de Yates. Por defecto es TRUE.

# La función devuelve el estadístico Chi-cuadrado, los grados de libertad, el p-valor, 
# la hipótesis alternativa, el intervalo de confianza y la estimación muestral de la proporción.


#### Ejemplos: 1 variable ####

# EJEMPLO 1: Igual a una proporción
# H0: la proporción de X es p
# H1: la proporción de X no es p

# 107 ensayos, 42 éxitos. 
# ¿Es la proporción igual a 0.6 para un nivel de confianza del 95%?
prop.test(x = 42, n = 107, p = 0.6, conf.level = 0.95)
prop.test(x = 42, n = 107, p = 0.6, conf.level = 0.95,alternative="two.sided")

# El p-valor obtenido (1.851e-05) es mucho menor que los niveles de significación habituales (0.05), 
# lo que indica una fuerte evidencia en contra de la hipótesis nula de que la verdadera proporción 
# es igual a 0.6. Por lo tanto, podemos concluir que la verdadera proporción de éxito es significativamente 
# diferente de 0.6. Además, el límite superior del intervalo de confianza (0.4919223) es menor que la 
# proporción hipotética (0.6).


# EJEMPLO 2: Menor que una proporción
# H0: la proporción de X es p
# H1: la proporción de X es < p

prop.test(x = 42, n = 107, p = 0.6, alternative = "less")

# El p-valor es casi cero, lo que implica que hay pruebas sólidas contra la hipótesis nula. 
# En consecuencia, rechazaríamos la hipótesis nula a favor de la alternativa, concluyendo que 
# la verdadera proporción de éxito es significativamente inferior a 0.6. 
# Además, es importante señalar que el límite superior del intervalo de confianza (0.4766163) 
# cae por debajo de la proporción de la hipótesis nula de 0.6.


# EJEMPLO 3: Mayor que una proporción
# H0: la proporción de X es p
# H1: la proporción de X es > p

prop.test(x = 42, n = 107, p = 0.6, alternative = "greater")

#El contraste arroja un p-valor de 1, lo que indica que no hay pruebas para rechazar la hipótesis nula 
# de que la proporción verdadera es p


#### Ejemplos: 2 variables ####


# EJEMPLO 1: Igualdad de proporciones
#HO: La proporción de X es igual a la proporción de Y (O la diferencia de proporciones es 0)
#H1: la proporción de X es distinta a la proporción de Y.

# X
p1 <- 50  # Éxitos
n1 <- 100 # Pruebas

# Y
p2 <- 80  # Éxitos
n2 <- 200 # Pruebas

# ¿Es la proporción de X igual a la proporción de Y?
prop.test(c(p1, p2), n = c(n1, n2))
prop.test(c(p1, p2), n = c(n1, n2), alternative = "two.sided")

# El p-valor es superior a los niveles de significación habituales, por lo que no tenemos 
# pruebas estadísticas suficientes para rechazar la hipótesis nula, es decir, no hay pruebas que 
# sugieran proporciones diferentes entre los grupos.

#t.test(var~cat,...)
#t.test(var1,var2,...)

## EJEMPLO 2: Menor
#HO: La proporción de X es igual a la proporción de Y (O la diferencia de proporciones es 0)
#H1: la proporción de X es menor que la proporción de Y.

prop.test(c(p1, p2), n = c(n1, n2), alternative = "less")

#El p-valor es 1.333e-05, cercano a cero, lo que implica que hay pruebas estadísticas para rechazar 
# la hipótesis nula y apoyar la hipótesis alternativa de que la proporción de X es menor que la proporción de Y.
# Además, el intervalo de confianza del 95 por ciento oscila entre -1 y -0.1460619, y como no contiene a 0, 
# sugiere que la verdadera diferencia entre las proporciones es significativamente menor que cero


## EJEMPLO 3: Mayor
#HO: La proporción de X es igual a la proporción de Y (O la diferencia de proporciones es 0)
#H1: la proporción de X es mayor que la proporción de Y.

prop.test(c(p1, p2), n = c(n1, n2), alternative = "greater")

#En este caso, el p-valor es 1, lo que implica que no hay pruebas que apoyen la afirmación de que 
# la proporción en el primer grupo (X) es significativamente mayor que la del segundo grupo (Y).

setwd("d:/dev/Estadistica/")
#EJEMPLO 4
base <- readxl::read_excel("Constructora.xlsx")
attach(base)

#a) ¿la media de horas trabajadas es < 210 hrs?
# H0: MU >= 210  
# H1: MU <210
t.test(Horas, mu=210, alternative="less", conf.level=0.95)
# valor-p=0.01052 < alfa=0.05 -> rechazo H0

#b)¿la media de horas trabajadas en la fanea A es < 210 hrs?
# H0: MUa >= 210  
# H1: MUa <210
FA <-base[Faena=="A", ]
t.test(FA$Horas, mu=210, alternative="less", conf.level=0.95)
#La horas trabajadas en la faena A es menor a 210 hrs.
#p-value = 0.0273

#c) Los trabajadores con educación entre 8-12 años ¿Es más del 50%?
# H0:P<=0.5   
# H1:P >0.5
addmargins(table(Educ))

prop.test(x=79, n=139, p=0.5, alternative="greater", conf.level=0.95)
# No hay evidencia que indique que el % no supere el 50%.

#d) ¿Hay diferencia según sexo en las horas extras medias trabajadas?
#primero:
# H0: Var(fem) = Var(masc)  
# H1: Var(fem) distinto Var(masc)
var.test(Hextras~Sexo, alternative = "two.sided",conf.level = 0.95) 
# -> var. distintas
#segundo:
# H0: Mu(fem) = Mu(masc)  
# H1: Mu(fem) distinto Mu(masc)
t.test(Hextras~Sexo, alternative = "two.sided",var.equal = FALSE,conf.level = 0.95)

#e) ¿Hay diferencia en bono promedio entre Hombres >50 y Mujeres > 50?
H50<- base[Sexo=="Masc" & Edad >50, ] 
M50<- base[Sexo=="Fem" & Edad >=50, ] 

var.test(H50$Bono,M50$Bono, alternative = "two.sided")

# HO: μ (masc)= μ(fem)      
# H1: μ(masc) ≠ μ(fem)

t.test(H50$Bono,M50$Bono, alternative = "two.sided",var.equal = TRUE,conf.level = 0.95)

#no hay diferencia en el promedio de bono recibido entre estos dos grupos


