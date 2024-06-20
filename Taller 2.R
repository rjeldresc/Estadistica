
# Ejercicio 1
# Ejercicio 1.a

# X = Cuenta # de accidentes de transito (exitos) en 24 horas (t). (en fiestas patrias)
# Ante poisson E(X) = lambda = 143
# X ~ Poisson(lambda = 143) 
# X : {0, 1, 2, 3, 4, ...}

# En un próximo 18/9 entre las 7:00 y 9:00 hrs ocurran por lo menos 5 accidentes
# de tránsito en el país

# Y = Cuenta # de accidentes de transito (exitos) en 2 horas (t). (en fiestas patrias)
# Y ~ Poisson(lambda = ?) 
# Y : {0, 1, 2, 3, 4, 5, ...}

# regla de 3
# lambda 143 <-> 24 horas
# lambda ?   <-> 1 hora
# lambda ?   <-> 2 horas

lambda_por_hora <- 143/24 # t: 1 hora
lambda_por_hora
lambda_por_2_horas <- lambda_por_hora*2 # t: 2 horas
lambda_por_2_horas
# Y ~ Poisson(lambda = 11.91667) 
# P(Y >= 5) = 1 - P(Y <= 4)
1 - ppois(4, lambda = lambda_por_2_horas) #es por el contexto de la pregunta
# R: 0.9919447

# Ejercicio 1.b

# Z: tiempo entre dos accidentes en fiestas patrias (unidad = minutos)
# Z ~ Exp(lambda_por_min)

lambda_por_minuto <- lambda_por_hora/60

# Me piden P(Z < 30)
pexp(30, lambda_por_minuto)
# R: 0.9491648

# Ejercicio 2
# Ejercicio 2.a

# X: # de encuestas que no llegan a un destinatario en 8 intentos
# X: {0, 1, 2, 3, 4, ..., 8}
# X ~ Binomial(n = 8, p = 0.05)

n = 8; p = 0.05

# P(X <= 2)
pbinom(2, n, p)
#R: 0.9942118

# Ejercicio 2.b
# Y: # de envios/encuestas hasta recibir el tercer "informe de no recepcion"
# (evento exito: no recibir una encuesta)
# Y: 3, 4, 5, 6, 7, 8, ...
# Y ~ BinNeg(r = 3, p = 0.05)

r = 3; p = 0.05

# E(Y) = r/p
r/p
#R: 60 encuestas de deben enviar para esperar el tercer informe de no recepcion

# Ejercicio 3

# A = Concentracion de plomo en muestras de plaguicida de tipo A
# A ~ Normal(mu = 1.48, var = 0.28^2)

mu = 1.48; sd = 0.28 # var = 0.28^2 o var = 0.28**2

# Ejercicio 3.a
# P(1 <= A <= 1.3) = P(A <= 1.3) - P(A <= 1)

p <- pnorm(1.3, mean = mu, sd = sd) - pnorm(1, mean = mu, sd = sd)
p
#R: 0.2169203

# Ejercicio 3.b

# Y: # de muestras de plaguicida tipo A con concentracion de plomo mayor a 1.3 u.
# evento exito: muestra posea una concentracion de plomo mayor a 1.3 u. (plaguicida tipo A)
# Y: {0, 1, 2, 3, ..., 7}
# Y ~ Binomial(n = 7, p = ?)

n = 7
# p = P(A > 1.3) = 1 - P(A <= 1.3)
p = 1 - pnorm(1.3, mu, sd); p
#R: 0.7398416
# Y ~ Binomial(n = 7, p = 0.7398416)

# Piden P(Y >= 2) = 1 - P(Y < 2) = 1- P(Y <= 1)
1 - pbinom(1, n, p)
#R: 0.9983136

# Ejercicio 3.c

# P(A <= a1) = 0.2 y P(A <= a2) = 0.85

qnorm(0.2, mu, sd)
# R: 1.244346 , Grupo bajo 20%
qnorm(0.85, mu, sd)
# R: 1.770201 , Grupo riesgo

# Ejercicio 4

# MA ~ Normal(38, 9)
# MB ~ Normal(50, 16)

# Ejercicio 4.a

# MT = (MA + MB) ~ Normal(38 + 50, 9 + 16)

mut = 38 + 50 #se suman los dos promedios
sdt =  sqrt(9 + 16) #se suman las dos varianzas, se pasa a desviacion estandar

# Ejercicio 4.b

# P(MT < 88.5)
pnorm(88.5, mut, sdt)
# R: 0.5398278

# Ejercicio 5 ####

# Ejericico 5.a #

# R: 0.2169203

# Ejercicio 5.b # Les he mentido :(, si la tienen que pensar (no es binomial, 
# habla de la concentracion "media")

# R: 1.263295e-08 ~~ 0








