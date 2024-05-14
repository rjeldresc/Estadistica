library(ggplot2) 

########Funciones disponibles para distribuciones discretas########
#dxxx(x, ...)  # Función de probabilidad, f(x)
#pxxx(q, ...)  # Función de distribución acumulada hasta q, F(x)
#qxxx(p, ...)  # Cuantil para el cual P(X <= q) = p
#rxxx(n, ...)  # Generador de números aleatorios.

# EJEMPLO - modelo Binomial
dbinom(3, size=10, prob=0.3)    # calcula probabilidad puntual P(X=3), para 10 ensayos con 0.3 de probabilidad de éxito en cada ensayo 
pbinom(3,size=10,prob=0.3)      # calcula probabilidad acumulada P(X<=3)
1-pbinom(3,size=10,prob=0.3)    # calcula P(X> 3)
qbinom(0.5, size=10, prob=0.3)  # determina la mediana p = 0.5

pbinom(3,size=10,prob=0.3) = pbinom(3,size=10,prob=0.3, lower.tail = TRUE)
1-pbinom(3,size=10,prob=0.3)= pbinom(3,size=10,prob=0.3, lower.tail = FALSE)


######## MODELO BERNOULLI ########
#Describe un experimento con dos resultados posibles, generalmente etiquetados como éxito o fracaso 
#(por ejemplo, lanzar una moneda). 
#Solo tiene un parámetro, la probabilidad de éxito (p)

x <- c(1, 0) # secuencia de resultados posibles
p <- 0.8  # Probabilidad de éxito en un solo ensayo
prob <- dbinom(x, size = 1, prob = p)
prob_acum <- pbinom(x, size = 1, prob = p)
cbind(x,prob,prob_acum)

ggplot(data.frame(x, prob), aes(x = x, y = prob)) +
  geom_bar(stat = "identity") +
  labs(x = "Resultados posibles", y = "Probabilidad", title = "Distribución Bernoulli") +
  theme_minimal()
  
plot(x=x, y=prob, type='h', las=1, lwd=10, main="Distribución Bernoulli",xlab="Resultados posibles",ylab="Probabilidad")
#type='h', lineas verticales
#las=1: direccion del texto de los ejes
#lwd=10: grosor de las lineas


######## MODELO BINOMINAL ########
#Es útil cuando tienes un número fijo de ensayos independientes de un experimento de Bernoulli 
#(por ejemplo, lanzar una moneda varias veces). 
#Describe la cantidad de éxitos en esos ensayos. 
#Tiene dos parámetros: el número total de ensayos (n) y la probabilidad de éxito en cada ensayo (p).

n <- 10  #Número de ensayos
p <- 0.5 #Probabilidad de éxito en cada ensayo
x <- 0:n #secuencia de números que representen el número de éxitos posibles
prob <- dbinom(x, size = n, prob = p)
prob_acum <- pbinom(x, size = n, prob = p)
cbind(x,prob,prob_acum)

ggplot(data.frame(x, prob), aes(x = x, y = prob)) +
  geom_bar(stat = "identity") +
  labs(x = "Número de éxitos posibles", y = "Probabilidad", title = "Distribución Binomial") +
  theme_minimal()

plot(x,prob,type='h', las=1, lwd=10, main="Distribución Binominal",xlab="Resultados posibles",ylab="Probabilidad")

#Ejemplo
#En un ensayo clínico se ha tomado un total de 50 pacientes y la probabilidad de que el paciente fume es 0.35.
#La variable aleatoria que cuenta el número de pacientes fumadores sigue una distribución Binomial de parámetros n = 50 y p = 0.35.
# a) Calcular la probabilidad de que exactamente 20 pacientes sean fumadores. P(x=20)
n=50
p=0.35
x=20
dbinom(x,n,p)

muestra = 0:n

ggplot(data.frame(muestra, dbinom(muestra, n, p)), aes(muestra, dbinom(muestra, n, p), fill=ifelse(muestra==x,"P(x=20)","Resto"))) +
  geom_bar(stat = "identity",width = 0.75) + xlab("Pacientes") + ylab("Densidad de probabilidad") +
  ggtitle("Distribución Binomial") +
  scale_fill_manual(name = "", values=c("red","grey")) + theme_bw()

#b) Obtener la probabilidad de que al menos 15 pacientes sean fumadores
#   Aquí se pide calcular P(X ≥15) = P(X > 14), que sería equivalente a P(X ≥ 15) = 1 − P(X < 15) = 1 − P(X ≤ 14)

x=14
pbinom(x,n,p,lower.tail=FALSE)
1-pbinom(x,n,p)

ggplot(data.frame(muestra, dbinom(muestra, n, p)), aes(muestra, dbinom(muestra, n, p), fill=ifelse(muestra>x,"P(x>14)=P(x>=15)","Resto"))) +
  geom_bar(stat = "identity",width=0.75) + xlab("Pacientes") + ylab("Densidad de probabilidad") +
  ggtitle("Distribución Binomial") +
  scale_fill_manual(name = "", values=c("red","grey")) + theme_bw()

#c) Obtener la probabilidad de que entre 10 y 15 pacientes (ambos inclusive) sean fumadores. 
# Se pide calcular P(10 ≤ X ≤ 15) = P(X ≤ 15) − P(X < 10) = P(X ≤ 15) − P(X ≤ 9)
x1=15
x2=9
pbinom(x1,n,p)-pbinom(x2,n,p)

ggplot(data.frame(muestra, dbinom(muestra, n, p)), aes(muestra, dbinom(muestra, n, p), fill=ifelse(muestra>=x2+1 & muestra<=x1,"P(10 ≤ X ≤ 15)","Resto")))+
  geom_bar(stat = "identity",width=0.75) + xlab("Pacientes") + ylab("Densidad de probabilidad") +
  ggtitle("Distribución Binomial") +
  scale_fill_manual(name = "", values=c("red","grey")) + theme_bw()

#d) Calcular el valor de la variable tal que deja a su derecha un 70% de las observaciones. 
# El valor de la variable que deja a su derecha un 70% de las observaciones es el mismo que deja a su izquierda el 30%
# restante, por lo que se pide calcular el valor q tal que P(X ≤ q) = 0.30
a=0.30
qbinom(a,n,p)

ggplot(data.frame(muestra, dbinom(muestra, n, p)), aes(muestra, dbinom(muestra, n, p), fill=ifelse(pbinom(muestra, n, p)<=a,"30%","Resto"))) +
  geom_bar(stat = "identity") + xlab("Pacientes") + ylab("Densidad de probabilidad") +
  ggtitle("Distribución Binomial") +
  scale_fill_manual(name = "", values=c("red","grey")) + theme_bw()


######## MODELO GEOMÉTRICO ########
#Describe el número de ensayos independientes de un experimento de Bernoulli necesarios para obtener el primer éxito. 
#Tiene un solo parámetro: la probabilidad de éxito en cada ensayo (p).

p <- 0.2  #Probabilidad de éxito en un ensayo
x <- 1:10 #secuencia de números de ensayos (1, 2, 3, ...)
prob <- dgeom(x - 1, prob = p)
prob_acum <- pgeom(x - 1, prob = p)
cbind(x,prob,prob_acum)

ggplot(data.frame(x, prob), aes(x, prob)) +
  geom_bar(stat = "identity") +
  labs(x = "Números de ensayos (1, 2, 3, ...)", y = "Probabilidad", title = "Distribución Geométrica") +
  theme_minimal()


######## MODELO BINOMINAL NEGATIVO ########
# Describe el número de ensayos independientes de un experimento de Bernoulli necesarios para obtener un número específico de éxitos. 
#Tiene dos parámetros: el número de éxitos deseado (r) y la probabilidad de éxito en cada ensayo (p).

r <- 3  #Número de éxitos deseados
p <- 0.3  #Probabilidad de éxito en un ensayo
x <- 1:20 #secuencia de números de ensayos (1, 2, 3, ...)
prob <- dnbinom(x - 1, size = r, prob = p)
prob_acum <- pnbinom(x - 1, size = r, prob = p)
cbind(x,prob,prob_acum)

ggplot(data.frame(x, prob), aes(x, prob)) +
  geom_bar(stat = "identity") +
  labs(x = "Números de ensayos (1, 2, 3, ...)", y = "Probabilidad", title = "Distribución Binomial Negativa") +
  theme_minimal()


######## MODELO HIPERGEOMÉTRICO ########
#Se aplica cuando estás extrayendo objetos de una población finita sin reemplazo. 
#Describe la probabilidad de obtener un número específico de éxitos en un cierto número de ensayos, sin reemplazar los objetos extraídos. 
#Tiene cuatro parámetros: 
#el tamaño de la población (N), 
#el número de éxitos en la población (M), 
#el tamaño de la muestra (n) 
#y el número de éxitos en la muestra (k)

N <- 100  #Tamaño total de la población
K <- 20   #Número total de elementos de la clase de interés en la población
n <- 10   #Tamaño de la muestra
x <- 0:min(n, K )#secuencia de números de éxitos posibles en la muestra (0 a n)
prob <- dhyper(x, K, N - K, n)
prob_acum <- phyper(x, K, N - K, n)
cbind(x,prob,prob_acum)

ggplot(data.frame(x, prob), aes(x, prob)) +
  geom_bar(stat = "identity") +
  labs(x = "Números de éxitos posibles en la muestra", y = "Probabilidad", title = "Distribución Hipergeométrica") +
  theme_minimal()


######## MODELO POISSON ########
#Utilizado para describir el número de eventos que ocurren en un intervalo de tiempo o espacio fijo, 
#cuando estos eventos ocurren a una tasa constante e independiente del tiempo. 
#Tiene un parámetro: la tasa de ocurrencia de eventos (λ)

lambda <- 2.5  #Tasa de eventos por unidad de tiempo o espacio
x <- 0:10 #secuencia de números de eventos posibles (0, 1, 2, ...)
prob <- dpois(x, lambda)
prob_acum <- ppois(x, lambda)
cbind(x,prob,prob_acum)

ggplot(data.frame(x, prob), aes(x, prob)) +
  geom_bar(stat = "identity") +
  labs(x = "Números de eventos posibles", y = "Probabilidad", title = "Distribución de Poisson") +
  theme_minimal()



                       ######## EJERCICIOS ########

######## Ejercicio 1 ########
#De un total de 10 postulantes a un cargo de docente,tres son mujeres. 
#Se seleccionará a solo 5 personas de estos postulantes 
#¿Cuál es la probabilidad que entre los elegidos haya solo una mujer?
N <- 10  #Tamaño total de la población
K <- 3   #Número total de elementos de la clase de interés en la población
n <- 5   #Tamaño de la muestra
x <- 1#secuencia de números de éxitos posibles en la muestra (0 a n)
dhyper(x, K, N - K, n)

######## Ejercicio 2 ########
#El n° de infracciones por mal estacionamiento en una ciudad en cualquier día de  la semana, en horario de
#8:00 a 20:00 sigue una distribución Poisson con una media de 36 infracciones diarias

# a) probailidad que ocurran 12 infracciones entre las 8:00 y 13:00 en un día cualquiera?
#Infracciones por hora = 36/12 = 3
lambda <- 15  #Tasa de eventos por unidad de tiempo o espacio = 3*5 en 5 horas
x <- 12 #números de eventos posibles 
dpois(x, lambda)

#b) probabilidad que ocurra al menos dos infracciones en un lapso de media hora?
lambda <- 1.5  #Tasa de eventos por unidad de tiempo o espacio = 3*0.5 en 1/2 hr 
#al menos 2 infracciones es igual que 1 menos "no mas de 1" => 𝑃(𝑋≥2) = 1−𝑃(𝑋≤1)
x <- 1 #números de eventos posibles 
1-dpois(x, lambda)

######## Ejercicio 3 ########
# Un laboratorio que trabaja 24 × 7 recibe muestras de PCR para su análisis. Suponga que las últimas 1.000
# muestras analizadas presentaron una positividad del 8% 
#¿Cuál es la probabilidad qué al revisar 20 resultados entre las últimas mil muestra analizadas, 
#al menos 3 arrojen PCR positivo?
N <- 1000  #Tamaño total de la población
K <- 80   #Número total de elementos de la clase de interés en la población. PCR positivos 1000*0.08
n <- 20   #Tamaño de la muestra
#al menos 3 es igual que 1 menos no mas de 2 => 𝑃(𝑋≥3) = 1−𝑃(𝑋≤2)
x <- 2 #números de éxitos posibles en la muestra 
1-phyper(2, K, N - K, n)

######## Ejercicio 4 ########
# Un grupo de investigadores está recopilando información para realizar un estudio estadístico. La información
# está siendo recopilada mediante el envío de una encuesta electrónica a mails de distintas personas, la
# cual queda registrada en una página web con el nombre del correo electrónico a quien se le envió. 
# Se sabe que cada una de las encuestas enviadas no llega al destinatario (mail enviado) el 5 % de las veces
# independientemente. Si la encuesta no fue recibida por un destinatario, el mail de ese destinatario queda sin
# información en la página web

# a) En un envío de 8 encuestas, ¿cuál es la probabilidadque a lo más 2 no lleguen a destino?
n <- 8  #Número de ensayos
p <- 0.05 #Probabilidad de éxito en cada ensayo
x <- 2  #números que representen el número de éxitos posibles
1- dbinom(x, size = n, prob = p) 

# b) Cuantas encuestas debería esperar enviar hasta recibir el tercer informe de no recepción
r <- 3  #Número de éxitos deseados
p <- 0.05  #Probabilidad de éxito en un ensayo
x <- 60 # ensayos 3/0.05
dnbinom(x - 1, size = r, prob = p)


######## Ejercicio 5 ########
#Fiscalizadores de tránsito revisan el estado de los buses. 
#De datos históricos se sabe que un 10% de los buses generan una mayor cantidad de humo de la permitida. 
#En cada jornada se revisan siempre 18 buses, asuma que el estado de un bus es independiente del estado de los otros buses.

#a) Calcular la probabilidad de que se encuentren exactamente 2 buses que generan una mayor cantidad de humo de la permitida. 
#   R-> P(X=2) = 0.2835121
dbinom(x=2, size=18, prob=0.10)

#b) Calcular la probabilidad de que el número de buses que sobrepasan el límite de generación de gases sea al menos 4
#   R-> P(X>=4) =0.09819684
#   Recuerde que: 1-pbinom(x,size,prob)    # calcula P(X> x)
1-pbinom(3,18,0.1)
pbinom(3,18,0.1,lower.tail = FALSE)

#c) Calcular la probabilidad de que tres o menos buses emitan gases por encima de lo permitido en la norma.
#   R-> P(X<=3)=0.9018032
pbinom(q=3, size=18, prob=0.10)
    
#d) Dibujar la función de probabilidad
x <- 0:18 
plot(x, dbinom(x=x, 18, 0.1), type='h', las=1, lwd=6,ylab="Probabilidad")

######## Ejercicio 6 ######## 
#En una línea de producción de ampolletas se sabe que sólo el 1% son defectuosas. 
#Una máquina toma la ampolleta y lo prueba. Si enciende, se siguen probando hasta que se encuentre una defectuosa, 
#ahí se para la línea de producción y se toman los correctivos necesarios para mejorar el proceso.

#a) Calcular la probabilidad de que se necesiten probar 125 ampolletas para encontrar la primera defectuosa.
#R: Geometrica: Describe el número de ensayos independientes de un experimento de Bernoulli necesarios para obtener el primer éxito.
#   En este caso, nuestro primer éxito es cuando encontremos la primera que falle
#P(X=124) = 0.002875836
dgeom(x=124, prob=0.01)


