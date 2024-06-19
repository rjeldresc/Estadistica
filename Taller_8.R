##############  DISTRIBUCIÓN NORMAL  ############## 

#rnorm: función de la distribución normal para generar datos aleatorios 
#dnorm: función de densidad de probabilidad 
#pnorm: función de distribución acumulada normal
#qnorm: función cuantil de la distribución normal

# ejemplo 
dnorm(3, m = 2, s = 1)    # calcula probabilidad puntual P(X=3), con mu= 2(media) y s=1 (varianza)
pnorm(3,m = 2, s = 1)     # calcula probabilidad acumulada P(X<3)
1-pnorm(3,m = 2, s = 1)   # calcula P(X> 3)
qnorm(0.8,m = 2, s = 1)   # determina la mediana, P(X < ?)= 0.8 / Q(P(X < ?))=Q(0.8)
qnorm(1-0.8,m = 2, s = 1) # P(X > ?)= 0.8 / Q(P(X > ?))= Q(1-P(X < ?))=Q(1-0.8)

#En una distribución continua la diferencia entre x >  y x ≥ es infinitesimal, 
#por lo que R puedes usar pnorm() con lower.tail = FALSE para obtener la probabilidad complementaria directa. 
#La función pnorm() maneja ambas situaciones de manera efectiva.


pnorm(3,m = 2, s = 1) = pnorm(3,m = 2, s = 1, lower.tail = TRUE)
1-pnorm(3,m = 2, s = 1)= pnorm(3,m = 2, s = 1, lower.tail = FALSE)

qnorm(1-0.5,m = 2, s = 1) = qnorm(0.5,m = 2, s = 1,lower.tail=FALSE)



############## EJEMPLOS ##############

#Ejemplo 1
x <- seq(-4, 8, 0.1) #sucesión de números entre -4 y 8, aumentando en 0.1.
y1 <- dnorm(x, m = 2, s = 1) 
plot(x,y1, ylim=c(0,1),type ="l",col="red",ylab ="y") # Gráfico de la densidad normal
grid(nx = NULL, ny = NULL,
     lty = 2,      # Tipo de línea
     col = "gray", # Color
     lwd = 2)      # Ancho de línea


#Ejemplo 2
#Distribución normal estandar
y2 <- dnorm(x)  # =dnorm(x,0,1) 
lines(x,y2,col="blue")
legend("topleft", c("Normal", "Normal Estándar"),
       lty = c(1, 1), 
       col = c("blue", "red"), 
       bty = "n", lwd = 1,cex=0.7)


# Generar la secuencia de valores x
x <- seq(-4, 8, 0.1)
# Densidad de la distribución normal estándar
y_standard <- dnorm(x)
# Densidad de otra distribución normal con media = 2 y desviación estándar = 1
y_normal <- dnorm(x, mean = 2, sd = 1)
# Graficar la distribución normal con media = 2 en azul
plot(x, y_normal, type = "l", col = "blue", ylab = "Densidad", xlab = "x", main = "Distribuciones Normales")
# Agregar la distribución normal estándar a la gráfica en rojo
lines(x, y_standard, col = "red")
# Agregar la leyenda
legend("topleft", c("Normal (media=2, sd=1)", "Normal Estándar"),
       lty = c(1, 1), 
       col = c("blue", "red"), 
       bty = "n", lwd = 1, cex = 0.7)


#Ejemplo 3
y3 <- pnorm(x, 2, 1) 
lines(x,y3,col="green")
legend("topleft", c("Normal", "Normal Estándar","Normal Acumulada"),
       lty = c(1, 1, 1),
       col = c("red", "blue","green"),
       bty = "n", lwd = 1,cex=0.7)

#Ejemplo 4
#La probabilidad de que X sea menor o igual a 3, P(X <= 3)
abline(v=3)
pnorm(3,2,1)
#La probabilidad de que X sea mayor o igual a 3, P(X > 3) = 1 - P(X <= 3)
pnorm(3, 2, 1, lower.tail=FALSE)
1-pnorm(3, 2, 1)

#Ejemplo 5
#Distribución normal estándar, la probabilidad de que X sea menor o igual que 3, mu= 2(media) y sd=1 (varianza)
#P(X <= 3) = P(Z<=(3-2)/1)= P(Z<=1)
pnorm((3-2)/1)

#Ejemplo 6
#La probabilidad de que X se encuentre entre -3 y 3 (ambos inclusive)
#P(-3<=X<=3)  =P(X<=3) - P(X<=-3) = ... = P(Z<=1) - P(Z<=-5)
pnorm(1) - pnorm(-5)

#Ejemplo 7
#Supongamos que X tiene distribución normal con media μ=2 y desviación estándar σ=1.1
#Sea Z la variable aleatoria que tiene distribución normal estándar. Hallar el valor de k tal que:
#a) P(X > k)=0.83
alfa <- 0.83
mean <- 2
sd <- 1.1

qnorm(1-alfa, mean, sd)
qnorm(alfa, mean, sd, lower.tail = FALSE)

#b) P(X ≤ k)=0.95
alfa <- 0.95
mean <- 2
sd <- 1.1
qnorm(alfa, mean, sd)

#c) P(k < X < 3.1)=0.75
#P(k < X < 3.1) = P(X < 3.1)-P(X < K) =0.75 
pnorm(3.1,2,1.1)-0.75
#P(X < K) = 0.09134475
qnorm(0.09134475,2,1.1)

#d) P(−k < Z < k)=0.95
#TAREA (R: 1.959964)
k <- qnorm(0.975)
k
p <- pnorm(k) - pnorm(-k)
p

############## Graficos ##############
## 1. Funcion "shadow.dist"
shadow.dist <- function(dist='dnorm', param=list(mean=0, sd=1),
                        a=NULL, b=NULL, type='lower',
                        col.shadow='skyblue', col.line='black', lwd=3,
                        nbreaks=10000, ylab=NULL, x, ...) {
  
  type <- match.arg(arg=type, choices=c('lower', 'middle', 'upper', 'two'))
  
  if (is.null(a) & is.null(b)) stop('At least define the parameter a')
  
  if (type %in% c('middle', 'two') & length(c(a, b)) <=1)
    stop("When type is 'middle' or 'two' you must define a & b")
  
  if (length(c(a, b)) == 2) { # To ensure that a < b
    values <- c(a, b)
    a <- min(values)
    b <- max(values)
  }
  
  if (is.null(a) & !is.null(b)) a <- b
  
  if (type == 'lower') {
    b <- a
    a <- -999
  }
  
  if (type == 'upper') {
    b <- 999
  }
  
  # To include the ylab automatically if ylab was NULL
  if (is.null(ylab)) ylab <- 'Density'
  
  # To create the main polygon
  step <- (b - a) / nbreaks
  cord.x <- c(a, seq(from=a, to=b, by=step), b)
  y <- seq(from=a, to=b, by=step)
  cord.y <- c(0, do.call(dist, c(list(x=y), param)), 0)
  
  # To create a secondary polygon if type='two'
  edge <- 999
  cord.x2 <- c(-edge, seq(from=-edge, to=edge, length.out=1e6), edge)
  y <- seq(from=-edge, to=edge, length.out=1e6)
  cord.y2 <- c(0, do.call(dist, c(list(x=y), param)), 0)
  
  curve(do.call(dist, c(list(x), param)), ylab=ylab, ...) # First curve
  if (type == 'two') {
    polygon(cord.x2, cord.y2, col=col.shadow)
    col.shadow <- 'white'
  }
  polygon(cord.x, cord.y, col=col.shadow)                   # Main shadow
  curve(do.call(dist, c(list(x), param)), add=TRUE,
        lwd=lwd, col=col.line)                              # Second curve
}

#Ahora usamos la función

# ejemplo 1: 
shadow.dist(dist='dnorm', 
            param=list(mean=0, sd=1),
            a=0, b=1, 
            type='middle', from=-3, to=3)

# ejemplo 2: 
shadow.dist(dist='dnorm', 
            param=list(mean=3.1, sd=1.2),
            a=qnorm(p=0.15, mean=3.1, sd=1.2), 
            type='lower', from=0, to=6,
            main='', las=1,
            col.shadow='springgreen3', xlab='X', ylab='Densidad')
text(x=1.3, y=0.05, "0.15", col="yellow1", cex=2)

#2.Funcion "normal_area" 
# mean: media de la variable normal
# sd: desviación típica de la variable normal
# lb: límite inferior del área
# ub: límite superior del área
# acolor: color del área
# ...: argumentos adicionales para ser pasados a la función lines

normal_area <- function(mean = 0, sd = 1, lb, ub, acolor = "lightgray", ...) {
  x <- seq(mean - 3 * sd, mean + 3 * sd, length = 100) 
  
  if (missing(lb)) {
    lb <- min(x)
  }
  if (missing(ub)) {
    ub <- max(x)
  }
  
  x2 <- seq(lb, ub, length = 100)    
  plot(x, dnorm(x, mean, sd), type = "n", ylab = "")
  
  y <- dnorm(x2, mean, sd)
  polygon(c(lb, x2, ub), c(0, y, 0), col = acolor)
  lines(x, dnorm(x, mean, sd), type = "l", ...)
}

#ejemplo 1
normal_area(mean = 0, sd = 1, 
            lb = -1, ub = 2, lwd = 2, 
            acolor = "red")

#ejemplo 2
normal_area(mean = 1000, sd = 10, 
            lb = 980, 
            acolor = "purple")
text(1000, 0.01, "97.72%")

#ejemplo 3
normal_area(mean = 10, sd = 5, 
            lb = 5, ub = 16,
            acolor = "green")
res<-pnorm(16, 10, 5) - pnorm(5, 10,5)
text(10, 0.02, labels=paste0(round(res*100,2),"%"), srt = 90)

#ejemplo 4
normal_area(mean = 0, sd = 1, 
            ub = -1.5,lwd = 2, 
            acolor = "cyan")
arrows(-0.5, 0.1, -1.45, 0, lwd = 2, length = 0.2)
text(-0.25, 0.13, "-1.5", cex = 1.5)

#3.Otra forma de "pintar"
regionX=seq(150,168)            # Intervalo a sombrear
xP <- c(150,regionX,168)             # Base de los polígonos que crean el efecto "sombra"
yP <- c(0,dnorm(regionX,170,12),0)   # Altura de los polígonos sombreados
curve(dnorm(x,170,12),xlim=c(130,210),yaxs="i",ylim=c(0,0.035),ylab="f(x)",
      main='Densidad N(170,12)') 
polygon(xP,yP,col="orange1")
box()




library(ggplot2)
# Crear un data frame con la secuencia de valores x y la densidad de la distribución normal estándar
x <- seq(-4, 4, 0.01)
y <- dnorm(x)
data <- data.frame(x, y)

# Definir los límites del área a pintar
x_fill <- seq(-1.96, 1.96, 0.01)
y_fill <- dnorm(x_fill)
data_fill <- data.frame(x = x_fill, y = y_fill)

# Crear el gráfico con ggplot
ggplot(data, aes(x = x, y = y)) +
  geom_line(color = "blue", size = 1) +  # Curva de la distribución normal estándar
  geom_area(data = data_fill, aes(x = x, y = y), fill = "skyblue", alpha = 0.5) +  # Área bajo la curva
  geom_vline(xintercept = c(-1.96, 1.96), color = "red", linetype = "dashed", size = 1) +  # Líneas verticales en los límites
  labs(title = "Área bajo la curva de la distribución normal estándar",
       x = "x",
       y = "Densidad") +
  theme_minimal()
# Explicación del código:
#   
#   Crear el data frame: Se crea un data frame con la secuencia de valores x y las densidades y de la distribución normal estándar.
# Definir los límites del área a pintar: Se crea un data frame separado (data_fill) con los valores de x y y dentro de los límites que queremos pintar (entre -1.96 y 1.96).
# Crear el gráfico con ggplot:
#   geom_line(color = "blue", size = 1): Dibuja la curva de la distribución normal estándar en azul.
# geom_area(data = data_fill, aes(x = x, y = y), fill = "skyblue", alpha = 0.5): Pinta el área bajo la curva en color azul cielo con cierta transparencia (alpha = 0.5).
# geom_vline(xintercept = c(-1.96, 1.96), color = "red", linetype = "dashed", size = 1): Añade líneas verticales en los límites -1.96 y 1.96.
# labs(...): Añade títulos y etiquetas a los ejes.
# theme_minimal(): Aplica un tema minimalista al gráfico.
# 
# Este código utiliza ggplot2 para crear un gráfico limpio y profesional que muestra la curva de la distribución normal estándar y el área bajo la curva entre los límites especificados.
