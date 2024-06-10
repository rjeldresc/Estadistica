############## EJERICIOS

#EJERICIO 1
#En una editorial se asume que todo libro de 250 páginas tiene en promedio 50 errores.
#a) Encuentre la probabilidad de que en una página cualquiera no se encuentren errores (R: 0.8187308)

#EJERCICIO 2
#Una familia desea tener hijos hasta conseguir 2 niñas, la probabilidad individual de obtener una niña es 0.5 y 
#se supone que todos los nacimientos son individuales, es decir, un sólo bebé.
#a) Calcular la probabilidad de que se necesiten 4 hijos, es decir, 4 nacimientos para consguir las dos niñas (R: 0.1875)
#b) Calcular P(familia tenga al menos 4 hijos) (R:0.5)

#EJERCICIO 3
#Una empresa farmacéutica desarrolló un nuevo medicamento y lo suministró a 10 enfermos elegidos aleatoriamente. 
#La experiencia ha demostrado que 30% de las personas que padecen la enfermedad se recupera al tomar dicho medicamento. 
#¿Cuál es la probabilidad de que por lo menos nueve de las 10 personas que toman el medicamento se recuperen? (R:0.0001436859)

#EJERCICIO 4
#Un equipo de trabajo establecido por el Ministerio de Medio Ambiente, programó visitas a dos fábricas 
#para investigar posibles violaciones a los reglamentos para el control de contaminación ambiental. 
#Sin embargo, los recortes presupuestales han reducido drásticamente el tamaño del equipo de trabajo por lo que 
#solamente se podrán investigar cinco de las 25 fábricas. 
#Si se sabe que 10 de las fábricas están operando sin cumplir los reglamentos, 
#calcular la probabilidad de que al menos una de las fábricas muestreadas esté operando en contra del reglamento (R: 0.9434783)

#EJERCICIO 5
#Los pacientes que entran a un centro de salud lo hacen a una tasa esperada de 0.50 clientes por minuto. 
#Hallar la probabilidad de que el número de clientes que entran en un intervalo específico de 10 minutos sea a lo más 3.
#(R:0.2650259)

#EJERCICIO 6
#Un experto tirador acierta en el blanco el 95 % de veces. 
#¿Cuál es la probabilidad de que falle por primera vez en su decimoquinto disparo? (R:0.02438375)




############## RESPUESTAS
#Ejercicio 1
#a) dpois(0, 0.2)

#Ejercicio 2
#a) dnbinom(2, 2, 0.5)
#b) 1 - pnbinom(1, 2, 0.5)  

#Ejercicio 3
#pbinom(8,n,p,lower.tail=FALSE)

#Ejercicio 4
#1 - dhyper(0, 10, 15, 5)

#Ejercicio 5
#ppois(3, 5)

#Ejercicio 6
#dgeom(14,0.05)