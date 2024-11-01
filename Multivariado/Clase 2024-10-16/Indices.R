#### INDICES
# Los índices son medidas utilizadas para resumir o representar la información contenida en un conjunto de variables. 
# Son útiles para reducir la complejidad de los datos y facilitar la interpretación.


#### IDH (Indice de desarrollo humano) ####
# Cada dimension o variable se debe expresar en una escala de 0 a 1.
# Se eligen los valores min y max para cada uno:
# Indice de dimension = (valor_real - valor_min)/(valor_max - valor_min)
# IDH se calcula con el promedio simple de los indices de las dimensiones


#### Ejercicio ####
# Indices MUJER
# Maternidad: Mortalidad infantil, embarazo adolescente, tasa global de fecundidad (TGF), Educación preescolar y 
#             % Papanicolaou 
# Calidad de Vida: Prevalencia de estrés, tasa de femicidios, tasa VIF, tasa de victimas de delitos, contaminación (MP10)
# Empoderamiento: Indigencia, Participación laboral, Salario promedio, brecha salarial, nivel de escolaridad, 
#                 porcentaje en puestos de gerentes, directivos y porcentaje en cargos políticos
setwd("D:/dev/Estadistica/Multivariado/Clase 2024-10-16")
base <- readxl::read_excel("RankingMujer.xlsx")
head(base)
names(base)
attach(base)  


# 1. Necesitamos "escalar" segun IDH (0 a 1) cada variable
# por ejemplo: Papanicolau
Papa1 <- (Papanicolau-min(Papanicolau)) / (max(Papanicolau) - min(Papanicolau))
summary(Papa1)

# Creamos una funcion para escalar o normalizar todas las varaibles:
normalizar = function(x) {
  aux <- (x-min(x))/(max(x)-min(x))
  return(aux)
}
# Por ejemplo, para Papanicolau
p1<-normalizar(Papanicolau)
summary(p1)  # coincide con la normalizacion anterior

# 2. Construimos las variables normalizados para cada dimension

# Indice de Maternidad
# directas:  Preescolar, Papanicolau, TGFecund
Pree1<-normalizar(Preescolar)
Papa1<-normalizar(Papanicolau)
TGF1<-normalizar(TGFecund)
# inversas: EmbAdoles, TMortInfantil (son perjudiciales para indice maternidad)
Emba1<- 1 - normalizar(EmbAdoles)
TMor1<- 1 - normalizar(TMortInfantil)

Maternidad = (Pree1+Papa1+TGF1+Emba1+TMor1)/5


# Indice de Calidad de Vida
# directas:  Ninguna!
# inversas: MP10, TVictTot, TEstrés, TFeminic, TVIntraf
MP101<- 1 - normalizar(MP10)
TVic1<- 1 - normalizar(TVictTot)
TEst1<- 1 - normalizar(TEstrés)
TFem1<- 1 - normalizar(TFeminic)
TVIn1<- 1 - normalizar(TVIntraf)

CVida <- (MP101+TVic1+TEst1+TFem1+TVIn1)/5

# Indice de Empoderamiento
# directa: FuerzaLaboral, SueldoProm, Escolarid, Puestos, Politicos
Fuer1<- normalizar(FuerzaLaboral)
Suel1<- normalizar(SueldoProm)
Esco1<- normalizar(Escolarid)
Pues1<- normalizar(Puestos)
Poli1<- normalizar(Politicos)
# inversas: Pindigencia, BrechaSueld
Pind1<- 1 - normalizar(Pindigencia)
Brec1<- 1 - normalizar(`Brecha sueld`)

Empod <- (Fuer1+Suel1+Esco1+Pues1+Poli1+Pind1+Brec1)/7 # 7 por cantidad de variables que hay

#Resumen por region
cbind(REGION,cbind(Maternidad,CVida,Empod)*100)

# Graficos "de a par" Maternidad vs CVida
plot(Maternidad, CVida ,
     xlab = "Maternidad", 
     ylab = "Calidad_Vida",
     ylim = c(0.3,1), xlim = c(0.2,1))   ## se buscan limites para ver nombres

text(Maternidad-0.02,CVida+0.02,
     REGION,
     col="blue",
     cex=0.7)
abline(h = 0.5, v = 0.5, lty = 2)

# Graficos "de a par" Maternidad vs CVida
plot(Maternidad, Empod ,
     xlab = "Maternidad", 
     ylab = "Empoderamiento",
     ylim = c(0.,.8), xlim = c(0.2,1))  ## se buscan limites para ver nombres
text(Maternidad-0.02,Empod+0.02,
     REGION,
     col="blue",
     cex=0.7)
abline(h = 0.5, v = 0.5, lty = 2)


# Indice global
indice <- (Maternidad+CVida+Empod)/3
listado <- cbind(REGION,round(indice*100,2))
colnames(listado) <- c("Region","Ptje")   #asignamos nombres
list <- as.data.frame(listado)            #indicamos que es una "base"
list[order(list$Ptje), ]		    #mostramos segun orden de ptje
