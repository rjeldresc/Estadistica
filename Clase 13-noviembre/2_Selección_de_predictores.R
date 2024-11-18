# Paquetes ####
# install.packages("doParallel")
# install.packages("randomForest")
library(caret)
library(tidyverse)
library(ggpubr)
library(recipes)

# ELIMINACIÓN RECURSIVA MEDIANTE RANDOM FOREST Y BOOTSTRAPPING ####

# Tamaño de los conjuntos de predictores analizados
subsets <- c(3:10)

# Número de resamples para el proceso de bootstrapping
repeticiones <- 30

# Se crea una semilla para cada repetición de validación. Esto solo es necesario si
# se quiere asegurar la reproducibilidad de los resultados, ya que la validación
# cruzada y el bootstrapping implican selección aleatoria.

# El número de semillas necesarias depende del número total de repeticiones: 
# Se necesitan B+1 elementos donde B es el número total de particiones (CV) o
# resampling (bootstrapping). Los primeros B elementos deben ser vectores formados
# por M números enteros, donde M es el número de modelos ajustados, que en este caso
# corresponde con el número de tamaños que se prueba por iteracion. 
# El último elemento solo necesita un único número para ajustar el modelo final.

set.seed(123)
seeds <- vector(mode = "list", length = repeticiones + 1)
for (i in 1:repeticiones) {
  seeds[[i]] <- sample.int(1000, 8)
} 
seeds[[repeticiones + 1]] <- sample.int(1000, 1)

# Se crea un control de entrenamiento donde se define el tipo de modelo empleado
# para la selección de variables, en este caso random forest, la estrategia de
# resampling, en este caso bootstrapping con 30 repeticiones, y las semillas para
# cada repetición. Con el argumento returnResamp = "all" se especifica que se
# almacene la información de todos los modelos generados en todas las repeticiones.
ctrl_rfe <- rfeControl(functions = rfFuncs, method = "boot", number = repeticiones,
                       returnResamp = "all", allowParallel = TRUE, verbose = FALSE,
                       seeds = seeds)
# rfeControl: control de un objeto para la seleccion de variables 
# rfFuncs: random forest (bosques aleatorios)
# returnResamp: metricas a guardarse de los muestreos

# Se ejecuta la eliminación recursiva de predictores

library(doParallel)
detectCores()
Mycluster = makeCluster(3)
registerDoParallel(Mycluster) # PARALELIZACIÓN DE PROCESO

set.seed(342)
ini=Sys.time()
rf_rfe <- rfe(Survived ~ ., data = datos_train_prep,
              sizes = subsets,
              metric = "Accuracy",
              # El accuracy es la proporción de clasificaciones correctas
              rfeControl = ctrl_rfe,
              ntree = 500)
Sys.time()-ini # Time difference of 39.33218 secs
# rfe: Un algoritmo simple de selección hacia atrás, 
# también conocido como eliminación recursiva de variables,
# recursive feature elimination (RFE)
# Dentro de rfe() se pueden especificar argumentos para el modelo empleado, por
# ejemplo, el hiperparámetro ntree=500.

stopImplicitCluster()

# Se muestra una tabla resumen con los resultados
rf_rfe

# El objeto rf_rfe almacena en optVariables las variables del mejor modelo.
rf_rfe$optVariables

# Valores de accuracy y kappa para cada tamaño de modelo en cada resample.
rf_rfe$resample %>% select(1, 2, 3, 8) %>% head(8)

# Métricas promedio de cada tamaño
rf_rfe$resample %>% group_by(Variables) %>%
  summarise(media_accuracy = mean(Accuracy),
            media_kappa = mean(Kappa)) %>%
  arrange(desc(media_accuracy))

# La siguiente imagen representa la evolución del accuracy estimado 
# en función del número de predictores incluido en el modelo.
ggplot(data = rf_rfe$results, aes(x = Variables, y = Accuracy)) +
  geom_line() +
  scale_x_continuous(breaks  = unique(rf_rfe$results$Variables)) +
  geom_point() +
  geom_errorbar(aes(ymin = Accuracy - AccuracySD, ymax = Accuracy + AccuracySD),
                width = 0.2) +
  geom_point(data = rf_rfe$results %>% slice(which.max(Accuracy)),
             color = "red") +
  theme_bw()

# ALGORITMO GENETICO ####

# Control de entrenamiento
ga_ctrl <- gafsControl(functions = rfGA,
                       method = "cv",
                       allowParallel = TRUE,
                       genParallel = TRUE, # paraleliza el calculo de las metricas
                       verbose = FALSE)
# gafsControl: control de parametros para algoritmos geneticos para seleccion de variables
# rfGA: bosque de arboles geneticos 

# Selección de predictores

Mycluster = makeCluster(detectCores()-1)# Parelelizacion
registerDoParallel(Mycluster)

set.seed(10)
ini=Sys.time()
rf_ga <- gafs(x = datos_train_prep %>% select(-Survived),
              y = datos_train_prep$Survived,
              iters = 10, # iteraciones para el arbol
              popSize = 10, # k -fold
              gafsControl = ga_ctrl,
              ntree = 100)
Sys.time()-ini# Time difference of 1.785941 mins
# gafs: Algoritmo genetico para seleccion de variables

stopImplicitCluster()

rf_ga
rf_ga$optVariables

# Accuracy media en cada generación
rf_ga$external %>% group_by(Iter) %>% summarize(accuracy_media = mean(Accuracy))

# FILTRADO DE PREDICTORES MEDIANTE ANOVA, RANDOM FOREST Y CV-REPETIDA ####

# Se crea una semilla para cada partición y cada repetición: el vector debe
# tener B+1 semillas donde B = particiones * repeticiones.
particiones = 10
repeticiones = 5
set.seed(123)
seeds <- sample.int(1000, particiones * repeticiones + 1)

# Control del filtrado
ctrl_filtrado <- sbfControl(functions = rfSBF, method = "repeatedcv",
                            number = particiones, repeats = repeticiones,
                            seeds = seeds, verbose = FALSE, 
                            saveDetails = TRUE, allowParallel = TRUE)
# sbfControl: control de objeto de seleccion por filtro
# rfSBF: bosques aleatorios para seleccion por filtro

Mycluster = makeCluster(3)# Parelelizacion
registerDoParallel(Mycluster)

set.seed(234)
ini=Sys.time()
rf_sbf <- sbf(Survived ~ ., data = datos_train_prep,
              sbfControl = ctrl_filtrado,
              # argumentos para el modelo de evaluación
              ntree = 500)
Sys.time()-ini
# Time difference of 9.680851 secs

stopImplicitCluster()

rf_sbf
rf_sbf$optVariables

# media de precision
mean(rf_sbf$resample$Accuracy)

# media de precision por repeticion
rf_sbf$resample %>% 
  group_by(rep = substring(Resample,8,11)) %>% 
  summarise(Accuracy = mean(Accuracy))

# PREDICTORES A UTILIZAR ####

predictores_filtrados <- rf_sbf$optVariables
predictores_filtrados

# la eleccion de predictores final se puede realizar por aquel modelo que entrego
# la mayor metrica utilizada (en este caso el accuracy) o por aquel que hubiese 
# logrado reducir la mayor cantidad de variables (siempre y cuando no cedamos
# demasiado en la metrica utilizada)


