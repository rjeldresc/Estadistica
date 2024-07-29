library(DBI)
library(odbc)

# Establece la conexión a la base de datos
con <- dbConnect(odbc::odbc(),
                 Driver = "ODBC Driver 17 for Sql Server", 
                 Server = "localhost",
                 Database = "DataEstadistica",
                 UID = "rodrigo",
                 PWD = "enter",
                 Port = 1433)

datos <- dbGetQuery(con, "SELECT * FROM [dbo].[AlfabMort]")
dbDisconnect(con)
head(datos)
attach(datos)

cor(Alfab, Mort)
plot(Alfab, Mort)

Mod1 <- lm(formula = Mort ~ Alfab)

summary(Mod1)
#test de significandia del modelo
#F-statistic: 150.1 on 1 and 23 DF,  p-value: 1.457e-11

#Validando los supuestos
par(mfrow=c(2,2))
plot(Mod1)

#Test lilitest
nortest::lillie.test(Mod1$residuals)
#D = 0.23369, p-value = 0.001104

#homocedasticidad
lmtest::bptest(Mod1)
#BP = 6.2971, df = 1, p-value = 0.01209

#agregando efecto cuadratico
Mod2 <- lm(Mort ~ Alfab + I(Alfab^2))
summary(Mod2)
#Adjusted R-squared:  0.9422 

plot(Mod2)

#para los supuestos
nortest::lillie.test(Mod2$residuals)
#D = 0.076014, p-value = 0.9689

lmtest::bptest(Mod2)
#BP = 7.6456, df = 2, p-value = 0.02187

#anomalias
car::outlierTest(Mod2)
#16 -3.071297          0.0057933      0.14483  dato outlier
summary(influence.measures(Mod2)) #influyentes 8 y 16

par(mfrow=c(2,2))
plot(Mod2)

#para ver los hat y los cook
car::influenceIndexPlot(Mod2) #dato 8 y 16





