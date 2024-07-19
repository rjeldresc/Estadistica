# ejercicio clase 2024-07-03

setwd("d:/dev/Estadistica/Bases de datos/")
bd <- readxl::read_excel("Galton1.xlsx")
plot(bd)
m1 <- lm(bd$T.Hijo ~ bd$T.Padre , data = bd ) #variable hijo "Y" explicada por X
abline(lm(bd$T.Hijo ~ bd$T.Padre , data = bd), col = "red")
print(m1)
# Coefficients:
#   (Intercept)   bd$T.Padre  
# 84.9550       0.4961 

# Y = 84.9550 + 0.4961 X

#Yi = Beta0 + Beta1*Xi + epsilon

