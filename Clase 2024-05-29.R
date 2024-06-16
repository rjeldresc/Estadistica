#clase 2024-05-29

#ejercicio para modelo continuo - normal

#X1 Normal de media 28°C y sd de 3°C
#X2 Normal de media 24°C y sd de 5°C


x=seq(0,50,0.5)
plot(x,dnorm( x = x, mean=28,sd=3),type="l",lwd=2, col="red") #region 1
lines(x, dnorm( x = x, mean = 24, sd = 5) , type = "l" , lwd = 2, col = "blue") #region 2

pnorm(0,mean = 4 , sd = 5.83)
pnorm(-0.69, 0,1)
