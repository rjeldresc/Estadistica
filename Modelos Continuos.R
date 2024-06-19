# modelo uniforme

# se denota X - Unif(a,b)

x <- seq(from = -0.5, to = 2.5 , by = 0.001)
?seq
plot(x, dunif(x = x, min = 0 , max =2), 
     type = "l" , 
     ylab = "F(x)", 
     lwd = 2, 
     col = "red")
