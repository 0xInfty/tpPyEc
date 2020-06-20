#####################################
# TP Probabilidad y Estadística (C) #
#                                   #
#          Inciso 1a                #
#####################################

# Sample
set.seed(2020)
x1...x1000 <- runif(1000) # Generamos una muestra de 1000 v.a. i.i.d. con distribución U[0.1]

# Histogram
hist(x1...x1000,
     freq = FALSE,
     main = "Histograma de muestra uniforme",
     xlab = "Muestra x1...x1000", 
     ylab = "Densidad",
     col = "#66B2FF")
abline(h=1,col="red") # Comparo con la densidad de una U[0,1]
text(0.25,1.05,"f(x)=1/(1-0)=1", col="red") # (si conoces una mejor forma de presentar a la densidad teórica en el gráfico mejor, porque queda medio feo esto)
