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
