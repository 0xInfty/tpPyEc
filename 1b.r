#####################################
# TP Probabilidad y Estadística (C) #
#                                   #
#          Inciso 1b                #
#####################################

# Sample
set.seed(2020)
promedio <- function(n) { # Calcula el promedio de n uniformes [0,1]
  x1...xn <- runif(n)
  sum(x1...xn)/n
}

x1...x1000 <- sapply(rep(2,times=1000), promedio) # Aplica la funcion promedio a mil numeros 2, por lo tanto se obtienen mil promedios de 2 uniformes [0,1]

# Histogram
hist(x1...x1000,
     freq = FALSE,
     main = "Histograma de promedio de dos uniformes",
     xlab = "Muestra x1...x1000", 
     ylab = "Densidad",
     col = "#66B2FF")
