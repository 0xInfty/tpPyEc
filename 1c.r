#####################################
# TP Probabilidad y Estadística (C) #
#                                   #
#          Inciso 1c                #
#####################################

set.seed(2020)

promedio <- function(n) { # Calcula el promedio de n uniformes [0,1]
  x1...xn <- runif(n)
  sum(x1...xn)/n
}

y1...y1000 <- sapply(rep(2,times=1000), promedio) # Aplica la funcion promedio a mil numeros 2, por lo tanto se obtienen mil promedios de 2 uniformes [0,1]

#set.seed(2020) # No se si volver a reestablecer la secuencia de numeros.. ni idea si hay que comparar con la misma secuencia o dejar que se sigan generando más números aleatorios

x1...x1000 <- sapply(rep(5,times=1000), promedio) # Aplica la funcion promedio a mil numeros 5, por lo tanto se obtienen mil promedios de 5 uniformes [0,1]

# Histogram
# Para comparar histogramas los llevo a una misma escala, tanto en x como en y

par(mfrow=c(2,1)) # Divido la pantalla en dos para comparar los histogramas

hy <-hist(y1...y1000,
          freq = FALSE,
          main = "Histograma de promedio de dos uniformes",
          xlab = "Muestra y1...y1000", 
          ylab = "Densidad",
          col = "#66B2FF",
          xlim = c(0,1),
          breaks = seq(0,1,by=0.1),
          ylim = c(0,3.5)) # El 3.5 fue puesto a posteriori para mantener la escala con el histograma de abajo
abline(col="red",h=max(hy$density))

hx <- hist(x1...x1000,
           freq = FALSE,
           main = "Histograma de promedio de cinco uniformes",
           xlab = "Muestra x1...x1000", 
           ylab = "Densidad",
           col = "#66B2FF",
           xlim = c(0,1),
           breaks = seq(0,1,by=0.1),
           ylim = c(0,3.5)) # El 3.5 fue puesto luego de ver el valor máximo que alcanzaba el histograma
abline(col="red",h=max(hx$density))

par(mfrow=c(1,1)) # Reestablezco la ventana gráfica para volver a tener un gráfico a la vez
