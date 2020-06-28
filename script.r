#####################################
# TP Probabilidad y Estadística (C) #
#####################################

# Dejar al menos un espacio al final del script para que al copiar y pegar en la consola del R se ejecuten todos los comandos :)

# Funciones Auxiliares

promedio_n <- function(n, gen) { # Calcula el promedio de n realizaciones de una variable aleatoria con distribución gen
  x1...xn <- gen(n) # Genera n realizaciones de una v.a. con distribucion gen
  sum(x1...xn)/n # Calcula y devuelve el promedio de todas ellas
}

generar_promedios_uniformes <- function(n, size) { # Genera un vector de size promedios de n uniformes [0,1]
  sapply(rep(n, times = size), promedio_n, gen = runif)
}

generar_promedios_cauchy <- function(n, size) { # Genera un vector de size promedios de n cauchys
  sapply(rep(n, times = size), promedio_n, gen = rcauchy)
}

varianza <- function(x) { # Calcula la varianza muestral
  mean(x^2)-mean(x)^2
}


#################### ITEM A ####################

set.seed(0) # Un seed para cada item (a = 0, b = 1, ...)

a <- generar_promedios_uniformes(1, 1000)

ha <- hist(a,
           freq = FALSE,
           main = "Histograma de muestra uniforme",
           xlab = "Muestra x1...x1000",
           ylab = "Densidad",
           col = "#66B2FF")

abline(h = 1, col = "red") # Comparo con la densidad de una U[0,1]

#text(0.25, 1.05, "f(x)=1/(1-0)=1", col = "red") # (si conoces una mejor forma de presentar a la densidad teórica en el gráfico mejor, porque queda medio feo esto)

#################### ITEM B ####################

set.seed(1)

b <- generar_promedios_uniformes(2, 1000)

hb <- hist(b,
           freq = FALSE,
           main = "Histograma de promedio de dos uniformes",
           xlab = "Muestra x1...x1000",
           ylab = "Densidad",
           col = "#66B2FF")


#################### ITEM C ####################

set.seed(2)

c <- generar_promedios_uniformes(5, 1000)

par(mfrow = c(2, 1)) # Divido la pantalla en dos filas para comparar los histogramas

hb <- hist(b,
           freq = FALSE,
           main = "Histograma de promedio de dos uniformes",
           xlab = "Muestra x1...x1000",
           ylab = "Densidad",
           col = "#66B2FF",
           xlim = c(0,1),
           breaks = seq(0, 1, by = 0.1),
           ylim = c(0, 3)) # El 3 fue puesto luego de ver el valor máximo que alcanzaba el histograma

abline(col = "red", h = max(hb$density))

hc <- hist(c,
           freq = FALSE,
           main = "Histograma de promedio de cinco uniformes",
           xlab = "Muestra x1...x1000",
           ylab = "Densidad",
           col = "#66B2FF",
           xlim = c(0,1),
           breaks = seq(0, 1, by = 0.1),
           ylim = c(0, 3)) # El 3 fue puesto luego de ver el valor máximo que alcanzaba el histograma

abline(col = "red", h = max(hc$density))

par(mfrow = c(1, 1)) # Reestablezco la ventana gráfica para volver a tener un gráfico a la vez


#################### ITEM D ####################

set.seed(3)

d <- generar_promedios_uniformes(30, 1000)

par(mfrow = c(2, 1))

hc <- hist(c,
           freq = FALSE,
           main = "Histograma de promedio de cinco uniformes",
           xlab = "Muestra x1...x1000",
           ylab = "Densidad",
           col = "#66B2FF",
           xlim = c(0,1),
           breaks = seq(0, 1, by = 0.05),
           ylim = c(0, 7))

abline(col = "red", h = max(hc$density))

hd <- hist(d,
           freq = FALSE,
           main = "Histograma de promedio de treinta uniformes",
           xlab = "Muestra x1...x1000",
           ylab = "Densidad",
           col = "#66B2FF",
           xlim = c(0,1),
           breaks = seq(0, 1, by = 0.05),
           ylim = c(0, 7))

abline(col = "red", h = max(hd$density))

par(mfrow = c(1, 1))

#################### ITEM E ####################

set.seed(4)

e <- generar_promedios_uniformes(500, 1000)

par(mfrow = c(2, 1))

hd <- hist(d,
           freq = FALSE,
           main = "Histograma de promedio de treinta uniformes",
           xlab = "Muestra x1...x1000",
           ylab = "Densidad",
           col = "#66B2FF",
           xlim = c(0.35, 0.65),
           breaks = seq(0.3, 0.7, by = 0.0125),
           ylim = c(0, 30))

abline(col = "red", h = max(hd$density))

he <- hist(e,
           freq = FALSE,
           main = "Histograma de promedio de quinientas uniformes",
           xlab = "Muestra x1...x1000",
           ylab = "Densidad",
           col = "#66B2FF",
           xlim = c(0.35, 0.65),
           breaks = seq(0.3, 0.7, by = 0.0125),
           ylim = c(0, 30))

abline(col = "red", h = max(he$density))

par(mfrow = c(1, 1))

# Ahora comparamos distintos tamaños de muestras con los promedios fijos (comparar con histogramas y/o con boxplots, pensar eso)

# par(mfrow = c(1, 2)) # Divido el gráfico en dos columnas para comparar los boxplots
# par(mfrow = c(2, 1)) # Divido el gráfico en dos filas para comparar los histogramas

# 1000 muestras de promedio de 500 (el primero que pidieron)
# 2000 muestras de promedio de 500
# 4000 muestras de promedio de 500
# 8000 muestras de promedio de 500
# 16000 muestras de promedio de 500

# (o proponer un estudio diferente :P)


#################### ITEM F ####################

set.seed(5)

f <- generar_promedios_uniformes(1200, 1000)

par(mfrow = c(2, 1))

he <- hist(e,
           freq = FALSE,
           main = "Histograma de promedio de quinientas uniformes",
           xlab = "Muestra x1...x1000",
           ylab = "Densidad",
           col = "#66B2FF",
           xlim = c(0.44, 0.56),
           breaks = seq(0.45, 0.55, by = 0.00625),
           ylim = c(0, 50))

abline(col = "red", h = max(he$density))

hf <- hist(f,
           freq = FALSE,
           main = "Histograma de promedio de mil doscientas uniformes",
           xlab = "Muestra x1...x1000",
           ylab = "Densidad",
           col = "#66B2FF",
           xlim = c(0.44, 0.56),
           breaks = seq(0.45, 0.55, by = 0.00625),
           ylim = c(0, 50))

abline(col = "red", h = max(hf$density))

par(mfrow = c(1, 1))

# Grafico los boxplots

datos_uniformes <- data.frame(a,b,c,d,e,f)

colnames(datos_uniformes) <- c("n=1", "n=2", "n=5", "n=30", "n=500", "n=1200")

boxplot(datos_uniformes, main = "Boxplots de promedios de U[0,1]")

abline(col = "red", h = 0.5)

# Calculemos la media y varianza muestral para cada conjunto de datos

media_a <- mean(a)
varianza_a <- varianza(a) # Usamos esta medida de varianza o usamos S^2 ?

media_b <- mean(b)
varianza_b <- varianza(b)

media_c <- mean(c)
varianza_c <- varianza(c)

media_d <- mean(d)
varianza_d <- varianza(d)

media_e <- mean(e)
varianza_e <- varianza(e)

media_f <- mean(f)
varianza_f <- varianza(f)

# Realizamos qqnorm para los seis conjuntos de datos

# Ponerles lindos nombres en los ejes ("Cuantiles Teóricos" y "Cuantiles Muestrales" estará bien? O es muy literal la traducción?)

par(mfrow = c(2, 3))

qqnorm(a, main = "QQPlot Normal n=1")
qqnorm(b, main = "QQPlot Normal n=2")
qqnorm(c, main = "QQPlot Normal n=5", ylim = c(0, 1))
qqnorm(d, main = "QQPlot Normal n=30", ylim = c(0.3, 0.7))
qqnorm(e, main = "QQPlot Normal n=500", ylim = c(0.3, 0.7))
qqnorm(f, main = "QQPlot Normal n=1200", ylim = c(0.3, 0.7))

par(mfrow = c(1, 1))

