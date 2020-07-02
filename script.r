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
           xlim = c(0, 1),
           breaks = seq(0, 1, by = 0.1),
           ylim = c(0, 3)) # El 3 fue puesto luego de ver el valor máximo que alcanzaba el histograma

abline(col = "red", h = max(hb$density))

hc <- hist(c,
           freq = FALSE,
           main = "Histograma de promedio de cinco uniformes",
           xlab = "Muestra x1...x1000",
           ylab = "Densidad",
           col = "#66B2FF",
           xlim = c(0, 1),
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
           xlim = c(0, 1),
           breaks = seq(0, 1, by = 0.05),
           ylim = c(0, 7))

abline(col = "red", h = max(hc$density))

hd <- hist(d,
           freq = FALSE,
           main = "Histograma de promedio de treinta uniformes",
           xlab = "Muestra x1...x1000",
           ylab = "Densidad",
           col = "#66B2FF",
           xlim = c(0, 1),
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

# Ahora comparamos distintos tamaños de muestras con los promedios fijos (con histogramas y con boxplots)

mayores1 <- e
mayores2 <- generar_promedios_uniformes(500, 2000)
mayores3 <- generar_promedios_uniformes(500, 4000)
mayores4 <- generar_promedios_uniformes(500, 8000)
mayores5 <- generar_promedios_uniformes(500, 16000)
mayores6 <- generar_promedios_uniformes(500, 32000)

mayores <- data.frame(mayores1, mayores2, mayores3, mayores4, mayores5, mayores6)

colnames(mayores) <- c("m=1000", "m=2000", "m=4000", "m=8000", "m=16000", "m=32000")

par(mfrow = c(2, 3))

# Ponerles color, titulo y nombre a los ejes de los histogramas
hist(mayores1, freq = F, ylim = c(0, 30), xlim = c(0.44, 0.56), breaks = seq(0.44, 0.56, by = 0.0125))
hist(mayores3, freq = F, ylim = c(0, 30), xlim = c(0.44, 0.56), breaks = seq(0.44, 0.56, by = 0.0125))
hist(mayores5, freq = F, ylim = c(0, 30), xlim = c(0.44, 0.56), breaks = seq(0.44, 0.56, by = 0.0125))
hist(mayores2, freq = F, ylim =c (0, 30), xlim = c(0.44, 0.56), breaks = seq(0.44, 0.56, by = 0.0125))
hist(mayores4, freq = F, ylim = c(0, 30), xlim = c(0.44, 0.56), breaks = seq(0.44, 0.56, by = 0.0125))
hist(mayores6, freq = F, ylim = c(0, 30), xlim = c(0.44, 0.56), breaks = seq(0.44, 0.56, by = 0.0125))

par(mfrow = c(1, 1))

boxplot(mayores)

# No se si sirven estas lineas.. pero bueno xD
abline(col = "red", h = 0.49)
abline(col = "red", h = 0.5)
abline(col = "red", h = 0.51)

# Desde 1000 en adelante los histogramas son similares, si vamos a los boxplots son similares, pero los de mayor numero de muestras contienen más outliers, lo cual es obvio ya que mientras más muestras se tomen, mayor es la cantidad de datos que se pueden salir de los parámetros normales.

# Pero veamos que ocurre con muestras desde 5 hasta 1000:

menores1 <- generar_promedios_uniformes(500, 5)
menores2 <- generar_promedios_uniformes(500, 10)
menores3 <- generar_promedios_uniformes(500, 50)
menores4 <- generar_promedios_uniformes(500, 100)
menores5 <- generar_promedios_uniformes(500, 500)
menores6 <- generar_promedios_uniformes(500, 1000)

par(mfrow = c(2, 3))

# Ponerles color, titulo y nombre a los ejes de los histogramas
hist(menores1, freq = F, ylim = c(0, 35), xlim = c(0.44, 0.56))
hist(menores2, freq = F, ylim = c(0, 35), xlim = c(0.44, 0.56))
hist(menores3, freq = F, ylim = c(0, 35), xlim = c(0.44, 0.56))
hist(menores4, freq = F, ylim = c(0, 35), xlim = c(0.44, 0.56))
hist(menores5, freq = F, ylim = c(0, 35), xlim = c(0.44, 0.56))
hist(menores6, freq = F, ylim = c(0, 35), xlim = c(0.44, 0.56))

menores <- data.frame(menores1, menores2, menores3, menores4, menores5, menores6)

colnames(menores) <- c("m=5", "m=10", "m=50", "m=100", "m=500", "m=1000")

par(mfrow = c(1, 1))

boxplot(menores)


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

