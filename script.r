#####################################
# TP Probabilidad y Estadística (C) #
#####################################

# Dejar al menos un espacio al final del script para que al copiar y pegar en la consola del R se ejecuten todos los comandos :)

# Librer�as

library(latex2exp)
library(ggplot2)

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

varianza <- function(x) { # Calcula la varianza muestral insesgada S^2
  sum((x-mean(x))^2)/(length(x)-1)
}

normalizar <- function(x, n) { # Normaliza los promedios de n U[0,1]
  (x-0.5)/sqrt(1/(12*n)) # 0.5 es la esperanza de una U[0,1] y 1/12 es su respectiva varianza
}


#################### ITEM A ####################

set.seed(0) # Un seed para cada item (a = 0, b = 1, ...)

# Generate sample
a <- generar_promedios_uniformes(1, 1000)

# Make data frames :)
a.df = data.frame("data" = a)
a.density <- data.frame("density.x" = c(-0.1,0,1,1.1),
                        "density.y" = c(0,1,0,0))

# Now plot histogram
ha <- ggplot(a.df, aes(x=data, after_stat(density)))
ha + geom_histogram(breaks=seq(0,1,.1),
                 fill=I("#66B2FF"), 
                 col=I("black"),
                 alpha=I(.4)) +
     ggtitle(TeX("Histograma de muestra uniforme $X_1,...X_{1000}$")) +
     theme(plot.title = element_text(hjust = 0.5)) +
     labs(x=TeX("Muestra $X_i \\forall i\\in \\[1,1000\\]$"), y="Densidad") + 
     geom_step(data=a.density, mapping=aes(density.x, density.y), color='red', size=1) + 
     scale_x_continuous(limits=c(-.1,1.1)) +
     annotate("text", x=0.2, y=1.1, label=TeX("$f(x)\\,=\\,\\frac{1}{1\\,-\\,0} \\, $I$_{[0,1]}(x)\\,=\\, $I$_{[0,1]}(x)$"), col="red")

#################### ITEM B ####################

set.seed(1)

# Sample
b <- generar_promedios_uniformes(2, 1000)

# Make data frames :)
b.df = data.frame("data" = b)

# Now plot histogram
hb <- ggplot(b.df, aes(x=data, after_stat(density)))
hb + geom_histogram(breaks=seq(0,1,.1),
                    fill=I("#d26df7"), 
                    col=I("black"),
                    alpha=I(.4)) +
  ggtitle(TeX("Histograma de promedio $\\bar{X}_{2}$ de muestra uniforme $X_1,X_2$")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x=TeX("Muestra $\\bar{X}_2_i\\forall i\\in \\[1,1000\\]$"), y="Densidad") + 
  geom_step(data=a.density, mapping=aes(density.x, density.y), color='red', size=1) + 
  scale_x_continuous(limits=c(-.1,1.1)) +
  annotate("text", x=0.1, y=1.1, label=TeX("$f(x)\\,=\\, $I$_{[0,1]}(x)$"), col="red")

#################### ITEM C ####################

set.seed(2)

# Sample
c <- generar_promedios_uniformes(5, 1000)

# Make data frames :)
c.df = data.frame("data" = c)
cd.df = data.frame(Tama�o = factor(rep(c("2","5"), each=1000)), 
                   data = c(b,c))

# Now plot histogram
hc <- ggplot(c.df, aes(x=data, after_stat(density)))
hc + geom_histogram(breaks=seq(0,1,.1),
                    fill=I("#bb00ff"), 
                    col=I("black"),
                    alpha=I(.4)) +
  ggtitle(TeX("Histograma de promedio $\\bar{X}_{5}$ de muestra uniforme $X_1,...,X_5$")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x=TeX("Muestra $\\bar{X}_5_i\\forall i\\in \\[1,1000\\]$"), y="Densidad") + 
  geom_step(data=a.density, mapping=aes(density.x, density.y), color='red', size=1) + 
  scale_x_continuous(limits=c(-.1,1.1)) +
  annotate("text", x=0.1, y=1.1, label=TeX("$f(x)\\,=\\, $I$_{[0,1]}(x)$"), col="red")

# Make one plot with both histograms
hcb <- ggplot(cd.df, aes(x=data, fill=Tama�o, after_stat(density)))
hcb + geom_histogram(breaks=seq(0,1,.1),
                     col=I("black"),
                     position="dodge",
                     alpha=I(.4)) +
  scale_fill_manual(values=c("#d26df7", "#bb00ff")) +
  ggtitle(TeX("Histograma de promedios $\\bar{X}_{2},\\bar{X}_{5}$ de muestras uniformes $X_i$")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x=TeX("Muestras $\\\\bar{X}_{2}_i,\\, \\bar{X}_{5}_i\\forall i\\in \\[1,1000\\]$"), y="Densidad") +
  scale_x_continuous(limits=c(-.1,1.1))


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

#################### ITEM G ####################

# Normalizo los 6 conjuntos de datos
a_n <- normalizar(a, 1)
b_n <- normalizar(b, 2)
c_n <- normalizar(c, 5)
d_n <- normalizar(d, 30)
e_n <- normalizar(e, 500)
f_n <- normalizar(f, 1200)

par(mfrow = c(2, 3))

# Comparo los histogramas de las estandarizaciones contra la N(0,1)

# Le puse au a la variable ('a' del item, y 'u' de que vino de las uniformes, porque despues tenemos las que vienen de las cauchys)
hist(a_n, freq = F, xlim = c(-4, 4), breaks = seq(-4, 4, by = 0.5), ylim = c(0, 0.5), main = "n=1 normalizado", xlab = "au1...au1000", ylab = "Densidad", col = "#66B2FF")
curve(col = "red", dnorm(x), add = T)

hist(b_n, freq = F, xlim = c(-4, 4), breaks = seq(-4, 4, by = 0.5), ylim = c(0, 0.5), main = "n=2 normalizado", xlab = "bu1...bu1000", ylab = "Densidad", col = "#66B2FF")
curve(col = "red", dnorm(x), add = T)

hist(c_n, freq = F, xlim = c(-4, 4), breaks = seq(-4, 4, by = 0.5), ylim = c(0, 0.5), main = "n=5 normalizado", xlab = "cu1...cu1000", ylab = "Densidad", col = "#66B2FF")
curve(col = "red", dnorm(x), add = T)

hist(d_n, freq = F, xlim = c(-4, 4), breaks = seq(-4, 4, by = 0.5), ylim = c(0, 0.5), main = "n=30 normalizado", xlab = "du1...du1000", ylab = "Densidad", col = "#66B2FF")
curve(col = "red", dnorm(x), add = T)

hist(e_n, freq = F, xlim = c(-4, 4), breaks = seq(-4, 4, by = 0.5), ylim = c(0, 0.5), main = "n=500 normalizado", xlab = "eu1...eu1000", ylab = "Densidad", col = "#66B2FF")
curve(col = "red", dnorm(x), add = T)

hist(f_n, freq = F, xlim = c(-4, 4), breaks = seq(-4, 4, by = 0.5), ylim = c(0, 0.5), main = "n=1200 normalizado", xlab = "fu1...fu1000", ylab = "Densidad", col = "#66B2FF")
curve(col = "red", dnorm(x), add = T)

par(mfrow=c(1, 1))

set.seed(7)
normal_posta <- rnorm(1000) # Genero una muestra de N(0,1) para comparar con las estandarizaciones pero ahora con boxplots

uniformes_normalizadas <- data.frame(normal_posta, a_n, b_n, c_n, d_n, e_n, f_n)
colnames(uniformes_normalizadas) <- c("normal", "n=1", "n=2", "n=5", "n=30", "n=500", "n=1200")

boxplot(uniformes_normalizadas, col = c("yellow", rep("#66B2FF", times = 6)), main = "Boxplot Normal vs Estandarizaciones")
abline(col = "red", h= 0.6744898) # tercer cuartil de N(0,1)
abline(col = "red", h= -0.6744898) # primer cuartil de N(0,1)
abline(col = "red", h= 0) # segundo cuartil - mediana de N(0,1)

#################### ITEM H ####################

# FALTA HACER EL e Y EL f CON LA DENSIDAD DE CAUCHY, EL PUNTO g NO TIENE SENTIDO YA QUE LA CAUCHY NO CUENTA CON ESPERANZA NI VARIANZA FINITAS.
# YA HAY FUNCION PARA GENERAR LAS CAUCHYS (ESTÁ EN LAS FUNCIONES AUXILIARES), ASÍ QUE RESTA GENERAR CON ESA FUNCION LOS PROMEDIOS QUE PIDEN, HACER LOS GRAFICOS Y COMPARAR.

set.seed(8)

ca <- generar_promedios_cauchy(1, 1000)

hca <- hist(ca,
           freq = FALSE,
           main = "Histograma de muestra cauchy",
           xlab = "Muestra x1...x1000",
           ylab = "Densidad",
           col = "#66B2FF")
curve(dcauchy(x), add=T, col="red")


set.seed(9)

cb <- generar_promedios_cauchy(2, 1000)

hcb <- hist(cb,
           freq = FALSE,
           main = "Histograma de promedio de dos cauchy",
           xlab = "Muestra x1...x1000",
           ylab = "Densidad",
           col = "#66B2FF")


set.seed(10)

cc <- generar_promedios_cauchy(5, 1000)

par(mfrow = c(1, 2)) # Divido la pantalla en dos filas para comparar los histogramas

hcb <- hist(cb,
           freq = FALSE,
           main = "Histograma de promedio de dos cauchy",
           xlab = "Muestra x1...x1000",
           ylab = "Densidad",
           col = "#66B2FF",
           xlim = c(-400, 200)
           )

abline(col = "red", h = max(hcb$density))

hcc <- hist(cc,
           freq = FALSE,
           main = "Histograma de promedio de cinco cauchy",
           xlab = "Muestra x1...x1000",
           ylab = "Densidad",
           col = "#66B2FF",
           xlim = c(-400, 200)
           )

abline(col = "red", h = max(hcc$density))



set.seed(11)

cd <- generar_promedios_cauchy(30, 1000)

par(mfrow = c(1, 2))

hcc <- hist(cc,
           freq = FALSE,
           main = "Histograma de promedio de cinco cauchy",
           xlab = "Muestra x1...x1000",
           ylab = "Densidad",
           col = "#66B2FF",
           xlim = c(-600, 400),
           ylim = c(0, 0.01))


hcd <- hist(cd,
           freq = FALSE,
           main = "Histograma de promedio de treinta cauchy",
           xlab = "Muestra x1...x1000",
           ylab = "Densidad",
           col = "#66B2FF",
           xlim = c(-600, 400),
           ylim = c(0, 0.01))


