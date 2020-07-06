#####################################
# TP Probabilidad y Estadistica (C) #
#####################################

# Dejar al menos un espacio al final del script para que al copiar y pegar en la consola del R se ejecuten todos los comandos :)

# Librerías

library(latex2exp)
library(ggplot2)

# Funciones Auxiliares

promedio_n <- function(n, gen) { # Calcula el promedio de n realizaciones de una variable aleatoria con distribuciÃ³n gen
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

podar <- function(x, p) { # Remueve el p% mas chico y mas grande del vector x
  ordenado <- x[order(x)]
  n <- length(ordenado)
  ordenado[((n*p/100)+1):(n-n*p/100)]
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
ha + geom_histogram(breaks=seq(0,1,.05),
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
hb + geom_histogram(breaks=seq(0,1,.05),
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
cb.df = data.frame(size = factor(rep(c("2","5"), each=1000)), 
                   data = c(b,c))

# Now plot histogram
hc <- ggplot(c.df, aes(x=data, after_stat(density)))
hc + geom_histogram(breaks=seq(0,1,.05),
                    fill=I("#d26df7"), 
                    col=I("black"),
                    alpha=I(.4)) +
  ggtitle(TeX("Histograma de promedio $\\bar{X}_{5}$ de muestra uniforme $X_1,...,X_5$")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x=TeX("Muestra $\\bar{X}_5_i\\forall i\\in \\[1,1000\\]$"), y="Densidad") + 
  geom_step(data=a.density, mapping=aes(density.x, density.y), color='red', size=1) + 
  scale_x_continuous(limits=c(-.1,1.1)) +
  annotate("text", x=0.1, y=1.1, label=TeX("$f(x)\\,=\\, $I$_{[0,1]}(x)$"), col="red")

# Make one plot with both histograms
hcb <- ggplot(cb.df, aes(x=data, fill=size, after_stat(density)))
hcb + geom_histogram(breaks=seq(0,1,.1),
                     col=I("black"),
                     position="dodge",
                     alpha=I(.4)) +
  scale_fill_brewer(palette="Greens") +
  ggtitle(TeX("Histograma de promedios $\\bar{X}_{2},\\bar{X}_{5}$ de muestras uniformes $X_i$")) +
  theme(plot.title = element_text(hjust = 0.5), legend.position=c(0.85,0.85)) +
  labs(x=TeX("Muestras $\\\\bar{X}_{2}_i,\\, \\bar{X}_{5}_i\\forall i\\in \\[1,1000\\]$"), y="Densidad", fill="Tama�o") +
  scale_x_continuous(limits=c(-.1,1.1))


#################### ITEM D ####################

set.seed(3)

# Sample
d <- generar_promedios_uniformes(30, 1000)

# Make data frames :)
d.df = data.frame("data" = d)
dcb.df = data.frame(size = factor(rep(c(2,5,30), each=1000)), 
                   data = c(b,c,d))

# Now plot histogram
hd <- ggplot(d.df, aes(x=data, after_stat(density)))
hd + geom_histogram(breaks=seq(0,1,.05),
                    fill=I("#d26df7"), 
                    col=I("black"),
                    alpha=I(.4)) +
  ggtitle(TeX("Histograma de promedio $\\bar{X}_{30}$ de muestra uniforme $X_1,...,X_{30}$")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x=TeX("Muestra $\\bar{X}_{30}_i\\forall i\\in \\[1,1000\\]$"), y="Densidad") + 
  geom_step(data=a.density, mapping=aes(density.x, density.y), color='red', size=1) + 
  scale_x_continuous(limits=c(-.1,1.1)) +
  annotate("text", x=0.1, y=1.3, label=TeX("$f(x)\\,=\\, $I$_{[0,1]}(x)$"), col="red")

# Make one plot with all histograms
hdcb <- ggplot(dcb.df, aes(x=data, fill=size, after_stat(density)))
hdcb + geom_histogram(breaks=seq(0,1,.1),
                      col=I("black"),
                      position="dodge",
                      alpha=I(.4)) +
  scale_fill_brewer(palette="Greens") +
  ggtitle(TeX("Histograma de promedios $\\bar{X}_{2},\\bar{X}_{5}, \\bar{X}_{30}$ de muestras uniformes $X_i$")) +
  theme(plot.title = element_text(hjust = 0.5), legend.position=c(0.85,0.85)) +
  labs(x=TeX("Muestras $\\\\bar{X}_{2}_i,\\, \\bar{X}_{5}_i\\, \\bar{X}_{30}_i\\forall i\\in \\[1,1000\\]$"), y="Densidad", fill="Tama�o") +
  scale_x_continuous(limits=c(-.1,1.1))


#################### ITEM E ####################

set.seed(4)

# Sample
e <- generar_promedios_uniformes(500, 1000)

# Make data frames :)
e.df = data.frame("data" = e)
edcb.df = data.frame(size = factor(rep(c(2,5,30,500), each=1000)), 
                    data = c(b,c,d,e))

# Now plot histogram
he <- ggplot(e.df, aes(x=data, after_stat(density)))
he + geom_histogram(breaks=seq(0,1,.05),
                    fill=I("#d26df7"), 
                    col=I("black"),
                    alpha=I(.4)) +
  ggtitle(TeX("Histograma de promedio $\\bar{X}_{500}$ de muestra uniforme $X_1,...,X_{500}$")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x=TeX("Muestra $\\bar{X}_{500}_i\\forall i\\in \\[1,1000\\]$"), y="Densidad") + 
  geom_step(data=a.density, mapping=aes(density.x, density.y), color='red', size=1) + 
  scale_x_continuous(limits=c(-.1,1.1)) +
  annotate("text", x=0.1, y=1.3, label=TeX("$f(x)\\,=\\, $I$_{[0,1]}(x)$"), col="red")

# Make one plot with all histograms
hedcb <- ggplot(edcb.df, aes(x=data, fill=size, after_stat(density)))
hedcb + geom_histogram(breaks=seq(0,1,.1),
                       col=I("black"),
                       position="dodge",
                       alpha=I(.4)) +
  scale_fill_brewer(palette="Greens") +
  ggtitle(TeX("Histograma de promedios $\\bar{X}_{2},\\bar{X}_{5}, \\bar{X}_{30}, \\bar{X}_{500}$ de muestras uniformes $X_i$")) +
  theme(plot.title = element_text(hjust = 0.9), legend.position=c(0.85,0.8)) +
  labs(x=TeX("Muestras $\\\\bar{X}_{2}_i,\\, \\bar{X}_{5}_i\\, \\bar{X}_{30}_i\\, \\bar{X}_{500}_i\\forall i\\in \\[1,1000\\]$"), 
       y="Densidad", fill="Tama�o") +
  scale_x_continuous(limits=c(-.1,1.1))

#################### ITEM E (PARTE 2) ####################

# Ahora comparamos distintos tamaños de muestras con los promedios fijos (con histogramas y con boxplots)
mayores1 <- e
mayores2 <- generar_promedios_uniformes(500, 2000)
mayores3 <- generar_promedios_uniformes(500, 4000)
mayores4 <- generar_promedios_uniformes(500, 8000)
mayores5 <- generar_promedios_uniformes(500, 16000)
mayores6 <- generar_promedios_uniformes(500, 32000)

# Make data frame
mayores.df <- data.frame(size = c(rep(1000, 1000),
                                    rep(2000, 2000),
                                    rep(4000, 4000),
                                    rep(8000, 8000),
                                    rep(16000, 16000),
                                    rep(32000, 32000)),
                         data = c(mayores1,
                                  mayores2,
                                  mayores3,
                                  mayores4,
                                  mayores5,
                                  mayores6),
                         sizeStr = c(rep("1000", 1000),
                                       rep("2000", 2000),
                                       rep("4000", 4000),
                                       rep("8000", 8000),
                                       rep("16000", 16000),
                                       rep("32000", 32000)))

# Now plot histograms in several subplots with the same scale
hmay <- ggplot(mayores.df, aes(x=data, fill=fct_reorder(sizeStr,size), after_stat(density)))
hmay + geom_histogram(breaks=seq(min(mayores.df$data), max(mayores.df$data), length.out=21),
                    col=I("black"),
                    alpha=I(.4)) +
  facet_grid(. ~ size) +
  scale_fill_brewer(palette="Spectral") +
  ggtitle(TeX("Histograma de promedio $\\bar{X}_{500}$ de muestra uniforme $X_1,...,X_{500}$")) +
  theme(plot.title = element_text(hjust = 0.3)) +
  labs(x=TeX("Muestra $\\bar{X}_{500}_i\\forall i\\in \\[1,size\\]$"), y="Densidad", fill="Tama�o")
# Save with width 888, height 350 :)

# Then make a boxplot with all of them in the same scale
bmay <- ggplot(mayores.df, aes(x=fct_reorder(sizeStr,size), y=data, group=size)) + 
  geom_boxplot(aes(fill=fct_reorder(sizeStr,size))) +
  scale_fill_brewer(palette="Spectral") +
  ggtitle(TeX("Boxplots de promedio $\\bar{X}_{500}$ de muestra uniforme $X_1,...,X_{500}$")) +
  labs(x=TeX("Muestra $\\bar{X}_{500}_i\\forall i\\in \\[1,size\\]$"), y="Densidad", fill="Tama�o") +
  theme(plot.title = element_text(hjust = 0.5))
bmay

# Desde 1000 en adelante los histogramas son similares, si vamos a los boxplots son similares, pero los de mayor numero de muestras contienen mÃ¡s outliers, lo cual es obvio ya que mientras mÃ¡s muestras se tomen, mayor es la cantidad de datos que se pueden salir de los parÃ¡metros normales.

# Pero veamos que ocurre con muestras desde 5 hasta 1000:
menores1 <- generar_promedios_uniformes(500, 5)
menores2 <- generar_promedios_uniformes(500, 10)
menores3 <- generar_promedios_uniformes(500, 50)
menores4 <- generar_promedios_uniformes(500, 100)
menores5 <- generar_promedios_uniformes(500, 500)
menores6 <- generar_promedios_uniformes(500, 1000)

# Make data frame
menores.df <- data.frame(size = c(rep(5, 5),
                                    rep(10, 10),
                                    rep(50, 50),
                                    rep(100, 100),
                                    rep(500, 500),
                                    rep(1000, 1000)),
                         data = c(menores1,
                                  menores2,
                                  menores3,
                                  menores4,
                                  menores5,
                                  menores6),
                         sizeStr = c(rep("5", 5),
                                       rep("10", 10),
                                       rep("50", 50),
                                       rep("100", 100),
                                       rep("500", 500),
                                       rep("1000", 1000)))

# Now plot histograms in several subplots with the same scale
hmen <- ggplot(menores.df, aes(x=data, fill=fct_reorder(sizeStr,size), after_stat(density)))
hmen + geom_histogram(breaks=seq(min(menores.df$data), max(menores.df$data), length.out=21),
                      col=I("black"),
                      alpha=I(.4)) +
  facet_grid(. ~ size) +
  scale_fill_brewer(palette="Spectral", direction=-1) +
  ggtitle(TeX("Histograma de promedio $\\bar{X}_{500}$ de muestra uniforme $X_1,...,X_{500}$")) +
  theme(plot.title = element_text(hjust = 0.3)) +
  labs(x=TeX("Muestra $\\bar{X}_{500}_i\\forall i\\in \\[1,size\\]$"), y="Densidad", fill="Tama�o")
# Save with width 888, height 350 :)

# Then make a boxplot with all of them in the same scale
bmen <- ggplot(menores.df, aes(x=fct_reorder(sizeStr,size), y=data, group=size)) + 
  geom_boxplot(aes(fill=fct_reorder(sizeStr,size))) +
  scale_fill_brewer(palette="Spectral", direction=-1) +
  ggtitle(TeX("Boxplots de promedio $\\bar{X}_{500}$ de muestra uniforme $X_1,...,X_{500}$")) +
  labs(x=TeX("Muestra $\\bar{X}_{500}_i\\forall i\\in \\[1,size\\]$"), y="Densidad", fill="Tama�o") +
  theme(plot.title = element_text(hjust = 0.5))
bmen

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
varianza_a <- varianza(a)

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

# Ponerles lindos nombres en los ejes ("Cuantiles TeÃ³ricos" y "Cuantiles Muestrales" estarÃ¡ bien? O es muy literal la traducciÃ³n?)

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
# YA HAY FUNCION PARA GENERAR LAS CAUCHYS (ESTÃ EN LAS FUNCIONES AUXILIARES), ASÃ QUE RESTA GENERAR CON ESA FUNCION LOS PROMEDIOS QUE PIDEN, HACER LOS GRAFICOS Y COMPARAR.

set.seed(8)

ca <- generar_promedios_cauchy(1, 1000)

hca <- hist(podar(ca, 2), # Podo el 2% de los datos mas grandes y mas chicos
           freq = FALSE,
           main = "Histograma de muestra cauchy",
           xlab = "Muestra x1...x1000",
           ylab = "Densidad",
           col = "#66B2FF",
           xlim = c(-18, 18),
           ylim = c(0,0.2))
curve(dcauchy(x), add = T, col = "red")


set.seed(9)

cb <- generar_promedios_cauchy(2, 1000)

hcb <- hist(podar(cb, 2),
           freq = FALSE,
           main = "Histograma de promedio de dos cauchy",
           xlab = "Muestra x1...x1000",
           ylab = "Densidad",
           col = "#66B2FF",
           xlim = c(-18, 18),
           ylim = c(0, 0.2))


set.seed(10)

cc <- generar_promedios_cauchy(5, 1000)

par(mfrow = c(1, 2)) # Divido la pantalla en dos filas para comparar los histogramas

hcb <- hist(podar(cb, 2),
           freq = FALSE,
           main = "Histograma de promedio de dos cauchy",
           xlab = "Muestra x1...x1000",
           ylab = "Densidad",
           col = "#66B2FF",
           xlim = c(-18, 18),
           ylim = c(0, 0.2))

hcc <- hist(podar(cc, 2),
           freq = FALSE,
           main = "Histograma de promedio de cinco cauchy",
           xlab = "Muestra x1...x1000",
           ylab = "Densidad",
           col = "#66B2FF",
           xlim = c(-18, 18),
           ylim = c(0, 0.2))


set.seed(11)

cd <- generar_promedios_cauchy(30, 1000)

par(mfrow = c(1, 2))

hcc <- hist(podar(cc, 2),
           freq = FALSE,
           main = "Histograma de promedio de cinco cauchy",
           xlab = "Muestra x1...x1000",
           ylab = "Densidad",
           col = "#66B2FF",
           xlim = c(-18, 18),
           breaks = seq(-18, 18, by = 2),
           ylim = c(0, 0.2))


hcd <- hist(podar(cd, 2),
           freq = FALSE,
           main = "Histograma de promedio de treinta cauchy",
           xlab = "Muestra x1...x1000",
           ylab = "Densidad",
           col = "#66B2FF",
           xlim = c(-18, 18),
           breaks = seq(-18, 18, by = 2),
           ylim = c(0, 0.2))


set.seed(12)

ce <- generar_promedios_cauchy(500, 1000)

par(mfrow = c(1, 2))

# Si no se podan los datos hay outliers muy grandes que desvirtuan todo

hcd <- hist(podar(cd, 2),
           freq = FALSE,
           main = "n = 30",
           xlab = "Muestra x1...x1000",
           ylab = "Densidad",
           col = "#66B2FF",
           xlim = c(-18, 18),
           breaks = seq(-18, 18, by = 2),
           ylim = c(0, 0.2))


hce <- hist(podar(ce, 2),
           freq = FALSE,
           main = "n = 500",
           xlab = "Muestra x1...x1000",
           ylab = "Densidad",
           col = "#66B2FF",
           xlim = c(-18, 18),
           breaks = seq(-18, 18, by = 2),
           ylim = c(0, 0.2))


mayores1 <- ce
mayores2 <- generar_promedios_cauchy(500, 2000)
mayores3 <- generar_promedios_cauchy(500, 4000)
mayores4 <- generar_promedios_cauchy(500, 8000)
mayores5 <- generar_promedios_cauchy(500, 16000)
mayores6 <- generar_promedios_cauchy(500, 32000)

mayores <- data.frame(podar(mayores1, 2), podar(mayores2, 2), podar(mayores3, 2), podar(mayores4, 2), podar(mayores5, 2), podar(mayores6, 2))

colnames(mayores) <- c("m=1000", "m=2000", "m=4000", "m=8000", "m=16000", "m=32000")

par(mfrow = c(2, 3))

# Ponerles titulo y nombre a los ejes de los histogramas

hist(podar(mayores1, 2),
     xlim = c(-18, 18),
     breaks = seq(-18, 18, by = 2),
     ylim = c(0,0.2),
     col = "#66B2FF",
     freq = F)

hist(podar(mayores2, 2),
     xlim = c(-18, 18),
     breaks = seq(-18, 18, by = 2),
     ylim = c(0,0.2),
     col = "#66B2FF",
     freq = F)

hist(podar(mayores3, 2),
     xlim = c(-18, 18),
     breaks = seq(-18, 18, by = 2),
     ylim = c(0,0.2),
     col = "#66B2FF",
     freq = F)

hist(podar(mayores4, 2),
     xlim = c(-18, 18),
     breaks = seq(-18, 18, by = 2),
     ylim = c(0,0.2),
     col = "#66B2FF",
     freq = F)

hist(podar(mayores5, 2),
     xlim = c(-18, 18),
     breaks = seq(-18, 18, by = 2),
     ylim = c(0,0.2),
     col = "#66B2FF",
     freq = F)

hist(podar(mayores6, 2),
     xlim = c(-18, 18),
     breaks = seq(-18, 18, by = 2),
     ylim = c(0,0.2),
     col = "#66B2FF",
     freq = F)

# Vemos que a medida que crece el numero de muestras, las colas se van agrandando muy de a poco.
# Tambien aparecen muchos mas outliers, pero para que no desvirtuen los graficos, estos fueron podados.


par(mfrow = c(1, 1))

boxplot(mayores)

# Observamos que a medida que crece la muestra considerada, aumenta la cantidad de outliers, pero las distancias intercuartiles se mantienen "constantes"

# Analizamos lo que ocurre cuando aumenta la cantidad de muestras a partir de un m chico (m=5)

menores1 <- generar_promedios_cauchy(500, 5)
menores2 <- generar_promedios_cauchy(500, 10)
menores3 <- generar_promedios_cauchy(500, 50)
menores4 <- generar_promedios_cauchy(500, 100)
menores5 <- generar_promedios_cauchy(500, 500)
menores6 <- generar_promedios_cauchy(500, 1000)

par(mfrow = c(2, 3))

hist(menores1[menores1 > -15 & menores1 < 15],
     freq = F,
     xlim = c(-15, 15),
     breaks = seq(-15, 15, by = 2.5),
     ylim = c(0, 0.25),
     ylab = "Densidad",
     xlab = "Muestra x1...x5",
     col = "#66B2FF",
     main = "")
curve(col = "red", dcauchy(x), add = T)

hist(menores2[menores2 > -15 & menores2 < 15],
     freq = F,
     xlim = c(-15, 15),
     breaks = seq(-15, 15, by = 2.5),
     ylim = c(0, 0.25),
     ylab = "Densidad",
     xlab = "Muestra x1...x5",
     col = "#66B2FF",
     main = "")
curve(col = "red", dcauchy(x), add = T)

hist(menores3[menores3 > -15 & menores3 < 15],
     freq = F,
     xlim = c(-15, 15),
     breaks = seq(-15, 15, by = 2.5),
     ylim = c(0, 0.25),
     ylab = "Densidad",
     xlab = "Muestra x1...x5",
     col = "#66B2FF",
     main = "")
curve(col = "red", dcauchy(x), add = T)

hist(menores4[menores4 > -15 & menores4 < 15],
     freq = F,
     xlim = c(-15, 15),
     breaks = seq(-15, 15, by = 2.5),
     ylim = c(0, 0.25),
     ylab = "Densidad",
     xlab = "Muestra x1...x5",
     col = "#66B2FF",
     main = "")
curve(col = "red", dcauchy(x), add = T)

hist(menores5[menores5 > -15 & menores5 < 15],
     freq = F,
     xlim = c(-15, 15),
     breaks = seq(-15, 15, by = 2.5),
     ylim = c(0, 0.25),
     ylab = "Densidad",
     xlab = "Muestra x1...x5",
     col = "#66B2FF",
     main = "")
curve(col = "red", dcauchy(x), add = T)

hist(menores6[menores6 > -15 & menores6 < 15],
     freq = F,
     xlim = c(-15, 15),
     breaks = seq(-15, 15, by = 2.5),
     ylim = c(0, 0.25),
     ylab = "Densidad",
     xlab = "Muestra x1...x5",
     col = "#66B2FF",
     main = "")
curve(col = "red", dcauchy(x), add = T)


menores <- data.frame(menores1, menores2, menores3, menores4, menores5, menores6)

colnames(menores) <- c("m=5", "m=10", "m=50", "m=100", "m=500", "m=1000")

par(mfrow = c(1, 1))

boxplot(menores, ylim=c(-10,10))

set.seed(13)

cf <- generar_promedios_cauchy(1200, 1000)

par(mfrow = c(1, 2))

hce <- hist(podar(ce, 2),
           freq = FALSE,
           main = "n = 30",
           xlab = "Muestra x1...x1000",
           ylab = "Densidad",
           col = "#66B2FF",
           xlim = c(-18, 18),
           breaks = seq(-18, 18, by = 2),
           ylim = c(0, 0.25))


hcf <- hist(podar(cf, 2),
           freq = FALSE,
           main = "n = 500",
           xlab = "Muestra x1...x1000",
           ylab = "Densidad",
           col = "#66B2FF",
           xlim = c(-18, 18),
           breaks = seq(-18, 18, by = 2),
           ylim = c(0, 0.25))

par(mfrow = c(1, 1))
datos_cauchy <- data.frame(ca,cb,cc,cd,ce,cf)
colnames(datos_cauchy) <- c("n=1", "n=2", "n=5", "n=30", "n=500", "n=1200")

boxplot(datos_cauchy, main = "Boxplots de promedios de C[0,1]", ylim = c(-15, 15))

# Vemos que los histogramas son todos muy similares, lo que da a entender que con esta distribucion no ocurre que el promedio se va concentrando en la media como si ocurria con las U(0,1). Ademas hay que notar que no tiene media definida.


media_ca <- mean(ca)
varianza_ca <- varianza(ca)

media_cb <- mean(cb)
varianza_cb <- varianza(cb)

media_cc <- mean(cc)
varianza_cc <- varianza(cc)

media_cd <- mean(cd)
varianza_cd <- varianza(cd)

media_ce <- mean(ce)
varianza_ce <- varianza(ce)

media_cf <- mean(cf)
varianza_cf <- varianza(cf)

# Tengamos en cuenta que estas medias y varianzas son muestrales. Las teoricas no existen y por eso no se parecen en nada las medias y varianzas a medida que crecen los promedios. No tienden a ningun valor.

par(mfrow = c(2, 3))

qqnorm(podar(ca, 2), main = "QQPlot Normal n=1")
qqnorm(podar(cb, 2), main = "QQPlot Normal n=2")
qqnorm(podar(cc, 2), main = "QQPlot Normal n=5")
qqnorm(podar(cd, 2), main = "QQPlot Normal n=30")
qqnorm(podar(ce, 2), main = "QQPlot Normal n=500")
qqnorm(podar(cf, 2), main = "QQPlot Normal n=1200")

par(mfrow = c(1, 1))

# Estos resultados anteriores son esperables ya que las cauchy nunca van a tender a una N(0,1) como si ocurre con las uniformes. En el caso de las uniformes los qqnorm daban una recta porque había mucha "correlacion" entre ambas distribuciones.

