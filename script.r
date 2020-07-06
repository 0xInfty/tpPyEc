#####################################
# TP Probabilidad y Estadistica (C) #
#####################################

# Dejar al menos un espacio al final del script para que al copiar y pegar en la consola del R se ejecuten todos los comandos :)

# Librer√≠as

library(latex2exp)
library(ggplot2)

# Funciones Auxiliares

promedio_n <- function(n, gen) { # Calcula el promedio de n realizaciones de una variable aleatoria con distribuci√É¬≥n gen
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
  labs(x=TeX("Muestras $\\\\bar{X}_{2}_i,\\, \\bar{X}_{5}_i\\forall i\\in \\[1,1000\\]$"), y="Densidad", fill="TamaÒo") +
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
  labs(x=TeX("Muestras $\\\\bar{X}_{2}_i,\\, \\bar{X}_{5}_i\\, \\bar{X}_{30}_i\\forall i\\in \\[1,1000\\]$"), y="Densidad", fill="TamaÒo") +
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
       y="Densidad", fill="TamaÒo") +
  scale_x_continuous(limits=c(-.1,1.1))

#################### ITEM E (PARTE 2) ####################

# Ahora comparamos distintos tama√±os de muestras con los promedios fijos (con histogramas y con boxplots)
mayores.df <- data.frame(size = c(rep(1000, 1000),
                                  rep(2000, 2000),
                                  rep(4000, 4000),
                                  rep(8000, 8000),
                                  rep(16000, 16000),
                                  rep(32000, 32000)),
                         data = c(e,
                                  generar_promedios_uniformes(500, 2000),
                                  generar_promedios_uniformes(500, 4000),
                                  generar_promedios_uniformes(500, 8000),
                                  generar_promedios_uniformes(500, 16000),
                                  generar_promedios_uniformes(500, 32000)),
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
  labs(x=TeX("Muestra $\\bar{X}_{500}_i\\forall i\\in \\[1,size\\]$"), y="Densidad", fill="TamaÒo")
# Save with width 888, height 350 :)

# Then make a boxplot with all of them in the same scale
bmay <- ggplot(mayores.df, aes(x=fct_reorder(sizeStr,size), y=data, group=size)) + 
  geom_boxplot(aes(fill=fct_reorder(sizeStr,size))) +
  scale_fill_brewer(palette="Spectral") +
  ggtitle(TeX("Boxplots de promedio $\\bar{X}_{500}$ de muestra uniforme $X_1,...,X_{500}$")) +
  labs(x=TeX("Muestra $\\bar{X}_{500}_i\\forall i\\in \\[1,size\\]$"), y="Densidad", fill="TamaÒo") +
  theme(plot.title = element_text(hjust = 0.5))
bmay

# Desde 1000 en adelante los histogramas son similares, si vamos a los boxplots son similares, pero los de mayor numero de muestras contienen m√É¬°s outliers, lo cual es obvio ya que mientras m√É¬°s muestras se tomen, mayor es la cantidad de datos que se pueden salir de los par√É¬°metros normales.

# Pero veamos que ocurre con muestras desde 5 hasta 1000:
menores.df <- data.frame(size = c(rep(5, 5),
                                  rep(10, 10),
                                  rep(50, 50),
                                  rep(100, 100),
                                  rep(500, 500),
                                  rep(1000, 1000)),
                         data = c(generar_promedios_uniformes(500, 5),
                                  generar_promedios_uniformes(500, 10),
                                  generar_promedios_uniformes(500, 50),
                                  generar_promedios_uniformes(500, 100),
                                  generar_promedios_uniformes(500, 500),
                                  generar_promedios_uniformes(500, 1000)),
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
  labs(x=TeX("Muestra $\\bar{X}_{500}_i\\forall i\\in \\[1,size\\]$"), y="Densidad", fill="TamaÒo")
# Save with width 888, height 350 :)

# Then make a boxplot with all of them in the same scale
bmen <- ggplot(menores.df, aes(x=fct_reorder(sizeStr,size), y=data, group=size)) + 
  geom_boxplot(aes(fill=fct_reorder(sizeStr,size))) +
  scale_fill_brewer(palette="Spectral", direction=-1) +
  ggtitle(TeX("Boxplots de promedio $\\bar{X}_{500}$ de muestra uniforme $X_1,...,X_{500}$")) +
  labs(x=TeX("Muestra $\\bar{X}_{500}_i\\forall i\\in \\[1,size\\]$"), y="Densidad", fill="TamaÒo") +
  theme(plot.title = element_text(hjust = 0.5))
bmen

#################### ITEM F ####################

set.seed(5)

f <- generar_promedios_uniformes(1200, 1000)

# Make data frames :)
f.df = data.frame("data" = f)
fedcb.df = data.frame(size = factor(rep(c(2,5,30,500,1200), each=1000)), 
                      sizeStr = factor(rep(c("2","5","30","500","1200"), each=1000)), 
                      data = c(b,c,d,e,f))

# Now plot histogram
hf <- ggplot(f.df, aes(x=data, after_stat(density)))
hf + geom_histogram(breaks=seq(0,1,.05),
                    fill=I("#d26df7"), 
                    col=I("black"),
                    alpha=I(.4)) +
  ggtitle(TeX("Histograma de promedio $\\bar{X}_{1200}$ de muestra uniforme $X_1,...,X_{1200}$")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x=TeX("Muestra $\\bar{X}_{1200}_i\\forall i\\in \\[1,1000\\]$"), y="Densidad") + 
  geom_step(data=a.density, mapping=aes(density.x, density.y), color='red', size=1) + 
  scale_x_continuous(limits=c(-.1,1.1)) +
  annotate("text", x=0.1, y=1.3, label=TeX("$f(x)\\,=\\, $I$_{[0,1]}(x)$"), col="red")

# Make one plot with all histograms
hfedcb <- ggplot(fedcb.df, aes(x=data, fill=size, after_stat(density)))
hfedcb + geom_histogram(breaks=seq(0,1,.1),
                       col=I("black"),
                       position="dodge",
                       alpha=I(.4)) +
  scale_fill_brewer(palette="Greens") +
  ggtitle(TeX("Histograma de promedios $\\bar{X}_{2},\\bar{X}_{5}, \\bar{X}_{30}, \\bar{X}_{500}, \\bar{X}_{1200}$ de muestras uniformes $X_i$")) +
  theme(plot.title = element_text(hjust = 0.9), legend.position=c(0.85,0.8)) +
  labs(x=TeX("Muestras $\\\\bar{X}_{2}_i,\\, \\bar{X}_{5}_i,\\, \\bar{X}_{30}_i,\\, \\bar{X}_{500}_i,\\, \\bar{X}_{1200}_i\\forall i\\in \\[1,1000\\]$"), 
       y="Densidad", fill="TamaÒo") +
  scale_x_continuous(limits=c(-.1,1.1))

# Grafico los boxplots
bf <- ggplot(fedcb.df, aes(x=size, y=data, group=size)) + 
  geom_boxplot(aes(fill=size), alpha=0.5) +
  scale_fill_brewer(palette="YlOrRd") +
  ggtitle(TeX("Boxplots de promedio $\\bar{X}_{n}$ de muestra uniforme $X_1,...,X_{n}$")) +
  labs(x=TeX("Muestra $\\bar{X}_{n}_i\\forall i\\in \\[1,1000\\]$"), y="Densidad", fill="n") +
  theme(plot.title = element_text(hjust = 0.5))
  bf

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

# Make data frame for it all
fedcba.df = data.frame(size = factor(rep(c(1,2,5,30,500,1200), each=1000)), 
                       sizeStr = factor(rep(c("1","2","5","30","500","1200"), each=1000)), 
                       data = c(a,b,c,d,e,f))

# Realizamos qqnorm para los seis conjuntos de datos
qqf <- ggplot(fedcba.df, aes(sample=data, color=size, group=size))
qqf + geom_qq(alpha=1) + 
  facet_grid(size ~ .) +
  scale_color_brewer(palette="YlOrRd") +
  ggtitle(TeX("QQnorm para promedio $\\bar{X}_{n}$ de muestra uniforme $X_1,...,X_{n}$")) +
  theme(plot.title = element_text(hjust = 0.3)) +
  labs(x="Cuantiles teÛricos", y=TeX("Cuantiles de la muestra $\\bar{X}_{n}_i\\forall i\\in \\[1,1000\\]$"), color="n")
# Save with width 888, height 350 :) 

#################### ITEM G ####################

# Normalizo los 6 conjuntos de datos
a_n <- normalizar(a, 1)
b_n <- normalizar(b, 2)
c_n <- normalizar(c, 5)
d_n <- normalizar(d, 30)
e_n <- normalizar(e, 500)
f_n <- normalizar(f, 1200)

# Make data.frame
g.df = data.frame(size = factor(rep(c(1,2,5,30,500,1200), each=1000)), 
                  sizeStr = factor(rep(c("1","2","5","30","500","1200"), each=1000)), 
                  data = c(a_n,b_n,c_n,d_n,e_n,f_n))
norm.df = data.frame(x = rep(seq(-4, 4, length.out=1000),6),
                     y = rep(dnorm(seq(-4, 4, length.out=1000)),6),
                     size = factor(rep(c(1,2,5,30,500,1200), each=1000)),
                     line.size = rep(rep(1, length(seq(-4,4,length.out=1000))),6))

# Comparo los histogramas de las estandarizaciones contra la N(0,1)

# Le puse au a la variable ('a' del item, y 'u' de que vino de las uniformes, porque despues tenemos las que vienen de las cauchys)

# Now plot histograms in several subplots with the same scale
hg <- ggplot(g.df, aes(x=data, fill=size, after_stat(density)))
hg + geom_histogram(breaks=seq(-4, 4, 0.5),
                      col=I("black"),
                      alpha=I(.4)) +
  facet_grid(. ~ size) +
  scale_fill_brewer(palette="YlOrRd") +
  ggtitle(TeX("Histograma de promedio $\\bar{X}_{500}$ de muestra uniforme $X_1,...,X_{500}$")) +
  theme(plot.title = element_text(hjust = 0.3)) +
  labs(x=TeX("Muestra $\\bar{X}_{500}_i\\forall i\\in \\[1,size\\]$"), y="Densidad", fill="TamaÒo") +
  geom_line(norm.df, mapping=aes(x=x, y=y, group=size), color='purple', size=1.5, alpha=0.8)
# Save with width 888, height 350 :)

# Genero una muestra de N(0,1) para comparar con las estandarizaciones pero ahora con boxplots
set.seed(7)

gn.df = data.frame(size = factor(rep(c(1,2,5,30,500,1200,2000), each=1000)), 
                   sizeStr = factor(rep(c("1","2","5","30","500","1200","N(0,1)"), each=1000)), 
                   data = c(a_n,b_n,c_n,d_n,e_n,f_n,rnorm(1000)))

# Grafico los boxplots
bgn <- ggplot(gn.df, aes(x=size, y=data, group=sizeStr)) + 
  geom_boxplot(aes(fill=size), alpha=0.5) +
  scale_fill_brewer(palette="YlGnBu") +
  ggtitle(TeX("Normal $N(0,1)$ vs Promedio $\\bar{X}_{n}$ de muestra uniforme $X_i \\sim U(0,1)$")) +
  labs(x=TeX("Muestra $\\bar{X}_{n}_i\\forall i\\in \\[1,1000\\]$"), y="Densidad", fill="n") +
  theme(plot.title = element_text(hjust = 0.4)) +
  geom_hline(yintercept=qnorm(.75), color="red") + # tercer cuartil de N(0,1)
  geom_hline(yintercept=qnorm(.5), color="red") + # segundo cuartil - mediana de N(0,1)
  geom_hline(yintercept=qnorm(.25), color="red") # primer cuartil de N(0,1)
bgn
# OJO QUE A ESTE LO EDIT… M¡S TARDE, JE

#################### ITEM H (ITEMS A-D) ####################

# FALTA HACER EL e Y EL f CON LA DENSIDAD DE CAUCHY, EL PUNTO g NO TIENE SENTIDO YA QUE LA CAUCHY NO CUENTA CON ESPERANZA NI VARIANZA FINITAS.
# YA HAY FUNCION PARA GENERAR LAS CAUCHYS (EST√É¬Å EN LAS FUNCIONES AUXILIARES), AS√É¬ç QUE RESTA GENERAR CON ESA FUNCION LOS PROMEDIOS QUE PIDEN, HACER LOS GRAFICOS Y COMPARAR.

# Generate sample
set.seed(8)
ca <- generar_promedios_cauchy(1, 1000)
set.seed(9)
cb <- generar_promedios_cauchy(2, 1000)
set.seed(10)
cc <- generar_promedios_cauchy(5, 1000)
set.seed(11)
cd <- generar_promedios_cauchy(30, 1000)
set.seed(12)
ce <- generar_promedios_cauchy(500, 1000)
set.seed(13)
cf <- generar_promedios_cauchy(1200, 1000)

# Make data frames :)
# cap.df = data.frame("data" = podar(ca, 2)) # Podo el 2% de los datos mas grandes y mas chicos
# Si no se podan los datos hay outliers muy grandes que desvirtuan todo

ca.df = data.frame("data" = ca) # Podo el 2% de los datos mas grandes y mas chicos
cb.df = data.frame("data" = cb)
cc.df = data.frame("data" = cc)
cd.df = data.frame("data" = cd)
ce.df = data.frame("data" = ce)
cf.df = data.frame("data" = cf)

ac.density <- data.frame("density.x" = seq(-18,18,length.out=1000),
                         "density.y" = dcauchy(seq(-18,18,length.out=1000)))

# Now plot single full histogram, showing outliers
hca <- ggplot(ca.df, aes(x=data, after_stat(density)))
hca + geom_histogram(breaks=seq(min(ca),max(ca),length.out=21),
                     fill=I("gold1"), 
                     col=I("black"),
                     alpha=I(.4)) +
  ggtitle(TeX("Histograma completo de muestra de Cauchy $X_1,...X_{1000}$")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x=TeX("Muestra $X_i \\forall i\\in \\[1,1000\\]$"), y="Densidad")

# Now plot single histogram
hca <- ggplot(ca.df, aes(x=data, after_stat(density)))
hca + geom_histogram(breaks=seq(-18,18,length.out=21),
                     fill=I("gold1"), 
                     col=I("black"),
                     alpha=I(.4)) +
  ggtitle(TeX("Histograma de muestra de Cauchy $X_1,...X_{1000}$")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x=TeX("Muestra $X_i \\forall i\\in \\[1,1000\\]$"), y="Densidad") + 
  geom_line(ac.density, mapping=aes(x=density.x, y=density.y), color='red', size=1)

# Make data.frame
hcall.df = data.frame(size = factor(rep(c(1,2,5,30,500,1200), each=1000)), 
                      sizeStr = factor(rep(c("1","2","5","30","500","1200"), each=1000)), 
                      data = c(ca, cb, cc, cd, ce, cf))
                      # data = c(podar(ca, 2), podar(cb, 2), podar(cc, 2), podar(cd, 2), podar(ce, 2), podar(cf, 2)))
cauchy.df = data.frame(x = rep(seq(-10,10,length.out=1000),6),
                       y = rep(dcauchy(seq(-10,10,length.out=1000)),6),
                       size = factor(rep(c(1,2,5,30,500,1200), each=1000)),
                       line.size = rep(rep(1, 1000),6))

# Now plot histograms in several subplots with the same scale
hcall <- ggplot(hcall.df, aes(x=data, fill=size, after_stat(density)))
hcall + geom_histogram(breaks=seq(-10,10,length.out=21),
                       col=I("black"),
                       alpha=I(.6)) +
  facet_grid(size ~ .) +
  scale_fill_brewer(palette="Purples") +
  ggtitle(TeX("Histograma de promedio $\\bar{X}_{n}$ de muestra de Cauchy $X_1,...,X_{n}$")) +
  theme(plot.title = element_text(hjust = 0.3)) +
  labs(x=TeX("Muestra $\\bar{X}_{n}_i\\forall i\\in \\[1,1000\\]$"), y="Densidad", fill="TamaÒo") +
  geom_line(cauchy.df, mapping=aes(x=x, y=y, group=size), color='red', size=0.8)
# Save with width 888, height 350 :)

# Vemos que los histogramas son todos muy similares, lo que da a entender que con esta distribucion no ocurre que el promedio se va concentrando en la media como si ocurria con las U(0,1). Ademas hay que notar que no tiene media definida.

#################### ITEM H (ITEMS E) ####################

# Ahora comparamos distintos tama√±os de muestras con los promedios fijos (con histogramas y con boxplots)
set.seed(14)
cmayores.df <- data.frame(size = c(rep(1000, 1000),
                                   rep(2000, 2000),
                                   rep(4000, 4000),
                                   rep(8000, 8000),
                                   rep(16000, 16000),
                                   rep(32000, 32000)),
                         data = c(ce,
                                  generar_promedios_cauchy(500, 2000),
                                  generar_promedios_cauchy(500, 4000),
                                  generar_promedios_cauchy(500, 8000),
                                  generar_promedios_cauchy(500, 16000),
                                  generar_promedios_cauchy(500, 32000)),
                         sizeStr = c(rep("1000", 1000),
                                     rep("2000", 2000),
                                     rep("4000", 4000),
                                     rep("8000", 8000),
                                     rep("16000", 16000),
                                     rep("32000", 32000)))

# Now plot histograms in several subplots with the same scale
hcmay <- ggplot(cmayores.df, aes(x=data, fill=fct_reorder(sizeStr,size), after_stat(density)))
hcmay + geom_histogram(breaks=seq(-15,15,length.out=21),
                       col=I("black"),
                       alpha=I(.4)) +
  facet_grid(. ~ size) +
  scale_fill_brewer(palette="Spectral") +
  ggtitle(TeX("Histograma de promedio $\\bar{X}_{500}$ de muestra de Cauchy $X_1,...,X_{500}$")) +
  theme(plot.title = element_text(hjust = 0.3)) +
  labs(x=TeX("Muestra $\\bar{X}_{500}_i\\forall i\\in \\[1,size\\]$"), y="Densidad", fill="TamaÒo")
# Save with width 888, height 350 :)

# Then make a boxplot with all of them in the same scale
bcmay <- ggplot(cmayores.df, aes(x=fct_reorder(sizeStr,size), y=data, group=size)) + 
  geom_boxplot(aes(fill=fct_reorder(sizeStr,size))) +
  scale_fill_brewer(palette="Spectral") +
  ggtitle(TeX("Boxplots de promedio $\\bar{X}_{500}$ de muestra de Cauchy $X_1,...,X_{500}$")) +
  labs(x=TeX("Muestra $\\bar{X}_{500}_i\\forall i\\in \\[1,size\\]$"), y="Densidad", fill="TamaÒo") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits=c(-15,15))
bcmay

# Vemos que a medida que crece el numero de muestras, las colas se van agrandando muy de a poco.
# Tambien aparecen muchos mas outliers, pero para que no desvirtuen los graficos, estos fueron podados.

# Observamos que a medida que crece la muestra considerada, aumenta la cantidad de outliers, pero las distancias intercuartiles se mantienen "constantes"

# Analizamos lo que ocurre cuando aumenta la cantidad de muestras a partir de un m chico (m=5)
set.seed(15)
cmenores.df <- data.frame(size = c(rep(5, 5),
                                   rep(10, 10),
                                   rep(50, 50),
                                   rep(100, 100),
                                   rep(500, 500),
                                   rep(1000, 1000)),
                          data = c(generar_promedios_cauchy(500, 5),
                                   generar_promedios_cauchy(500, 10),
                                   generar_promedios_cauchy(500, 50),
                                   generar_promedios_cauchy(500, 100),
                                   generar_promedios_cauchy(500, 500),
                                   generar_promedios_cauchy(500, 1000)),
                          sizeStr = c(rep("1000", 5),
                                      rep("2000", 10),
                                      rep("4000", 50),
                                      rep("8000", 100),
                                      rep("16000", 500),
                                      rep("32000", 1000)))

# Now plot histograms in several subplots with the same scale
hcmen <- ggplot(cmenores.df, aes(x=data, fill=fct_reorder(sizeStr,size), after_stat(density)))
hcmen + geom_histogram(breaks=seq(-15,15,length.out=21),
                       col=I("black"),
                       alpha=I(.4)) +
  facet_grid(. ~ size) +
  scale_fill_brewer(palette="Spectral", direction=-1) +
  ggtitle(TeX("Histograma de promedio $\\bar{X}_{500}$ de muestra de Cauchy $X_1,...,X_{500}$")) +
  theme(plot.title = element_text(hjust = 0.3)) +
  labs(x=TeX("Muestra $\\bar{X}_{500}_i\\forall i\\in \\[1,size\\]$"), y="Densidad", fill="TamaÒo")
# Save with width 888, height 350 :)

# Then make a boxplot with all of them in the same scale
bcmen <- ggplot(cmenores.df, aes(x=fct_reorder(sizeStr,size), y=data, group=size)) + 
  geom_boxplot(aes(fill=fct_reorder(sizeStr,size))) +
  scale_fill_brewer(palette="Spectral", direction=-1) +
  ggtitle(TeX("Boxplots de promedio $\\bar{X}_{500}$ de muestra de Cauchy $X_1,...,X_{500}$")) +
  labs(x=TeX("Muestra $\\bar{X}_{500}_i\\forall i\\in \\[1,size\\]$"), y="Densidad", fill="TamaÒo") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits=c(-15,15))
bcmen

#################### ITEM H (ITEMS F) ####################

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

# Make data frame for it all
cfedcba.df = data.frame(size = factor(rep(c(1,2,5,30,500,1200), each=1000)), 
                        sizeStr = factor(rep(c("1","2","5","30","500","1200"), each=1000)), 
                        data = c(ca,cb,cc,cd,ce,cf))

# Realizamos qqnorm para los seis conjuntos de datos
cqqf <- ggplot(cfedcba.df, aes(sample=data, color=size, group=size))
cqqf + geom_qq(alpha=1) + 
  facet_grid(size ~ .) +
  scale_color_brewer(palette="YlOrRd") +
  ggtitle(TeX("QQnorm para promedio $\\bar{X}_{n}$ de muestra de Cauchy $X_1,...,X_{n}$")) +
  theme(plot.title = element_text(hjust = 0.3)) +
  labs(x="Cuantiles teÛricos", y=TeX("Cuantiles de la muestra $\\bar{X}_{n}_i\\forall i\\in \\[1,1000\\]$"), color="n")
# Save with width 888, height 350 :) 

#par(mfrow = c(2, 3))
#qqnorm(podar(ca, 2), main = "QQPlot Normal n=1")
#qqnorm(podar(cb, 2), main = "QQPlot Normal n=2")
#qqnorm(podar(cc, 2), main = "QQPlot Normal n=5")
#qqnorm(podar(cd, 2), main = "QQPlot Normal n=30")
#qqnorm(podar(ce, 2), main = "QQPlot Normal n=500")
#qqnorm(podar(cf, 2), main = "QQPlot Normal n=1200")
#par(mfrow = c(1, 1))

# Estos resultados anteriores son esperables ya que las cauchy nunca van a tender a una N(0,1) como si ocurre con las uniformes. En el caso de las uniformes los qqnorm daban una recta porque hab√≠a mucha "correlacion" entre ambas distribuciones.
