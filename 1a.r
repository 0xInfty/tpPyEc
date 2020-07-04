#####################################
# TP Probabilidad y Estadistica (C) #
#                                   #
#          Inciso 1a                #
#####################################

library(latex2exp)
library(ggplot2)

# Sample
set.seed(2020)
x1...x1000 <- runif(1000) # Generamos una muestra de 1000 v.a. i.i.d. con distribucion U[0.1]

# Histogram
#hist(x1...x1000,
#     freq = FALSE,
#     main = "Histograma de muestra uniforme",
#     xlab = TeX("Muestra $x_1...x_{1000}$"), 
#     ylab = "Densidad",
#     col = "#66B2FF")
#abline(h=1, col="red") # Comparo con la densidad de una U[0,1]
#text(0.25,1.1,TeX("$f(x)\\,=\\frac{1}{1\\,-\\,0}\\,=\\,1$"), col="red")

# Make data frames :)
df.x1...x1000 = data.frame("data" = x1...x1000)
df.density <- data.frame("density.x" = c(-0.1,0,1,1.1),
                         "density.y" = c(0,1,0,0))

# Now plot histogram
ggplot(df.x1...x1000, aes(x=data, after_stat(density))) +
geom_histogram(
        breaks=seq(0,1,.1),
#        binwidth = .1, 
        fill=I("#66B2FF"), 
        col=I("black"), 
        alpha=I(.4),
#        xlim=c(-0.1,1.1)
        ) +
ggtitle("Histograma de muestra uniforme") +
        theme(plot.title = element_text(hjust = 0.5)) +
labs(x=TeX("Muestra $x_1...x_{1000}$"), y="Densidad") + 
geom_step(data=df.density, mapping=aes(density.x, density.y), color='red', size=1) + 
scale_x_continuous(limits=c(-.1,1.1)) +
annotate("text", x=.8, y=1.14, label=TeX("$f(x)\\,=\\,\\frac{1}{1\\,-\\,0}\\,=\\,1$"), col="red")
