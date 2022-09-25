#### Configuraciones iniciales ####
# limpieza memoria
rm(list = ls())
par(mfrow=c(1,1))

# Setting working directory
if (Sys.info()[["sysname"]] == "Windows") {
  if (Sys.info()[["user"]] == "bchaihuaque") {
    setwd("D:/Doctorado/Semestre 2/Metodos Cuantitativos 2/S1") # PC work
  } else {
    if (Sys.info()[["user"]] == "MEF") {
      setwd("D:/carpeta") # Laptop Dell
    } else {
      if (Sys.info()[["user"]] == "bchai"){
        setwd("D:/Estudiando/R/R4DS/Clase8")
      }
    }
  }
  
} else {
  if (Sys.info()[["sysname"]] == "Linux") {
    setwd("/home/bchaihuaque/Estudiando/R/Clase8/Teorema_Limite_Central") # Laptop con Ubuntu 
  } else {
    if (Sys.info()[["sysname"]] == "MacOS") {
      setwd("poner_ruta") # Mi Macbook Pro
    }
  }
}
getwd()
Sys.info()

# Instalación de paquetes
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("dplyr", "tidyverse", "nortest")

ipak(packages)
# ── Conflicts ────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
# ✖ dplyr::filter() masks stats::filter()
# ✖ dplyr::lag()    masks stats::lag()
# ✖ dplyr::recode() masks car::recode()
# ✖ dplyr::select() masks MASS::select()
# ✖ purrr::some()   masks car::some()

#### Análisis de densidad ####
help("density")
par(mfrow=c(2,2))
n <- c(10, 100, 1000, 10000)
# Gráficos de densidad
for (i in n) {
  x <- rnorm(i)
  plot(density(x), main = bquote(~n == .(i)),
       ylab = 'Densidad', col = c("blue3", "brown3", "mediumseagreen", "yellow"),
       xlab = 'x', las = 1, lwd = 4)
}
#### Gráficos qqplot ####
for (i in n) {
  x <- rnorm(i)
  qqnorm(x, main = bquote(~n == .(i)))
  qqline(x) # agrega la línea de referencia
  }
# Gráficos qqplot con bandas
require(car)
for (i in n) {
  x <- rnorm(i)
  qqPlot(x, pch = 20, ylab = "sample values", main = bquote(~n == .(i)))
}
#### Pruebas de normalidad ####
# H0: la muestra proviene de una población normal
# H1: la muestra NO proviene de una población normal

# Shapiro-Wilk
y <- rnorm(n=100, mean = 150, sd = 5)
z <- rnorm(n=10, mean = 150, sd = 5)
shapiro.test(y)
shapiro.test(z)
#Shapiro-Wilk normality test

# data:  y
# W = 0.99139, p-value = 0.7754
# p-value = 0.7754 que es mayor al nivel de significancia de 0.05, por lo que no
# hay evidencia para rechazar la hipótesis nula de normalidad

# Anderson - Darling normality test
ad.test(y)
ad.test(z)
# Anderson-Darling normality test

# data:  y
# A = 0.1616, p-value = 0.9449
# p-value = 0.9449 que es mayor al nivel de significancia de 0.05, por lo que no
# hay evidencia para rechazar la hipótesis nula de normalidad

# Cramer-von Mises
cvm.test(y)
cvm.test(z)
# Cramer-von Mises normality test

# data:  y
# W = 0.018363, p-value = 0.98
# p-value = 0.98 que es mayor al nivel de significancia de 0.05, por lo que no
# hay evidencia para rechazar la hipótesis nula de normalidad

# Lilliefors (Kolmogorov-Smirnov)
lillie.test(y)
lillie.test(z)
# Lilliefors (Kolmogorov-Smirnov) normality test

# data:  y
# D = 0.045108, p-value = 0.8856
# p-value = 0.8856 que es mayor al nivel de significancia de 0.05, por lo que no
# hay evidencia para rechazar la hipótesis nula de normalidad

# Prueba Pearson - Chi square
pearson.test(y)
pearson.test(z)
# Pearson chi-square normality test

# data:  y
# P = 4.78, p-value = 0.9054
# p-value = 0.9054 que es mayor al nivel de significancia de 0.05, por lo que no
# hay evidencia para rechazar la hipótesis nula de normalidad

# Shapiro-Francia
sf.test(y)
sf.test(z)
# Shapiro-Francia normality test

# data:  y
# W = 0.99256, p-value = 0.78
# p-value = 0.78 que es mayor al nivel de significancia de 0.05, por lo que no
# hay evidencia para rechazar la hipótesis nula de normalidad
