#### Configuraciones iniciales ####
rm(list = ls())
setwd("D:/Estudiando/R/R4DS/Clase8")

#### Teorema del límite central ####

# Distribución normal
# P[a<X<b] = integral_{a}^{b}\frac{1}{\sqrt{2*pi*\sigma^2}}\exp(\frac{-(x-\mu)^2}{2\sigma^2})dx

# Ejemplo 1
poblacion <- unlist(read.csv("https://raw.githubusercontent.com/robintux/Datasets4StackOverFlowQuestions/master/femaleControlsPopulation.csv"))

# Muetsremos un par de tamaño de muestras y veamos como se va comportando la media con estos tamaños de muestra
#
# Media de la población
mean(poblacion)
# Número de repeticiones
n <- 500
# Dos tamaños de muestra : 10 y 50
mediastam10 <- vector("numeric", n)
mediastam50 <- vector("numeric", n)
#
# Estructura repetitva
for (i in 1:n) {
  x10 <- sample(poblacion, 10)
  x50 <- sample(poblacion, 50)
  mediastam10[i] <- mean(x10)
  mediastam50[i] <- mean(x50)
}

# Mostremos histogramas para cada uno de estos vectores que almacenan medias aritméticas

par(mfrow=c(1,2))
hist(mediastam10)
hist(mediastam50)

# Rango de datos de mi población
min(poblacion) # 15.51
max(poblacion) # 36.84

# Histograma de la población
par(mfrow=c(1,1))
hist(poblacion)
mean(poblacion) # 23.89338
mean(mediastam10) # 23.92504
mean(mediastam50) # 23.90197
# Rango para las medias de tamaño 10 y 50
min(mediastam10) # 20.3
max(mediastam10) # 27.437

min(mediastam50) # 22.4026
max(mediastam50) # 25.2834

# Consideremos el intervalo de 23 a 25
mean(mediastam10 > 23 & mediastam10 < 25)
# 0.668 más de la mitad de los datos
mean(mediastam50 > 23 & mediastam50 < 25)
# 0.974 la mayoría de las observaciones
#
# Teóricamente 
media_p <- mean(poblacion)
desvest_p <- sd(poblacion)
# Calculamos la probabilidad, suponiendo normalidad
pnorm(25-media_p)/desvest_p-pnorm(23-media_p)/desvest_p 

### Ejemplo 2 ####
# consideremos el siguiente dataset:
datos <- read.csv("https://raw.githubusercontent.com/robintux/Datasets4StackOverFlowQuestions/master/mice_pheno.csv")
head(datos)
# Nombres de columnas
colnames(datos)
str(datos)
# como se distribuyen los datos
table(datos$Sex)
table(datos$Diet)
mean(datos$Bodyweight)
# este resultado muestra que hay missing data
datosClean <- na.omit(datos)
#
# Variables cualitativas posibles valores
unique(datosClean$Sex)
# F femenino y M masculino
unique(datosClean$Diet)
# hf dieta alta en grasas chow dieta controlada
# 
# Analicemos el peso corporal (Bodyweight) para cada combinación de sexo y dieta
MascCtrl <- datosClean$Bodyweight[datosClean$Sex == "M" & datosClean$Diet == "chow"]
MascGrasa <- datosClean$Bodyweight[datosClean$Sex == "M" & datosClean$Diet == "hf"]
FemeCtrl <- datosClean$Bodyweight[datosClean$Sex == "F" & datosClean$Diet == "chow"]
FemeGrasa <- datosClean$Bodyweight[datosClean$Sex == "F" & datosClean$Diet == "hf"]
# Mostremos los cuatro histogramas
par(mfrow=c(2,2))
hist(MascCtrl)


hist(MascGrasa)
hist(FemeCtrl)
hist(FemeGrasa)

# Mediasy desviaciones estándar
mu_MascCtrl <- mean(MascCtrl)
sd_MascCtrl <- sd(MascCtrl)
mu_MascGrasa <- mean(MascGrasa)
sd_MascGrasa <- sd(MascGrasa)
mu_FemeCtrl <- mean(FemeCtrl)
sd_FemeCtrl <- sd(FemeCtrl)
mu_FemeGrasa <- mean(FemeGrasa)
sd_FemeGrasa <- sd(FemeGrasa)

# Definamos un tamaño de muestra = 25
# Cosntruyamos muestras
Xm_Ctrl <- sample(MascCtrl, 25)
Xm_Grasa <- sample(MascGrasa, 25)
Yh_Ctrl <- sample(FemeCtrl, 25)
Yh_Grasa <- sample(FemeGrasa, 25)

# Calculemos las diferencias en medias de las poblaciones con estas muestras
abs(mean(MascCtrl)-mean(MascGrasa)-(mean(Xm_Ctrl)-mean(Xm_Grasa)))
abs(mean(FemeCtrl)-mean(FemeGrasa)-(mean(Yh_Ctrl)-mean(Yh_Grasa)))
### Teorema del Límite Central ####
# Cuando el tamaño de la muestra es grande, la media de una muestra aleatoria sigue una distribución normal
# centrada en la media de la población, y la desviación estándar es igual a la desviación est+ándar 
# de la pobñlación dividido por la raiz cuadrada del tamaño de la muestra
# (mean(muestra)-mean(poblacion))/sd(poblacion)/sqrt(N))
# Donde N es el tamaño de muestra
# Aproximar a una normal centrada en 0 con una sd igual a 1
# Reescribiría mi TLC en el caso de dos muestras
#mean(muestra1)-mean(muestra2)/(sqrt((sigma^2(muestra1)/N1) + sigma^2(muestra2)))
# N1 : tamaño de la muestra 1
# N2 Tamaño de la muestra 2

# Consideremos una muestra de tamaño 1000 de una normal
x <- rnorm(1000)
par(mfrow=c(1,1))
hist(x, xlab = NULL, ylab = NULL, main = NULL, axes = F)
par(new = T)
plot(density(x))
pnorm(1)-pnorm(-1)
pnorm(2)-pnorm(-2)
pnorm(3)-pnorm(-3)

# Para mi ejemplo
Z <- (MascCtrl-mean(MascCtrl))/sd(MascCtrl) 
mean(abs(Z)<1)
mean(abs(Z)<2)
mean(abs(Z<3))

# Para mi ejemplo femenino
Z1 <- (FemeCtrl-mean(FemeCtrl))/sd(FemeCtrl)
mean(abs(Z1)<1)
mean(abs(Z1)<2)
mean(abs(Z1)<3)

Z2 <- (FemeGrasa-mean(FemeGrasa))/sd(FemeGrasa)
mean(abs(Z2)<1)
mean(abs(Z2)<2)
mean(abs(Z2)<3)

# qqplot
par(mfrow=c(1,1))
qqnorm(Z)
abline(0,1, col = "red")

qqnorm(Z2)
abline(0,1, col = "red")
# Realicemos la construcción de nuestro vector de medias para muchas muestras
# Tamaño de muestra = 25
MediasMascCtrl <- replicate(500, mean(sample(MascCtrl, 25)))
par(mfrow=c(1,2))
hist(MediasMascCtrl)
qqnorm(MediasMascCtrl)
abline(0, 1, col = "red")