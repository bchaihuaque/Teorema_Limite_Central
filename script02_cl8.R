#### Configuraciones iniciales ####
rm(list = ls())
setwd("D:/Estudiando/R/R4DS/Clase8")

# Cargamos datos 
datos <- c(3.5,7.2,2.6,3.8,6.2,2.8,5.4,3.9,3.9,5.0,4.1,3.4,4.2,3.6,4.7,3.7,3.9,4.5,2.7,3.6,4.2,4.0,4.4,1.5,3.3,3.3,4.1,6.6,2.2,2.9,5.2,2.8,5.4,4.1,2.0,5.5,2.7,4.4,5.9,2.8)

# Cargamos algunos paquetes
library(MASS)
library(nortest)
install.packages("normtest")
library(normtest)
install.packages("moments")
library(moments)
install.packages("DescTools")
library(DescTools)
par(mfrow=c(2,1))
hist(datos, col = "green", freq = FALSE, main = " ")
curve(dnorm(x, mean(datos), sd(datos)), add = T, lwd = 2.5)

# Boxplot: en busca de outliers
boxplot(datos, col = "green", border = "blue",
        horizontal = TRUE)

# qqplot (gráfico quantile quantile)
par(mfrow=c(1,1))
qqnorm(datos)
qqline(datos)

#### Método analítico ####
# Asimetria (G1) cercana a 0


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

# Cálculo del coefiente de asimetria y Kurtosis para MascGrasa
g1 = m3/m2^(1.5)
g2 = m4/m2^2
m <- mean(MascGrasa)
# Número de observaciones
n <- length(MascGrasa)
# Cálculo del coefiente de asimetria
g1_MascGrasa <- (((sum(MascGrasa - m))^3)/n)/(sum(MascGrasa-m)^2/n)^(1.5)
m1 <- mean(MascCtrl)
n1 <- length(MascCtrl)
g1_MascCtrl <- (((sum(MascCtrl - m1))^3)/n1)/(sum(MascCtrl-m1)^2/n1)^(1.5)

#
# Conclusiones: analiticamente hablando el grupo MascGrasa tiene un comportamiento más normal que
# el grupo MascCtrl, pues el coeficiente de asimetria para el primer grupo es más cercano a cero 
# que el coeficiente de asimetria del segundo grupo

# Calculemos el g2 (Kurtosis)
g2_MascGrasa <- ((sum(MascGrasa - m)^4)/(n))/((sum(MascGrasa-m)^2)/(n))^2
g2_MascCtrl <- ((sum(MascCtrl - m1)^4)/(n1))/((sum(MascCtrl-m1)^2)/(n1))^2

# Reafirmamos que el grupo MascGrasa tiene un comportamiento más normal que el grupo
# MascCtrl
# Podriamos replicar estas cuentas para:
# FemeGrasa
# FemeCtrl
# 

# Calculemos g1 y g2 para datosClean

bw_clean <- datosClean$Bodyweight
mbw <- mean(bw_clean)
nbw <- length(bw_clean)
g1_bw_clean <- (((sum(bw_clean - mbw))^3)/nbw)/(sum(bw_clean-mbw)^2/nbw)^(1.5)
g2_bw_clean <- ((sum(bw_clean - mbw)^4)/(nbw))/((sum(bw_clean-mbw)^2)/(nbw))^2

#### Técnicas estadísticas inferenciales ####
# Ho: La muestra proviene de una población con distribución normal 
# H1: La muestra no proviene de una población con distribución normal
