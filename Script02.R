#### COnfiguraciones iniciales  ####

# Limpiamos memoria
rm(list = ls())
# Configuracion del directorio de trabajo
setwd()
# Cargamos datos 
datos <- c(3.5,7.2,2.6,3.8,6.2,2.8,5.4,3.9,3.9,5.0,4.1,3.4,4.2,3.6,4.7,3.7,3.9,4.5,2.7,3.6,4.2,4.0,4.4,1.5,3.3,3.3,4.1,6.6,2.2,2.9,5.2,2.8,5.4,4.1,2.0,5.5,2.7,4.4,5.9,2.8)

# Cargamos algunos paquetes
library(MASS)
library(nortest)
library(normtest)
library(moments)
library(DescTools)