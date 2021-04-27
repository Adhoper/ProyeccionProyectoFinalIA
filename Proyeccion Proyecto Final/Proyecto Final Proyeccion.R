#Instalacion de Librerias

install.packages("readxl")

install.packages("ggplot2")

install.packages("tidyverse")


#Se llaman las librerias
library(readxl)
library(ggplot2)
library(tidyverse)


#Se descarga los datos o la tabla de datos de http://datos.gob.do/

download.file(url ="http://dariocontreras.gob.do/transparencia/index.php/datos-abiertos/category/404-estadistica?download=1761:Emergencias%20atendidas%20por%20transporte%20accidentado,%20Edad%20y%20Sexo,%20HDUDDC,%20Enero%20-%20Marzo%202021", destfile = "Servicios de emergencia Enero - Marzo 2021.xlsx", mode = "wb")

#Se importa el archivo excel con los datos.
archivo <- readxl::read_excel("Servicios de emergencia Enero - Marzo 2021.xlsx")


#Se agrega el campo Dias para la Proyeccion
archivo$Dias=archivo$`AUTOS (F)`*archivo$`MOTOS (F)`

#HOMBRES

#Se grafica la grafica de REGRESIÓN LINEAL SIMPLE PARA MAYO 2021 
archivo %>%
  ggplot(aes(x=Dias,y=`AUTOS (M)...4`)) + geom_point()+ geom_abline(intercept =15.227270 ,slope = 0.092671,col = 'blue')+ geom_vline(xintercept = 30, col = "red")

#Se grafica la grafica de REGRESIÓN LINEAL SIMPLE PARA JUNIO 2021 
archivo %>%
  ggplot(aes(x=Dias,y=`AUTOS (M)...4`)) + geom_point()+ geom_abline(intercept =15.227270 ,slope = 0.092671,col = 'blue')+ geom_vline(xintercept = 60, col = "red")

#Se grafica la grafica de REGRESIÓN LINEAL SIMPLE PARA JULIO 2021 
archivo %>%
  ggplot(aes(x=Dias,y=`AUTOS (M)...4`)) + geom_point()+ geom_abline(intercept =15.227270 ,slope = 0.092671,col = 'blue')+ geom_vline(xintercept = 90, col = "red")

#Se hace la regresion
regresion <- lm(`AUTOS (M)...4`~ Dias,data = archivo)

#se obtiene un resumen de la regresion
summary(regresion)

#Tabla anova
anova(regresion)

#se obtienen los residuos estandarizados
residuo <- rstandard(regresion)

#se obtienen los residuos ajustados
ajustado <- fitted(regresion)

plot(ajustado,residuo)

abline(h=0)

#MUJERES

#Se grafica la grafica de REGRESIÓN LINEAL SIMPLE PARA MAYO 2021 
archivo %>%
  ggplot(aes(x=Dias,y=`AUTOS (F)`)) + geom_point()+ geom_abline(intercept =3.756944 ,slope = 0.021532,col = 'blue')+ geom_vline(xintercept = 30, col = "yellow")

#Se grafica la grafica de REGRESIÓN LINEAL SIMPLE PARA MAYO 2021 
archivo %>%
  ggplot(aes(x=Dias,y=`AUTOS (F)`)) + geom_point()+ geom_abline(intercept =3.756944 ,slope = 0.021532,col = 'blue')+ geom_vline(xintercept = 60, col = "yellow")

#Se grafica la grafica de REGRESIÓN LINEAL SIMPLE PARA MAYO 2021 
archivo %>%
  ggplot(aes(x=Dias,y=`AUTOS (F)`)) + geom_point()+ geom_abline(intercept =3.756944 ,slope = 0.021532,col = 'blue')+ geom_vline(xintercept = 90, col = "yellow")

#Se hace la regresion
regresion <- lm(`AUTOS (F)`~ Dias,data = archivo)

#se obtiene un resumen de la regresion
summary(regresion)

#Tabla anova
anova(regresion)

#se obtienen los residuos estandarizados
residuo <- rstandard(regresion)

#se obtienen los residuos ajustados
ajustado <- fitted(regresion)

plot(ajustado,residuo)

abline(h=0)

