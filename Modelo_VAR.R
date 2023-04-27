#install.packages(c("readxl", "tidyverse", "lubridate", "car", "urca", "tseries", "astsa", "forecast", "foreign", "timsac", "vars", "mFilter", "dynlm", "nlme", "strucchange", "quantmod", "xts", "Hmisc"))
install.packages("scales") #instala el paquete scales (solo necesario una vez)
 #carga el paquete

library(readxl)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(car)
library(urca)
library(tseries)
library(astsa)
library(forecast)
library(foreign)
library(timsac)
library(vars)
library(mFilter)
library(dynlm)
library(nlme)
library(strucchange)
library(quantmod)
library(xts)
library(Hmisc)
library(scales)
# library(het.test)


datos <- read_excel("Base_VAR_1996_2022.xlsx", sheet = "SinVariaciones")

datos <- ts(datos, start = 1996, frequency = 4)

png(file = "Comportamiento_variables.png")
colores <- c("#00ff1a", "blue", "#ffd500", "#f02020") # Definir un vector de colores para las series de datos
titulos <- c("Dólar Observado", "TPM", "Precio del Cobre", "Variación Trimestralizada IPC") # Definir un vector con los títulos personalizados para cada gráfico
par(mfrow = c(4, 1), mar = c(4, 4, 4, 1) + 0.1) # Configurar los parámetros de la ventana gráfica para tener 4 gráficos en la misma imagen
for (i in 1:ncol(datos)) { # Dibujar cada columna/serie de datos en un gráfico separado con su respectivo color y título
  plot(datos[, i], type = "l", col = colores[i], main = titulos[i])
}
dev.off()


datos

dolar_obs = ts(datos[,1], start=1996, freq=4)
dolar_obs
TPM = ts(datos[,2], start=1996, freq=4)
TPM
prec_cobre = ts(datos[,3], start=1996, freq=4)
prec_cobre
IPC = ts(datos[,4], start=1996, freq=4)
IPC

ndiffs(dolar_obs)

ndiffs(TPM)

ndiffs(prec_cobre)

ndiffs(IPC)


adf.test(dolar_obs, alternative = "stationary")
# p-value = 0.8535
adf.test(TPM, alternative = "stationary")
# p-value = 0.3263
adf.test(prec_cobre, alternative = "stationary")
# p-value = 0.3645
adf.test(IPC, alternative = "stationary")
# p-value = 0.02444

#### Diferenciacion

dolar_obs_diff <- diff(dolar_obs)
TPM_diff <- diff(TPM)
prec_cobre_diff <- diff(prec_cobre) 

# Extensiones (IPC tiene una variable más, porque no se diferenció)
length(dolar_obs)
length(dolar_obs_diff)
length(TPM_diff)
IPC <- tail(IPC, n = length(IPC) - 1) #Le quitamos el primer valor al IPC para que su extensión encaje con las series diferenciadas
length(IPC)

ndiffs(dolar_obs_diff)
ndiffs(TPM_diff)
ndiffs(prec_cobre_diff)
ndiffs(IPC)

# Defino una base con las variables ya estacionarias
datos_est <- cbind(dolar_obs_diff, TPM_diff, prec_cobre_diff, IPC)

png(file = "Comportamiento_variables_estacionarias.png")
plot(datos_est, main = "Comportamiento de las variables estacionarias")
dev.off()

#Comparaciones

png(file = "Comparación_estacionariedad_dolar_obs.png")
par(mfrow=c(2,2), mar=c(4,4,4,1) + .1)
plot(dolar_obs, ylab="Dolar Observado", xlab="Tiempo")
acf(dolar_obs, main="Serie no Estacionaria")
plot(dolar_obs_diff, ylab="D.O", xlab="Tiempo")
acf(dolar_obs_diff, main="Serie Estacionaria")
dev.off()

png(file = "Comparación_estacionariedad_TPM.png")
par(mfrow=c(2,2), mar=c(4,4,4,1) + .1)
plot(TPM, ylab="TPM", xlab="Tiempo")
acf(TPM, main="Serie no Estacionaria")
plot(TPM_diff, ylab="TPM", xlab="Tiempo")
acf(TPM_diff, main="Serie Estacionaria")
dev.off()

png(file = "Comparación_estacionariedad_precio_cobre.png")
par(mfrow=c(2,2), mar=c(4,4,4,1) + .1)
plot(prec_cobre, ylab="Precio", xlab="Tiempo")
acf(prec_cobre, main="Serie no Estacionaria")
plot(prec_cobre_diff, ylab="Precio del cobre", xlab="Tiempo")
acf(prec_cobre_diff, main = "Serie Estacionaria")
dev.off()


## Normalización

# Convertir las columnas de la serie de tiempo a objetos de clase "numeric"
datos_est_num <- apply(datos_est, 2, as.numeric)

# Normalizar los datos en un rango de 0 a 1 utilizando la función scales::rescale()
datos_norm <- apply(datos_est_num, 2, scales::rescale)

# Convertir el resultado a un dataframe
datos_norm <- as.data.frame(datos_norm)
datos_norm_ts <- ts(datos_norm, start=1996, freq=4)





# H0: El precio del cobre no es causado en el sentido de granger por el dolar observado > 0,05
# H1: El precio del cobre si es causado en el sentido de granger por el dolar observado < 0,05
grangertest(prec_cobre_diff ~ dolar_obs_diff, order = 1)
# 0.02521 * (, < 0,05)
# Se rechaza la hipótesis nula, el precio del cobre sí es causado en el sentido de granger por el precio del dolar

#Ahora probamos la causalidad en el sentido contrario 

# H0: La TPM no es causado en el sentido de granger por el  precio del cobre > 0,05
# H1: La TPM si es causado en el sentido de granger por el  precio del cobre < 0,05
grangertest(dolar_obs_diff~prec_cobre_diff, order = 1)

# 0.07975 .
# Crear una lista vacía para almacenar los resultados de la prueba de Granger
granger_results <- list()
# Realizar la prueba de causalidad de Granger para diferentes órdenes (1 a 12)
for (order in 1:12) {
  granger_results[[order]] <- grangertest(dolar_obs_diff ~ prec_cobre_diff, order = order)
}
# Ver los resultados de la prueba de Granger
granger_results


# H0: La TPM no es causada en el sentido de granger por el IPC > 0,05
# H1: La TPM si es causada en el sentido de granger por el IPC < 0,05
grangertest(TPM_diff~IPC, order = 1)

# 0.0002038 ***

# H0: El IPC no es causada en el sentido de granger por la TPM > 0,05
# H1: El IPC si es causada en el sentido de granger por La TPM < 0,05
grangertest(IPC~TPM_diff, order = 1)
grangertest(IPC~TPM_diff, order = 2)
# 0.0002038 ***

# Crear una lista vacía para almacenar los resultados de la prueba de Granger
granger_results <- list()
# Realizar la prueba de causalidad de Granger para diferentes órdenes (1 a 12)
for (order in 1:12) {
  granger_results[[order]] <- grangertest(IPC ~ TPM_diff, order = order)
}
# Ver los resultados de la prueba de Granger
granger_results

#No se encuentra causalidad LA TPM no es efectiva [No es tan así, esto simplemente sugiere que esa variable no tiene poder predictivo sobre la otra. Sin embargo, esto no significa que no haya ninguna interacción entre las variables en el sistema]

# H0: El IPC no es causada en el sentido de granger por la TPM > 0,05
# H1: El IPC si es causada en el sentido de granger por La TPM < 0,05
grangertest(IPC~dolar_obs_diff, order = 1)
# Crear una lista vacía para almacenar los resultados de la prueba de Granger
granger_results <- list()
# Realizar la prueba de causalidad de Granger para diferentes órdenes (1 a 12)
for (order in 1:12) {
  granger_results[[order]] <- grangertest(IPC ~ dolar_obs_diff, order = order)
}
# Ver los resultados de la prueba de Granger
granger_results




########################################## VAR


ejvarA1 <- datos_norm_ts

VARselect(ejvarA1, lag.max = 12)

var_A1 <- VAR(ejvarA1, p=3)
#var_A1

summary(var_A1)

#Para graficar
par(mfrow=c(1,1), mar=c(3,3,3,1) + .1)
plot(var_A1)

###Haremos la prueba de autocorrelación serial en los residuales

# H0: Los residuales no están correlacionados, -> p value > 0,05 Aceptar H0 -- No rechazar H0
# H1: Los residuales si están correlacionados, -> p value < 0,05 Aceptar H1 -- Rechazar H0

serialvar_A1 <- serial.test(var_A1, lags.pt = 3, type = "PT.asymptotic")
serialvar_A1$serial

## en nuestro caso obtuvimos 2.2e-16, que es muy cercano a 0 y < a 0,05
#Por ende, rechazamos la hipotesis nula, los residuales sí están correlaciondos 
#Hay presencia de correlación serial

###Procedemos a hacer la prueba de Jarque-Bera para evaluar normaliad de los residuales

## Nos vamos a fijar en los p value de la kurtosis y del sesgo (skewness)

#H0: Los residuales se distribuyen normal   (pvalue > 0,05 -> Aceptamos H0)
#H1: Los residuales no se distribuyen normal (pvalue < 0,05 -> Rechazamos H0)

normvar_A1=normality.test(var_A1)
normvar_A1$jb.mul

#Sesgo -> p-value = 3.789e-06
#kurtosis -> p-value < 2.2e-16

##Se concluye que no hay normalidad, valores p < 0,05

#Procedemos a realizar la prueba de homocedasticidad de la varianza de los residuales

arch_A1 <- arch.test(var_A1, lags.multi = 3)

#H0: La varianza de los residuales es constante (pvalue >  0,05) 
#H1: La varianza de los residuales no es constante (pvalue < 0,05)

arch_A1$arch.mul
#p-value =  0.2989, esto quiere decir que la varianza de los residuales es constante


#### Modelo impulso respuesta


# 1° Veremos el impulso respuesta del tipo de cambio, frente a variaciones de las otras variables
impulso_cobre_respuesta_dolar_A1= irf(var_A1, impulse = "prec_cobre_diff", response ="dolar_obs_diff", n.ahead=12, boot=TRUE)
impulso_cobre_respuesta_dolar_A1
plot(impulso_cobre_respuesta_dolar_A1, main="Función Impulso Respuesta, Precio del Cobre - Dolar Observado (VAR A1)")

## Este modelo impulso respuesta nos muestra como responde el precio del dólar ante un impulso del precio del cobre

## Ahora veremos como responde el IPC frente a un impulso del dólar

impulso_dolar_respuesta_IPC_A1=irf(var_A1, impulse = "dolar_obs_diff", response ="IPC", n.ahead=12, boot=TRUE)
impulso_dolar_respuesta_IPC_A1
plot(impulso_dolar_respuesta_IPC_A1, main="Función Impulso Respuesta, Dólar Observado - IPC (VAR A1)")





