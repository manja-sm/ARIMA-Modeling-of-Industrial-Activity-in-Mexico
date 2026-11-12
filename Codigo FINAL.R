# Librerías
install.packages("TSstudio")
install.packages("ggseas")
install.packages("lmtest")
install.packages("nortest")
install.packages("TSA")
install.packages("kableExtra")

library(corrplot)
library(knitr)
library(kableExtra)
library(TSA)
library(lmtest)
library(tidyverse)
library(readxl)
library(ggseas)
library(forecast)
library(tseries)
library(ggseas)
library(TSstudio)
library(readxl)
library(nortest)
library(ggplot2)

#Datos
data <- read_xlsx("C:\\Users\\maria\\Desktop\\UDLAP\\8\\Eco 2\\Indicadores.xlsx")



Indice <- data[,2]
Indice <- Indice[-nrow(Indice), , drop = FALSE] #quitar febrero de la serie

#Serie de tiempo
Serie <- ts(Indice, start = c(1993, 01), frequency = 12)
plot(Serie)

#Prueba de Estacionalidad
ggseasonplot(Serie, year.labels = TRUE, main = "Patrón estacional del índice")
nsdiffs(Serie) # 0 -> no estacional

#Prueba de Estacionariedad: Dickey Fuller
adf_test <- adf.test(Serie)
print(adf_test) #p-value es .2685 ent no es estacionaria
?xts

#Transformación de la Serie
#Box-Cox
lambda <- BoxCox.lambda(Serie) #lambda = 0.04350955
BCST<-BoxCox(Serie,lambda)

adf_test <- adf.test(BCST) #p-value = 0.3, sigue sin ser estacionaria 
nsdiffs(BCST) # 0 -> no estacional

#Diferencias 
#Diferencia 1
BCST_diff1 <- diff(BCST, differences = 1)
adf_test <- adf.test(BCST_diff1) #.01 -> estacionaria
nsdiffs(BCST_diff1) 

#Diferencia 2
BCST_diff2 <- diff(BCST, differences = 2)
adf_test <- adf.test(BCST_diff2) #.01 -> estacionaria

#Checar varianzas
var(BCST_diff1) # 0.0007267258
var(BCST_diff2) # 0.001320976

#La varianza de la primera diferencia es menor que el de la segunda entonces nos quedamos con ese.

#Propuestas del modelo
#MANUAL
# Checar funciones ACF y PACF para proponer dos modelos
FACSM <- acf(BCST_diff1) #MA
plot(FACSM)

FACPM <- pacf(BCST_diff1) #parcial es para AR
FACPM

# Momento donde una autocorrelacion del ACF es significativamente distinta de 0
N <- length(BCST) #longitud de la serie
d <- 1 #se diferenció 1 vez
suma <- 0 
paro <- TRUE #va a parar cuando la autocorrelacion k no sea significativamente distinta de cero
k <- 1 #contador de la k autocorrelación
facsuma <-c()

for (k in 1:10){
  if (abs(FACSM$acf[k+1])>(2*sqrt((1/(N-d))*(1+2*suma)))){
    cat("La autocorrelación ", k, " = ",FACSM$acf[k+1], "Significativa!!!!!!!!!!!\n")
    facsuma[k]<-FACSM$acf[k+1]
  }else{
    cat("La autocorrelación ", k," = ",FACSM$acf[k+1], "No significativa\n")
    facsuma[k]<-0
  }
  suma <- suma + facsuma[k]^2
}

#Fue en el 1

#Momento donde una autocorrelacion del PACF es significativamente distinta de 0
for (k in 1:10) {
  if (abs(FACPM$acf[k])>(2*sqrt(1/(N-d)))){
    cat("La autocorrelación parcial ", k, "=",FACPM$acf[k] ,"Significativa!!!!!!!!!!!\n")
  }else{
    cat("La autocorrelación parcial", k, "=",FACPM$acf[k] , "No significativa\n")
  }
}
#Significativa en 2

#Observando, se propone un ARIMA(2,1,1) y un ARIMA(1,1,1)


#AUTOMÁTICO
auto.arima(BCST) #ARIMA(1,1,2)

##CONCLUSIÓN##
#Se proponen tres modelos: ARIMA(1,1,2), ARIMA(2,1,1), ARIMA (1,1,1) y a sugerencia de la profa un ARIMA (2,1,0) 

#Verificación de Supuestos
ARIMA112 <- Arima(BCST,order=c(1,1,2))
ARIMA211 <- Arima(BCST,order=c(2,1,1))
ARIMA111 <- Arima(BCST,order=c(1,1,1))
ARIMA210<-Arima(BCST,order = c(2,1,0))

## 1. Errores con Media 0
t.test(residuals(ARIMA112)) # p-value = 0.1092 -> no se rechaza la nula, ent la media de los residuos no es significativamente diferente de 0.
t.test(residuals(ARIMA211)) # p-value = 0.1642 -> ""
t.test(residuals(ARIMA111)) # p-value = 0.3255 -> ""
t.test(residuals(ARIMA210)) # p-value = .2103 -> ""

#ARIMA 112 : 0.002120548
#ARIMA 211 : 0.001846754 
#ARIMA 111 : 0.001329215 
#ARIMA 210 : 0.00166566

#Para este supuesto es mejor el ARIMA 111, porque su p-value es mayor y su media es la mas cercana a 0.

## 2. Errores con Varianza Constante
bptest(lm(residuals(ARIMA112) ~ fitted(ARIMA112))) # p-value = 0.6917, hay varianza constante.
bptest(lm(residuals(ARIMA211) ~ fitted(ARIMA211))) # p-value = 0.6672, hay varianza constante. 
bptest(lm(residuals(ARIMA111) ~ fitted(ARIMA111))) # p-value = 0.4926, hay varianza constante.
bptest(lm(residuals(ARIMA210) ~ fitted(ARIMA210))) # p-value = 0.6645, hay varianza constante.

## 3. Normalidad en Errores
lillie.test(ARIMA112$residuals) # p-value < 2.2e-16
lillie.test(ARIMA211$residuals) # p-value < 2.2e-16
lillie.test(ARIMA111$residuals) # p-value < 2.2e-16
lillie.test(ARIMA210$residuals) # p-value < 2.2e-16

#No pasa la prueba de normalidad ninguno, es demasiado estricto.

#Vamos a graficar los residuos y ver donde es que hay problema. 
qqnorm(residuals(ARIMA112), main = "Gráfico Q-Q de los Residuos del Modelo ARIMA(1,1,2)")
qqline(residuals(ARIMA112), col = "red", lwd = 2)

qqnorm(residuals(ARIMA211), main = "Gráfico Q-Q de los Residuos del Modelo ARIMA(2,1,1)")
qqline(residuals(ARIMA211), col = "red", lwd = 2)

qqnorm(residuals(ARIMA111), main = "Gráfico Q-Q de los Residuos del Modelo ARIMA(1,1,1)")
qqline(residuals(ARIMA111), col = "red", lwd = 2)

qqnorm(residuals(ARIMA210), main = "Gráfico Q-Q de los Residuos del Modelo ARIMA(2,1,0)")
qqline(residuals(ARIMA210), col = "red", lwd = 2)

#En los tres modelos hay problema en los extremos. 

#Graficamos el histograma de los residuos de cada modelo
hist(ARIMA112$residuals)
hist(ARIMA211$residuals)
hist(ARIMA111$residuals)
hist(ARIMA210$residuals)

#Ver qué tanto porcentaje de los errores están contenidos en un intervalo de 2 stdv

# ARIMA 112
contador<-0
for (i in 1:length(ARIMA112$residuals)){
  if (abs(ARIMA112$residuals[i])<2*sd(ARIMA112$residuals)){
    contador<-contador+1
  }
}
cat("El ", contador/length(ARIMA112$residuals)*100, "% esta contenido en el intervalo de 2 desviaciones estandar")
# 97.66234 %

# ARIMA 211
contador<-0
for (i in 1:length(ARIMA211$residuals)){
  if (abs(ARIMA211$residuals[i])<2*sd(ARIMA211$residuals)){
    contador<-contador+1
  }
}
cat("El ", contador/length(ARIMA211$residuals)*100, "% esta contenido en el intervalo de 2 desviaciones estandar")
# 97.92208 %

# ARIMA 111
contador<-0
for (i in 1:length(ARIMA111$residuals)){
  if (abs(ARIMA111$residuals[i])<2*sd(ARIMA111$residuals)){
    contador<-contador+1
  }
}
cat("El ", contador/length(ARIMA111$residuals)*100, "% esta contenido en el intervalo de 2 desviaciones estandar")
# 97.66234 %

# ARIMA 210
contador<-0
for (i in 1:length(ARIMA210$residuals)){
  if (abs(ARIMA210$residuals[i])<2*sd(ARIMA210$residuals)){
    contador<-contador+1
  }
}
cat("El ", contador/length(ARIMA210$residuals)*100, "% esta contenido en el intervalo de 2 desviaciones estandar")
# 97.66234 %

# Ya con esto se puede probar normalidad para los residuos en los 4 modelos.

## 4. Errores independientes
Box.test(residuals(ARIMA112), type = "Box-Pierce") # p-value = 0.9172 -> los errores son ruido blanco, o sea independientes.
Box.test(residuals(ARIMA211), type = "Box-Pierce") # p-value = 0.9879 -> los errores son ruido blanco, o sea independientes.
Box.test(residuals(ARIMA111), type = "Box-Pierce") # p-value = 0.4193 -> los errores son ruido blanco, o sea independientes.
Box.test(residuals(ARIMA210), type = "Box-Pierce") # p-value = 0.8121 -> los errores son ruido blanco, o sea independientes.

#Mejor e ARIMA 211

## 5. Parsimonia
confint(ARIMA112) # MA2 incluye el 0.
confint(ARIMA211) # AR1 y MA1 incluyen el 0.
confint(ARIMA111) # Ninguno incluye el 0.
confint(ARIMA210) # Ninguno incluye el 0.

# Los mejores son el ARIMA 111 Y ARIMA 210

## 6. Regiones Admisibles

#ARIMA 112
ar_coefs <- ARIMA112$model$phi
ma_coefs <- ARIMA112$model$theta

# Raíces AR
raices_ar <- polyroot(c(1, -ar_coefs))
modulo_ar <- Mod(raices_ar) # 2.462464

# Raíces MA
raices_ma <- polyroot(c(1, ma_coefs))
modulo_ma <- Mod(raices_ma) # 1.431198 2.567000

# ARIMA 211
ar_coefs <- ARIMA211$model$phi
ma_coefs <- ARIMA211$model$theta

# Raíces AR
raices_ar <- polyroot(c(1, -ar_coefs))
modulo_ar <- Mod(raices_ar) # 1.975602 1.975602

# Raíces MA
raices_ma <- polyroot(c(1, ma_coefs))
modulo_ma <- Mod(raices_ma) # 3.054926

# ARIMA 111
ar_coefs <- ARIMA111$model$phi
ma_coefs <- ARIMA111$model$theta

# Raíces AR
raices_ar <- polyroot(c(1, -ar_coefs))
modulo_ar <- Mod(raices_ar) # 2.300787

# Raíces MA
raices_ma <- polyroot(c(1, ma_coefs))
modulo_ma <- Mod(raices_ma) # 1.670447

# ARIMA 210
ar_coefs <- ARIMA210$model$phi
ma_coefs <- ARIMA210$model$theta

# Raíces AR
raices_ar <- polyroot(c(1, -ar_coefs))
modulo_ar <- Mod(raices_ar) # 2.067341 2.067341


#Todos fueron mayores a 1, ent son inv y estacionarios respectivamente

## 7. Modelo Estable
# ARIMA 112
cov_mat_112 <- vcov(ARIMA112)
cor_mat <- cov2cor(cov_mat_112)
corrplot(cor_mat, method = "color", type = "upper", 
         addCoef.col = "black", # Agrega valores
         tl.col = "black", tl.srt = 45, # Color y rotación de etiquetas
         title = "Matriz de Correlación de Coeficientes ARIMA(1,1,2)",
         mar=c(0,0,2,0))  # Márgenes para dejar espacio al título

#ARIMA 211
cov_mat_211 <- vcov(ARIMA211)
cor_mat <- cov2cor(cov_mat_211)
corrplot(cor_mat, method = "color", type = "upper", 
         addCoef.col = "black", # Agrega valores
         tl.col = "black", tl.srt = 45, # Color y rotación de etiquetas
         title = "Matriz de Correlación de Coeficientes ARIMA(1,1,2)",
         mar=c(0,0,2,0))  # Márgenes para dejar espacio al título


#ARIMA 111
cov_mat <- vcov(modelo_111_limpio)
cor_mat <- cov2cor(cov_mat)
corrplot(cor_mat, method = "color", type = "upper", 
         addCoef.col = "black", # Agrega valores
         tl.col = "black", tl.srt = 45, # Color y rotación de etiquetas
         title = "Matriz de Correlación de Coeficientes ARIMA(1,1,1)",
         mar=c(0,0,2,0))  # Márgenes para dejar espacio al título  

#ARIMA 210
cov_mat <- vcov(ARIMA210)
cor_mat <- cov2cor(cov_mat)
corrplot(cor_mat, method = "color", type = "upper", 
         addCoef.col = "black", # Agrega valores
         tl.col = "black", tl.srt = 45, # Color y rotación de etiquetas
         title = "Matriz de Correlación de Coeficientes ARIMA(1,1,1)",
         mar=c(0,0,2,0))  # Márgenes para dejar espacio al título 

#L Todos tienen correlaciones altas con excepción del ARIMA 210

## 8. Valores Atipicos
# Outliers
outliers1 <- boxplot(residuals(ARIMA112), plot = FALSE) # 20 outliers de 385
outliers2 <- boxplot(residuals(ARIMA211), plot = FALSE) # 19 outliers de 385
outliers3 <- boxplot(residuals(ARIMA111), plot = FALSE) # 24 outliers de 385
outliers4 <- boxplot(residuals(ARIMA210), plot = FALSE) # 24 outliers de 385

# Puntos de Apalancamiento
# ARIMA 112
residuales1 <- residuals(ARIMA112)
residuales_std1 <- (residuales1 - mean(residuales1)) / sd(residuales1) #estandarización
plot(residuales_std1, type="h", main="Residuos Estandarizados ARIMA(1,1,2)", ylab="Residuo estandarizado")
abline(h=c(-3,3), col="red", lty=2)
outliers_apalancamiento1 <- which(abs(residuales_std1) > 3) # 26  28  35 328 330

# ARIMA 211
residuales2 <- residuals(ARIMA211)
residuales_std2 <- (residuales2 - mean(residuales2)) / sd(residuales2) #estandarización
plot(residuales_std2, type="h", main="Residuos Estandarizados ARIMA(2,1,1)", ylab="Residuo estandarizado")
abline(h=c(-3,3), col="red", lty=2)
outliers_apalancamiento2 <- which(abs(residuales_std2) > 3) # 26  28  35 328 330 332

# ARIMA 111
residuales3 <- residuals(ARIMA111)
residuales_std3 <- (residuales3 - mean(residuales3)) / sd(residuales3) #estandarización
plot(residuales_std3, type="h", main="Residuos Estandarizados ARIMA(1,1,1)", ylab="Residuo estandarizado")
abline(h=c(-3,3), col="red", lty=2)
outliers_apalancamiento3 <- which(abs(residuales_std3) > 3) # 26  35 328 330

# ARIMA 210
residuales4 <- residuals(ARIMA210)
residuales_std4 <- (residuales4 - mean(residuales4)) / sd(residuales4) #estandarización
plot(residuales_std4, type="h", main="Residuos Estandarizados ARIMA(1,1,1)", ylab="Residuo estandarizado")
abline(h=c(-3,3), col="red", lty=2)
outliers_apalancamiento4 <- which(abs(residuales_std4) > 3) # 26  28  35 328 330 332

# Outliers y Apalancamiento
# ARIMA 112
outliers1 <- boxplot(residuales1, plot = FALSE)$out
indices_outliers1 <- which(residuales1 %in% outliers1)

apalancamiento_es_outlier1 <- outliers_apalancamiento1 %in% indices_outliers1 # Todos los puntos apalancamiento son outliers
apalancamiento_es_outlier1

# ARIMA 211
outliers2 <- boxplot(residuales2, plot = FALSE)$out
indices_outliers2 <- which(residuales2 %in% outliers2)

apalancamiento_es_outlier2 <- outliers_apalancamiento2 %in% indices_outliers2 # Todos los puntos apalancamiento son outliers
apalancamiento_es_outlier2

# ARIMA 111
outliers3 <- boxplot(residuales3, plot = FALSE)$out
indices_outliers3 <- which(residuales3 %in% outliers3)

apalancamiento_es_outlier3 <- outliers_apalancamiento3 %in% indices_outliers3 # Todos los puntos apalancamiento son outliers
apalancamiento_es_outlier3

# ARIMA 210
outliers4 <- boxplot(residuales4, plot = FALSE)$out
indices_outliers4 <- which(residuales4 %in% outliers4)

apalancamiento_es_outlier4 <- outliers_apalancamiento4 %in% indices_outliers4 # Todos los puntos apalancamiento son outliers


# Hasta ahora el mdelo que mejor se ajusta y cumple con todos los supuestos sin problema ha sido el ARIMA 210 
# No se van a quitar los puntos influyentes porque solo son 6 de 385 datos.

# Pronóstico 
# El modelo es un ARIMA 210 con phi1 =  0.1140303 y phi2 = -0.2339785 
# Coeficientes del modelo ARIMA(2,1,0)
phi1 <- ARIMA210$coef["ar1"] 
phi2 <- ARIMA210$coef["ar2"]

pronosticos0 <- forecast(ARIMA210, h=10) #primeros 10 pronosticos 
pronosticos <- InvBoxCox(pronosticos0$mean,lambda = lambda)



serie_completa <- ts(c(Serie, pronosticos),
                     start = start(Serie),
                     frequency = frequency(Serie))

autoplot(serie_completa) +
  autolayer(pronosticos, series = "Pronóstico", PI = FALSE) +
  ggtitle("Pronóstico con ARIMA(2,1,0)") +
  ylab("Índice") +
  xlab("Gráfico 19") +
  theme_minimal()

# Evaluando el primer pronóstico
primer_pronostico <- pronosticos[1] # 101.0301
ultimo_valor_real <- tail(data[[2]], 1) #103.2298
error_absoluto <- abs(ultimo_valor_real - primer_pronostico) # 2.199634

#Intervalos de Predicción
# Cálculo de Psi
h <- 10
psi <- numeric(h)
psi[1] <- 1  # psi_0

if (h >= 2) psi[2] <- phi1 + phi2
if (h >= 3) {
  for (j in 3:h) {
    psi[j] <- phi1 * psi[j-1] + phi2 * psi[j-2]
  }
}

# Stdv Residuos
sigma <- sd(residuals(ARIMA210))

# Limites
Z <- 1.96  # para 95%
intervalos <- data.frame(
  t = 1:h,
  pronostico = pronosticos,
  inferior = NA,
  superior = NA
)

# Expresión completa
for (i in 1:h) {
  suma_psi2 <- sum(psi[1:i]^2)
  margen <- Z * sigma * sqrt(suma_psi2)
  
  intervalos$inferior[i] <- pronosticos[i] - margen
  intervalos$superior[i] <- pronosticos[i] + margen
}

intervalos

#Gráfica pronósticos con Banda de Confianza
ggplot(intervalos, aes(x = t, y = pronostico)) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = inferior, ymax = superior), fill = "lightblue", alpha = 0.4) +
  ggtitle("Pronósticos ARIMA(2,1,0) con intervalos (manual, vía ψ)") +
  xlab("Horizonte (h)") +
  ylab("Valor pronosticado (escala original)") +
  theme_minimal()

### GRÁFICAS PARA REPORTE ###

# Crear tabla resumen de supuestos
tabla_supuestos <- data.frame(
  Supuesto = c(
    "Media cero en errores",
    "Varianza constante",
    "Normalidad (95%)",
    "Errores independientes",
    "Parsimonia (IC ≠ 0)",
    "Estacionariedad (raíces AR > 1)",
    "Invertibilidad (raíces MA > 1)",
    "Estabilidad (correlación baja)",
    "Outliers detectados",
    "Apalancamientos detectados"
  ),
  ARIMA_112 = c(
    "Sí (p = 0.1092)",
    "Sí (p = 0.6917)",
    "Sí (97.66234 %)",
    "Sí (p = 0.9172)",
    "No (MA2 incluye 0)",
    "Sí (2.46)",
    "Sí (1.43, 2.56)",
    "No (alta correlación)",
    "20",
    "5"
  ),
  ARIMA_211 = c(
    "Sí (p = 0.1642)",
    "Sí (p = 0.6672)",
    "Sí (97.92208 %)",
    "Sí (p = 0.9879)",
    "No (AR1 y MA1 incluyen 0)",
    "Sí (1.97, 1.97)",
    "Sí (3.05)",
    "No (alta correlación)",
    "19",
    "6"
  ),
  ARIMA_111 = c(
    "Sí (p = 0.3255)",
    "Sí (p = 0.4926)",
    "Sí (97.66234 %)",
    "Sí (p = 0.4193)",
    "Sí",
    "Sí (2.30)",
    "Sí (1.67)",
    "No (alta correlación)",
    "24",
    "4"
  ),
  ARIMA_210 = c(
    "Sí (p = 0.2103)",
    "Sí (p = 0.6645)",
    "Sí (97.66234 %)",
    "Sí (p = 0.8121)",
    "Sí",
    "Sí (2.06, 2.06)",
    "-",
    "Sí (baja correlación)",
    "24",
    "6"
  )
)

##Actualización de pronóstico

data
Indice <- data[,2]
feb <- tail(Indice,1) #quitar febrero de la serie
pron_actualizado <- c()
#Las actualizaciones del pronóstico serían
a<- BCST[N] - phi1*BCST[N-1]-phi2*BCST[N-2]

for (i in 1:9) {
  
  pron_actualizado[i] <- pronosticos[i] - psi[i] * a
}

pron_actualizado
#Al principio cuando


# Mostrar tabla bonita con kable
kable(tabla_supuestos, caption = "Resumen de Supuestos Evaluados para Cada Modelo ARIMA") %>%
  kable_styling(full_width = F, bootstrap_options = c("striped", "hover", "condensed"))