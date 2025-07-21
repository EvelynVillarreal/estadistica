################
# REGRESION LINEAL SIMPLE
#################
# CARGAR LOS DATOS
# x = Runsize (unidades), y = Runtime (min)
x = c(175, 189, 344, 88, 114, 338, 271, 173, 284, 277)
y = c(195, 215, 243, 162, 185, 231, 234, 166, 253, 196)

n = 10  # total de pares ordenados (X, Y)

# Grafico de dispersion
plot(x, y)

# Vector de medias
vecmed = c(mean(x), mean(y))
vecmed

# Covarianza muestral
Sxy = sum ((x - mean(x))*(y - mean(y))) / n
Sxy

Sxy = sum(x*y)/n - mean(x)*mean(y)
Sxy

# Covarianza cuadrada de x
S2x =  sum ((x - mean(x))^2) / n
S2x

# ESTIMACION DE LOS BETAS
# Para Beta 1
Beta1 = Sxy / S2x
Beta1

# Para Beta 0
Beta0 = mean(y) - Beta1 * mean(x)
Beta0

# Proyecciones de Y
Yproy = Beta0 + Beta1*x
Yproy

# Residuos de la regresión
resid = y - Yproy
resid

# Grafico de la recta de proyeccion
plot(x, y)
points(x, Yproy, type="l", col =2)

# Suma de residuos
sum(resid) # practicamente CERO

# Suma cuadrada de residuos: SCR
SCR = sum(resid^2)
SCR

# Estimacion varianza del error
sigma2 = SCR / (n-2)
sigma2

sigma = sqrt(sigma2) # desviación estándar de residuos
sigma

# INFERENCIA PARA LOS BETAS

# Para beta Cero
etB0 = sigma * sqrt((1/n)+((mean(x)^2)/(n*S2x)))
etB0 # estimacion del error típico del beta cero

tB0 = Beta0 / etB0
tB0 # estadistico de contraste de Beta 0

PvalorB0 = 2*(1 - pt(tB0, n-2))
PvalorB0 # es menor al 5% , H0 se rechaza (Beta 0 es distinto de 0)

# Para Beta 1
etB1 = sigma / sqrt(n*S2x)
etB1

tB1 = Beta1 / etB1
tB1

PvalorB1 = 2*(1 - pt(tB1, n-2))
PvalorB1 # es menor al 5% , H0 se rechaza (Beta 1 es distinto de 0)

# CALCULO USANDO LA FUNCION LINEAR MODEL
RSL1 = lm(y ~ x) #devuelve un objeto de clase "lm"

RSL1$coefficients
RSL1$fitted.values
RSL1$residuals

summary(RSL1)


# DESCOMPOSICION DE LA VARIABILIDAD
SCT = sum((y - mean(y))^2) # suma cuadrada total/global
SCT

SCReg = sum((Yproy - mean(y))^2) # suma cuadrada de la regresion
SCReg

SCR = sum((y - Yproy)^2) # suma cuadrada del residuo
SCR

SCReg + SCR #esta suma debe ser igual a la SCT

# PRUEBA GLOBAL F (contraste F)
Ftest = (SCReg/1) / (SCR/(n-2)) # estadistico F
Ftest

PvalorF = 1 - pf(Ftest, 1, n-2)
PvalorF # Este valor es menor al 5%, por tanto la H0 se rechaza
# Beta1 es distinto de cero, luego Y si depende de X

# COEFICIENTES DE DETERMINACION
R2 = 1 - (SCR/SCT)
R2 # 68% de la variabilidad de Y es explicada con el modelo de RLS 
# (32% se pierden a los residuos)

R2adj = 1 - ((SCR/(n-2))/(SCT/(n-1)))
R2adj #64% de la variabilidad se explica por el modelo de RLS

1 - (sigma2/var(y)) # formula alternativa

#CALCULO AUTOMATICO
summary(RSL1)

# PREDICCION EN RLS
x0 = 200
y0 = Beta0 + Beta1*x0
y0

# IC para la media condicionada (IC - recta proyectada)

# calculo del tamaño equivalente n0
n0 = n / (1 + (((200 - mean(x))^2)/S2x))
n0

LIC.MC = y0 - qt(0.975, n-2)*sigma/sqrt(n0)
LSC.MC = y0 + qt(0.975, n-2)*sigma/sqrt(n0)
LIC.MC
LSC.MC

# IC para la prediccion puntual (IC para Y0)
LIC.y0 = y0 - qt(0.975, n-2)*sigma*sqrt(1+ 1/n0)
LSC.y0 = y0 + qt(0.975, n-2)*sigma*sqrt(1+ 1/n0)
LIC.y0
LSC.y0

# PREDICCION DE FORMA AUTOMATICA
newX = c(200, 250) # solo se puede predecir de forma interpolada

# IC para la media condicionada
predict(RSL1, newdata = data.frame(x = newX), interval = "confidence", level = 0.95)

# IC para la prediccion puntual
predict(RSL1, newdata = data.frame(x = newX), interval = "prediction", level = 0.95)

# DIAGNOSTICO DEL MODELO
library(lmtest) #este paquete contiene los contrastes de diagnosticos
# debemos tener previamente construido el objeto "lm"

# Comprobar linealidad (test RESET)
reset(RSL1) # P Valor mayor a 5%, H0 de linealidad se cumple

# Comprobar homocedasticidad (test Harrison Mc Cabe)
hmctest(RSL1) # P valor menor al 5%, H0 de homocedasticidad no se cumple

# Comprobar normalidad (test Shapiro wilk)
shapiro.test(resid) #P valor mayor a 5%, H0 de normalidad se cumple

library(nortest) 
lillie.test(resid) # test de lilliefors cuando n es mayor o igual a 50

# Comprobar independencia (Test de Durbin Watson)
dwtest(RSL1, alternative = "two.sided") # P valor mayor a 5%, H0 de independencia se cumple

#analisis gráfico
plot(RSL1)


###################################################################################

#REGRECION LINEAL SIMPLE
x=c(4.6,4.2,4.8,5.5,6.2,5.9,5.7,6.8,6.2,4.8) #grososr en cm
y=c(8.1,8.8,8.4,10.5,11.8,11.2,10.9,15.1,14.2,12) #tiempo de corte en m
n=length(x) #tamaño muestral =numero de pares ordenados

#MEIDAS DESCRIPTIVAS BIDIMENCIONALES
vecmed = c(mean(x),mean(y))
vecmed
#en promedio el grosor es de 5.47 y el tiempo de corte de 11.10m

#Grafico de dispersion
plot(x,y.main="Grafico de dispercion de tiempo por grosor")
abline (v=mean(x),col=2)
abline (h=mean(y),col=2)


#Calculo de la covarianza
sxy=sum(x-mean(x))*(y-mean(y))/n
sxy

#formula para la covarianza (pero no da lo mismo)
cov(x,y)*(n-1)/n #ojo con la funcion que esta dividida para n-1

#MODELOS DE REGRESION
#si la covarianza es distienta de 0
#se asigna a una como dependiente y a otra como idpendiente
#decimos que una variable es funcion de la optra
Sxx = sum(x-mean(x))*(x-mean(x))/n
Sxx
Beta_1 = sxy/Sxx
Beta_1

Beta_0 = mean(y) - Beta_1 * mean(x)
Beta_0

#modelo RLS estimado: Y = 1.675 + 2.336x + ro E
plot(x,y)
abline(a=Beta_0,b=Beta_1,col=2)

#proyecciones del modelo
Proy = Beta_0 + Beta_1*x
Proy

points(x, Proy,col=2)
resid=y - Proy
resid
