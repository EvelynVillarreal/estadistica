#Y = tiempo uso maquina (min)
#X1 = nro unidades producidas
#X2 = nro de unidades reprocesadas
# Y = f(X1, X2) = B0 + B1x1 + B2x2

#datos observados
y=c(40,35,30,20,25) #usaste la maquina 40 min
x1=c(100,90,80,75,70) #en esos 40 min produciste 100 unidades
x2=c(35,32,28,20,30) # de esas 100 unidades 35 son reprocesadas

n=length(y)
p= 3 #nro de betas

#construir las matrices Y y X (matriz de disenio)

Y=matrix(y,n,1)
X=matrix(c(rep(1,n),x1,x2), n, p)
Y;X
#construir el sistema de ecuaciones normales de regresion: A * (BETA) = b
A=t(X) %*% X  #eso es para que sea producto matricial
A # A tiene orden p x p

b=t(X) %*% Y
b #b tiene orden px1

#Beta por MCO se obtiene resolviendo el sistema

Ainv = solve(A)
Ainv

B.MCO= Ainv %*% b
B.MCO #betas estimados por MCO

# Modelo de regresion lineal multiple
# Y = -21.856 + 0.379x1 + 0.7x2 + E

#Modelo de regresion lineal multiple genera un hiperplano de proyeccion en el espacio Rp

y[1]; x1[1]; x2[1] #datos observados

X[1,] %*% B.MCO #proyeccion del modelo de regresion lineal multiple

B.MCO[1][1] + B.MCO[2][1] * x1[1] + B.MCO[3][1] * x2[1] #es lo mismo

#Residuo = Y - Proy Y

y[1] - X[1,] %*% B.MCO


#CALCULAR EL VECTOR DE PROYECCIONES Y DE RESIDUOS

Proy = X %*% B.MCO
Proy

resid=Y-Proy
resid

plot(1:n, Y)
points(1:n, Proy, col=2)

#Calcular  el promedio, SCR, varianza del residuo
prom=mean(resid)

SCR=sum(resid^2)
SCR

sigma2=SCR/(n-p)
sigma2
sigma=sqrt(sigma2)
sigma
#################################
#inferencia
cov.beta=sigma2*Ainv
cov.beta

err.tip.beta=sqrt(diag(cov.beta))
err.tip.beta
T.test.beta=B.MCO/err.tip.beta
T.test.beta
P.valor.beta=2*(1-pt(abs(T.test.beta),n-p))
P.valor.beta
#grados de libertad es n-p

## OJO todos los betas, P valores son altos, entonces, los betas se anulan
#Posibles causas, muestra muy pequeña
# No se cumplen los supuestos, linealidad, 
# Homocedasticidad, Normalidad, Independencia

##################################
# R Cuadrado = coef de determinacion
# 1 - suma cuadrada de lso Residuos, sobre la suma cuadrada total

SCT = sum((y - mean(y))^2)
R2 = 1 - SCR/SCT
R2

# R cuadrado ajustado

R2.aj = 1 - sigma2 / var(y)
R2.aj

## Prueba F = suma cuadrada de regreecion, sobre suma cuadrada de residuo
## ambas dividia por sus grados de libertad

# SCT = SCreg + SCR
SCreg = SCT - SCR
SCreg

F.test = (SCreg / (p - 1))/(SCR / (n-p))
F.test

P.valor.F = 1 - pf(F.test, p - 1, n - p)
P.valor.F

# hipotesis nula de la prueba F, 
# Como el P valor es bajo, entonces Y si depende de algun Xi
# Esto parece contradecir las pruebas T no dependen d eninguno

########### Linealidad
library(lmtest)
?reset
reset(modelo)
# No se puede comrpobar linealidad por q la muestra es muy pequeña
# Comprobar Homocedasticidad
?bptest
bptest(modelo)
# como el P valor es alto, se cumple la homocedasticidad

# independencia
?dwtest
dwtest(modelo, alternative = "two.sided")
# Como Pvalor es alto, si se cumple la Independencia

# normalidad
?shapiro.test
shapiro.test(resid)
# P valor mayor a 5% con las justas se cumplen
# En reusmen, el modelo RLM no cumple la linealidad, muestra muy pequeña
# Por esa razon el modelo no es adecuado

# Supeusto lienal, multi colinealidad, solo en regresion lineal multiple
# MULTICOLINEALIDAD (SOLO EN RLM)
# evitar la multi colinealidad, s edebe medir el factor de inflación de Varainza
# vif
library(car)
?vif

vif(modelo) # OJO todos los vif deben ser menores a 5
# Si alguna varaible tiene un vif mayor a 5, se debe eliminar del modelo

################# SUPOINGAMOS QUE EL MODELO ES VALIDO(PRUEBAS APARTE)
#Predicciones en Regrecion Lineal Multiple (RLM) (usamdo el objeto "lm)
# digamos que quiero fabricar 95 uniaddes, y reprocesar 25 unidades (no estan en los datos)
#Mientras no se salga del rango/soporte, puedo dar predicciones, entre valores min y max, de cada x

#se desea predecir el tiempo de uso de maquina para x1 = 95 unidades producides
# x2 = 25 unidades reprocesadas
# OJO x1 x2 valores interpolados dentro del rango de interpolacion
?predict
new = data.frame( x1 = c(95, 92), x2 = c(25, 27))
#Estimacion de la media condicionada (rango del hiperplano RLM)
predict(modelo, newdata = new, interval = "confidence") #Por donde pasa el hiperplano

#Prediccion puntual (incluyendo el error)
predict(modelo, newdata = new, interval = "prediction") #Por donde va a caer el punto
##############################
#Funcion automatica lm

modelo=lm(y ~ x1 + x2)
summary(modelo)

y = c(240, 136, 190, 274, 301)
x1 = c(25, 31, 45, 60, 65)
x2 =  c(91, 90, 88, 87, 91)

n=length(y)
p= 3 #nro de betas

#construir las matrices Y y X (matriz de disenio)

Y=matrix(y,n,1)
X=matrix(c(rep(1,n),x1,x2), n, p)
Y;X

#construir el sistema de ecuaciones normales de regresion: A * (BETA) = b
A=t(X) %*% X  #eso es para que sea producto matricial
A # A tiene orden p x p

b=t(X) %*% Y
b #b tiene orden px1

#Beta por MCO se obtiene resolviendo el sistema

Ainv = solve(A)
Ainv

B.MCO= Ainv %*% b
B.MCO #betas estimados por MCO

#CALCULAR EL VECTOR DE PROYECCIONES Y DE RESIDUOS

Proy = X %*% B.MCO
Proy

resid=Y-Proy
resid

plot(1:n, Y)
points(1:n, Proy, col=2)

#Calcular  el promedio, SCR, varianza del residuo
prom=mean(resid)

SCR=sum(resid^2)
SCR

sigma2=SCR/(n-p)
sigma2
sigma=sqrt(sigma2)
sigma

#grados de libertad es n-p

##############################
#Funcion automatica lm

modelo=lm(y ~ x1 + x2)
summary(modelo)


##################
x1=c(1,2,3,4,8,9,10)
y=c(812.52,822.50,1211.50,1348.00,1301.00,2567.50,2526.50)

n=length(x)

