#Practica de REGRESION LINEAL SIMPLE

x=c(4.6,4.2,4.8,5.5,6.2,5.9,5.7,6.8,6.2,4.8) 
y=c(8.1,8.8,8.4,10.5,11.8,11.2,10.9,15.1,14.2,12) 
n=length(x) 

#Medidas descriptivas bidimensionales

vecmed=c(mean(x),mean(y))
vecmed

#GRAFICO DE DISPERSION
plot(x,y,main="Grafico de dispersion Grosor vs Tiempo de corte")
abline(v = mean(x),col=2)
abline(h = mean(y),col=2)

#COVARIANZA

Sxy=sum((x-mean(x))*(y-mean(y)))/n
Sxy

cov(x,y)*(n-1)/n


#####################################
#Modelos de regresion

Sxx=sum((x-mean(x))*(x-mean(x)))/n
Sxx

beta_1=Sxy/Sxx
beta_0=mean(y)-beta_1*mean(x)

beta_0;beta_1

#modelo estimado y = -1.68 + 2.34x

plot(x,y)
abline(a=beta_0,b=beta_1,col=2)

#Proyecciones
Proy = beta_0 + beta_1*x
Proy

points(x,Proy,col=2) 

#residuos
resid=y-Proy
resid 

#promedio de los residuos
promRes=mean(resid)
promRes

#suma de residuos cuadrado
sumResCuadrado=sum((y-Proy)^2)
sumResCuadrado
hist(resid)

sigma2 = sumResCuadrado/(n-2)
sigma2


err.tip.Beta0=sqrt(sigma2)*sqrt(1/n+(mean(x)^2)/(n*Sxx))
err.tip.Beta0

err.tip.Beta1=sqrt(sigma2)/sqrt(Sxx*n)
err.tip.Beta1

T.test.Beta0= beta_0 / err.tip.Beta0
T.test.Beta0
Pvalor.Beta0=2*pt(T.test.Beta0,n-2)
Pvalor.Beta0

T.test.Beta1= beta_1 / err.tip.Beta1
T.test.Beta1

Pvalor.Beta1=2*(1-pt(T.test.Beta1,n-2))
Pvalor.Beta1

SCT = sum((y-mean(y))^2)
SCT

R2= 1- sumResCuadrado/SCT
R2

SCreg= sum((Proy-mean(y))*2)
SCreg

SCreg+sumResCuadrado
SCT
########################################################
#funcion automatica = lm = lineal model
?lm
modelo = lm(y~x)
summary(modelo)
#####################################################
#Ejercicio FABRICA
x=c(5.6,5,5.8,6.6,7.4,7.1,6.8,8.1,7.4,5.7) 
y=c(7.7,10.4,7.2,6.4,4.6,5.1,5.8,5.3,7,11) 
n=length(x)

vecmed=c(mean(x),mean(y))
vecmed

plot(x,y)
abline(v = mean(x),col=2)
abline(h = mean(y),col=2)

#COVARIANZA

Sxy=sum((x-mean(x))*(y-mean(y)))/n
Sxy

cov(x,y)*(n-1)/n

#Modelos de regresion
Sxx=sum((x-mean(x))*(x-mean(x)))/n
Sxx

beta_1=Sxy/Sxx
beta_0=mean(y)-beta_1*mean(x)

beta_0;beta_1

#modelo estimado y = 18.61 -1.74x

plot(x,y)
abline(a=beta_0,b=beta_1,col=2)

Proy = beta_0 + beta_1*x
Proy

points(x,Proy,col=2)

resid=y-Proy
resid 

#promedio de los residuos
promres=mean(resid)
promres 

#suma de residuos cuadrado
sumRes2=sum((y-Proy)^2)
sumRes2

hist(resid)

sigma2 = sumRes2/(n-2)
sigma2

errBeta0=sqrt(sigma2)*sqrt(1/n+(mean(x)^2)/(n*Sxx))
errBeta0

errBeta1= sqrt(sigma2)/sqrt(Sxx*n)
errBeta1

tbeta0 = beta_0/errBeta0
tbeta0
tbeta1 = beta_1/errBeta1
tbeta1

pbeta0 = 2 * (1 - pt(abs(tbeta0), n - 2))
pbeta1 = 2 * (1 - pt(abs(tbeta1), n - 2))

pbeta0;pbeta1


Modelo = lm(y~x)
summary(Modelo)