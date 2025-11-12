##################################
# PRACTICAC ANOVA DE 1 FACTOR
# modelo de regresion, la variable x no es numerica, es categórica
# modelo mismo
# y = F(x) + E
y = c(85, 72, 83, 80, 80, 84, 81, 78, 82, 82, 80, 85, 90, 88)
x = c(rep("P1", 4), rep("P2", 5), rep("P3", 5))
y; x
x = as.factor(x) #vector x como factor
x
# Data frame, 
datos = data.frame(nota = y, programa = x)
datos
attach(datos)
#############################
# ANALISIS EXPLORATORIO

library(psych)
describeBy(nota, programa)
plot(programa,nota)

library(PASWR)
plot.design(nota~programa,data=datos)

mean(nota)#media global

?oneway.plots()
oneway.plots(nota,programa)
par(mfrow=c(1,1))

#######################
#analisis de variabilidad
plot(nota,programa)
abline(v=mean(y),col=2)
points(c(80,81,85),c(1,2,3),col=4,pch=8)
#calculo de promedios
prom.gral=mean(nota)
prom.p1=mean(nota[programa=="P1"])
prom.p2=mean(nota[programa=="P2"])
prom.p3=mean(nota[programa=="P3"])

n=length(nota)#total muestra
k=3#total de programas
#variabilidad total
SCT=sum((nota-prom.gral)^2)
gl.T=n-1
MCT=SCT/gl.T
MCT#variabilidad total= varianza muestral de y
var(nota)
#variabilidad intermuestral
SCinter= 4*(prom.p1-prom.gral)^2 +
  5*(prom.p2-prom.gral)^2+
  5*(prom.p3-prom.gral)^2
SCinter
glinter=k-1
glinter
MCinter = SCinter/glinter
MCinter
#variabilidad intramuestral
vec.prom= c(rep(prom.p1,4),rep(prom.p2,5),rep(prom.p3,5))
vec.prom
SCintra = sum((nota-vec.prom)^2)
SCintra
glintra=n-k
glintra
MCintra = SCintra/glintra
MCintra
#comprobacion
SCT
SCinter+SCintra
gl.T
glinter+glintra
##
#prueba f
F.test = MCinter/MCintra
F.test
P.valor = 1-pf(F.test, glinter, glintra)
P.valor
##############################
?aov
modelo = aov(nota~programa)
summary(modelo)

#################################
#comprobacion supuestos

library(car)
?leveneTest
leveneTest(modelo) #si se cumple omosedasticidad

library(nortest)
resid=modelo$residuals
resid
shapiro.test(resid)#si hay mas de 50 residuos

library(lmtest)
dwtest(modelo,alternative = "two.sided")
#si se cumple la independencia
#############################
#pruebas de comparacion multiple
#solo si la H0 se rechaza
#para identificar medidad distintas

#ej 13.23
y=c(55,55,57,54,54,56,
    60,61,60,60,60,60,
    70,72,72,68,77,77,
    72,72,72,70,68,69,
    65,66,60,64,65,65)
x=c(rep("0C",6),rep("25C",6),rep("50C",6),rep("75C",6),rep("100C",6))
x=as.factor(x)
x=as.factor(x) #aqui si declaramos x como un factor
x

##################################
#Creamos un data frame 
datos=data.frame(segundos=y, temperatura=x)
datos
attach(datos) #carga las variables del data frame en memoria

##################################
#ANALISIS EXPLORATORIO

library(psych)
describeBy(segundos,temperatura)
plot(temperatura,segundos)

library(PASWR)
plot.design(segundos~temperatura, data=datos)

mean(segundos) #media global

oneway.plots(segundos,temperatura)
par(mfrow = c(1,1))

#########################################
#Analasis de variabilidad

plot(segundos, temperatura)
abline(v = mean(y), col=2)

#varianza INTRAmuestral es la de cada dato con su media local
#varianza INTERmuestral es la de cada campana (cada media local) respecto a la media global

##########################################
#Calculo de promedios
prom.gral=mean(segundos)
prom.0=mean(segundos[temperatura=="0C"])
prom.25=mean(segundos[temperatura=="25C"])
prom.50=mean(segundos[temperatura=="50C"])
prom.75=mean(segundos[temperatura=="75C"])
prom.100=mean(segundos[temperatura=="100C"])

n=length(segundos)
k=nlevels(x)

plot(segundos,temperatura)
abline(v=mean(y),col=2)
points(c(prom.0,prom.25,prom.50,prom.75,prom.100),c(1,2,3,4,5),col=4,pch=8)
k=nlevels(x)

SCT=sum((segundos-prom.gral)^2)
gl.T=n-1
MCT=SCT/gl.T
MCT#variabilidad total= varianza muestral de y
var(segundos)

SCInter=6*(prom.0-prom.gral)^2+
  6*(prom.25-prom.gral)^2+
  6*(prom.50-prom.gral)^2+
  6*(prom.75-prom.gral)^2+
  6*(prom.100-prom.gral)^2
SCInter
glinter=k-1
glinter
MCinter = SCInter/glinter
MCinter

vec.prom= c(rep(prom.0,5),rep(prom.25,5),rep(prom.50,5),rep(prom.75,5),rep(prom.100,5))
vec.prom
SCintra = sum((segundos-vec.prom)^2)
SCintra
glintra=n-k
glintra
MCintra = SCintra/glintra
MCintra
#comprobacion
SCT
SCInter+SCintra
gl.T
glinter+glintra
##
#prueba f
F.test = MCinter/MCintra
F.test
P.valor = 1-pf(F.test, glinter, glintra)
P.valor
##############################
?aov
modelo = aov(segundos~temperatura)
summary(modelo)
#COMPROBACIOB DE SUSPUESTOS
#HOMOCEDASTICIDAD: TEST DE LEVENE
#INDEPENDENCIA
#NORMALIDAD: TEST DE SHAPIRO WILK(n<50), Lilliefors

library(car)
?leveneTest
leveneTest(modelo) #si se cumple la homocedasticidad

library(nortest)
resid=modelo$residuals # calculo de residuuos
shapiro.test(resid) #Si se cumple la normalidad
#se cumplen todos los supuestos
#modelo ANOVA es valido

library(lmtest)
dwtest(modelo,alternative="two.sided")
###################################
#PRUEBAS DE COMPARACION MULTIPLE

library(asbio)
#pairw.anova(x=temperatura,y=segundos,method = "bonf")
#pairw.anova(x=temperatura,y=segundos,method = "scheffe")
pairw.anova(x=temperatura,y=segundos,method = "tukey")

#tukey=disenio equilibrado o balanceado(mismo tamanio en cada nivel)
#bonferrori= no balanceado con menos de 4 niveles
#scheffe = no balanceado con 4 niveles o mas

####################################
#13.18 Los datos siguientes son valores de presión (psi) en un resorte de 
#torsión para valores distintos del ángulo entre las vueltas del resorte 
#en posición libre.

y=c(83,85,
    84,85,85,86,86,87,
    86,87,88,88,88,90,87,87,88,88,89,
    89,90,90,91,
    90,92)
    
x=c(rep("67",2),rep("71",6),rep("75",11),rep("79",4),rep("83",2))

y; x
x = as.factor(x) #vector x como factor
x
# Data frame, 
datos = data.frame(psi = y, angulo = x)
datos
attach(datos)

##################################
#ANALISIS EXPLORATORIO

describeBy(psi,angulo)
plot(angulo,psi)

plot.design(psi~angulo, data=datos)

oneway.plots(psi,angulo)

par(mfrow = c(1,1))

#########################################
#Analasis de variabilidad

promg= mean(psi)
prom67 = mean(psi[angulo=="67"])
prom71 = mean(psi[angulo=="71"])
prom75 = mean(psi[angulo=="75"])
prom79 = mean(psi[angulo=="79"])
prom83 = mean(psi[angulo=="83"])
n=length(psi)
k=nlevels(x)

plot(psi, angulo)
abline(v = mean(y), col=2)
points(c(prom67,prom71,prom75,prom79,prom83),
       c(1,2,3,4,5),col=4,pch=8)

sct=sum((psi-promg)^2)
gl.t=n-1
mct=sct/gl.t
mct

var(psi)

scinter = 2*(prom67-promg)^2+
  6*(prom71-promg)^2 +
  11*(prom75-promg)^2+
  4*(prom79-promg)^2+
  2*(prom83-promg)^2
scinter

glinter=k-1
glinter
mcinter = scinter/glinter
mcinter

vecprom=c(rep(prom67,2),rep(prom71,6),rep(prom75,11),rep(prom79,4),rep(prom83,2))
scintra=sum((psi- vecprom)^2)
scintra
glintra=n-k
glintra
mcintra = scintra/glintra
mcintra
#COMPROBACION
sct
scinter+scintra

gl.t
glinter+glintra

######################
#PRUEBA F
ftest = mcinter/mcintra
ftest

pvalor = 1- pf(ftest,glinter,glintra)
pvalor
#h0 se rechaza

############################
Modelo = aov(psi~angulo)
summary(Modelo)

leveneTest(Modelo)

residuos = Modelo$residuals
residuos

shapiro.test(residuos)

dwtest(Modelo,alternative="two.sided")

pairw.anova(x=angulo,y=psi,method = "scheffe")