################################
# PROPIEDADES DE LA DISTRIB. NORMAL

###################################
# PROPIEDAD REPRODUCTIVA DE LA DISTRIBUCION NORMAL

# generamos tres m.a.s normales con parametros distintos, con n = 1000
set.seed(1234) #fijamos semillas

m1 <- rnorm(1000, mean =1, sd = sqrt(2)) # N(1, raiz(2)) - Var1 = 2
m2 <- rnorm(1000, mean =2, sd = sqrt(3)) # N(2, raiz(3)) - Var2 = 3
m3 <- rnorm(1000, mean =3, sd = sqrt(4)) # N(3, raiz(4)) - Var3 = 4

# sumamos las muestras
m.sum <- m1+m2+m3

# estadisticos descriptivos
mean(m.sum) # media de la suma es suma de las medias
var(m.sum) #varianza de la suma es la suma de varianzas

# histograma de la suma 
hist(m.sum, freq = FALSE, ylim = c(0, 0.15)) 
curve(dnorm(x, 6, sqrt(9)), -3, 18, add = TRUE, col = 2)


###################################
# FORMULA DE ESTANDARIZACION APLICADA A LA NORMAL
# tomamos la m.a.s m3 -> N(3, raiz(4))

hist(m3, freq = FALSE) 
mu <- mean(m3)
sigma <- sd(m3)
curve(dnorm(x, 3, sqrt(4)), -3, 18, add = TRUE, col = 2)

z <- (m3 - mu)/sigma
hist(z, freq = FALSE)
curve(dnorm(x, 0, sqrt(1)), -4, 4, add = TRUE, col = 3)
mean(z)
sd(z)

curve(dnorm(x, 3, sqrt(4)), -4, 10, col = 2, ylim = c(0, 0.4))
curve(dnorm(x, 0, sqrt(1)), -4, 10, add = TRUE, col = 3)

###################################
# DISTRIBUCION CHI CUADRADA
# generamos una matriz con 30 m.a.s. de v.a. Z con n = 1000 cada una
set.seed(1)
matriz.z <- replicate(1000, rnorm(30)) # 30 filas, 1000 columnas
hist(matriz.z[1,])

matriz.z2 <- matriz.z^2 #elevamos al cuadrado cada v.a. Z

# generamos 1000 v.a. chi cuadrada sumando cada columna
datos.chi <- colSums(matriz.z2)

# histograma de los datos simulados
hist(datos.chi, freq = FALSE)
curve(dchisq(x, 30), 0, 60, col = 2, add = TRUE)
# observe que la distribucion chi cuadrada tiene 30 grados de libertad

# media teorica = n
mean(datos.chi) # promedio muestral simulado

# varianza = 2n
var(datos.chi)# varianza muestral simulada

###################################
# DISTRIBUCION T DE STUDENT

# generamos una m.a.s. de 1000 v.a. Z
datos.z <- rnorm(1000)

# Construimos 1000 variables T utilizando la v.a. chi anterior
# ojo: g.l.(chi cuadrada) = 30
datos.t <- datos.z/(sqrt(datos.chi/30))

# histograma de los datos simulados
hist(datos.t, freq = FALSE, ylim =c(0, 0.4))
curve(dt(x, 30), -3, 3, col = 2, add = TRUE)

# media teorica = 0
mean(datos.t) #promedio muestral simulado

# varianza = n/(n-2)
30/28 # sigma cuadrada teorica
var(datos.t) # varianza muestral

###################################
# DISTRIBUCION F DE SNEDECOR

# Simularemos 1000 v.a. chi cuadrada, con 20 grados de libertad
set.seed(1)
matriz.z <- replicate(1000, rnorm(20))
matriz.z2 <- matriz.z^2 #elevamos al cuadrado cada v.a. Z
datos.chi2 <- colSums(matriz.z2)

# Construimos la v.a. F
datos.f <- (datos.chi/30)/(datos.chi2/20)

# histograma de los datos simulados
hist(datos.f, freq = FALSE, ylim = c(0, 1.1))
curve(df(x, 30, 20), 0, 4, col = 2, add = TRUE)

# media = n/(n-2)
mean(datos.f)
30/28


# histograma de los datos simulados
hist(datos.chi, freq = FALSE)
curve(dchisq(x, 30), 0, 60, col = 2, add = TRUE)
curve(dnorm(x,30,sqrt(60)), 0, 60, col = 3, add = TRUE)


# INTERVALO DE CONFIANZA PARA LA MEDIA
#EJERCICIO 9.7
#datos
prom=23500
n=100
desv=3900
nc=-0.99

#condiciones: Normalidad, muestra grande y sigma desconocido
#estadistico pivote z
alfa=1+nc
z.alfa=qnorm(1-alfa/2)

#error estimado
err=z.alfa*desv/sqrt(n)
err

#limites de confianza
LIC=prom-err
LSC=prom+err
LIC;LSC


##############################################
#Distribuvion muestral del promedio bajo
#T de STUDENT

#Condiciones: poblacion es normal, muestra es pequeña

set.seed(1234) #fijamos semillas

m1 <- rnorm(1000, mean =1, sd = sqrt(2)) # N(1, raiz(2)) - Var1 = 2
m2 <- rnorm(1000, mean =1, sd = sqrt(2)) # N(1, raiz(2)) - Var1 = 2
m3 <- rnorm(1000, mean =1, sd = sqrt(2)) # N(1, raiz(2)) - Var1 = 2
m4 <- rnorm(1000, mean =1, sd = sqrt(2)) # N(1, raiz(2)) - Var1 = 2

prom=(m1+m2+m3+m4)/4 #promedio de x1,x2,x3,x4

#asumiremos que la varianza poblacional (sigma2=2) es desconocida
#usaremos la varianza muestral

var.prom = var(prom) #varianza muestral
# es muy diferente al valor de sigma 2 = 2

#error tipico
err.est=sqrt(var.prom/4)
err.est

sqrt(2/4) # hay mucha diferentea entre las varfianzas, teoricas y estimadas, ya no trabajmos
#con la normal
hist((prom - 1)/sqrt(2/4), freq = FALSE)
curve(dnorm(x), -6, 6, col = 2, add = TRUE)
curve(dt(x, 4 -1), -6, 6, col = 4, add = TRUE) # grados de libertad = n-1

#IC para la media 
# IC = (promedio menos erro estimado; promedio + error estimado) = Intervalo de confianza
#Usar probabilidad de 90%, las regiones que sale, (media no peretenece al intervalo de confianza)
#probabilidad de alfa, si es 90%, con 10% debería pasar, 5% para q salga muy abaajo, y 5% muy arriba
#Error positivo, seria el que corresponde, el 5% mayor

###########################
#eJERCICIO 9.15

n = 12
prom = 48.5
s = 1.5
nc = 0.90
alfa = 0.10

T.alfa = qt(1 - alfa/2, n-1)
T.alfa
err.est = T.alfa *s/ sqrt(n)
err.est

LIC = prom - err.est
LSC = prom + err.est
LIC; LSC

#Una maquina produce 
datos = c(1.01, 0.97, 1.03, 1.04, 0.99, 0.98, 0.99, 1.01, 1.03)

#IC 99%

sum(datos)

mean(datos)

var
sd(datos)

n = length(datos)
prom = mean(datos)
s = sd(datos)
n; prom; s

nc = 0.99
alfa = 0.01

T.alfa = qt(1 - alfa/2, n-1)
T.alfa
err.est = T.alfa *s/ sqrt(n)
err.est

LIC = prom - err.est
LSC = prom + err.est
LIC; LSC

t.test

t.test(datos, conf.level = 0.99)

# Con una muestra grande z y te seran iguales

#La formula me sirve en todos los casos

##################################
# CONVERGENCIA ENTRE AL DISTRI.t Y LA DISTR.Z
curve(dnorm(x), -3, 3) # distr
curve(dt(x,3), -3, 3, col = 2, add = TRUE)
curve(dt(x,15), -3, 3, col = 3, add = TRUE)
curve(dt(x,30), -3, 3, col = 4, add = TRUE)
curve(dt(x,5), -3, 3, col = 5, add = TRUE)
curve(dt(x,10), -3, 3, col = 6, add = TRUE)
curve(dt(x,20), -3, 3, col = 7, add = TRUE)
curve(dt(x,25), -3, 3, col = 8, add = TRUE)

#Objetivo calcular el tamaño de una M.A.S. para aproximar la media de una poblacion a partir de un IC
# prefijar el erro de estimacion

##################################
#Ejercicios 9.4 y 9.8

n = 30
sigma = 40
prom = 780
nc = 0.96
alfa = 0.04

Z.alfa = qnorm(1 - alfa/2)
Z.alfa

err.est = Z.alfa*sigma/sqrt(n)
err.est
LIC = prom - err.est
LSC = prom + err.est
LIC; LSC

n = Z.alfa^2*sigma^2/10^2
n
#Ojo, calculo de N, el valor NO se redondea, si no qwue se aproxima al entero superior
# ("celing")
ceiling(n) # Se redondea hacia arriba
set.seed(1)
## Variables categoricas
matriz.p = replicate(500,rbinom(100,1,0.10))

nro.defectos = colSums(matriz.p)
head(nro.defectos)
min(nro.defectos)
max(nro.defectos)
mean(nro.defectos)
p.est = nro.defectos/100
mean(p.est) # p= 0.1, promedio debe ser similar a dicho valor

var(p.est) # var(p) = p*(1-p)/n

0.1*0.9/100 # Varianza teorica

err.tipico = sqrt(0.1*0.9/100) # error tipico de p.muestral
hist(p.est, freq = FALSE)
curve(dnorm(x, 0.1, err.tipico), 0, 0.20, col = 2, add = TRUE)

#TCL a partir de 50 datos, mínimo.
hist(nro.defectos)
#Se parece a una dist.normal(T.C.L)
#Como la proporción muestral se puede describir como un promedio muestral, entonces si la muestra es
#grande, dicho promedio sigue una distr. Normal por el T.C.L


# Ejercicio 9.43 Welch

n1 = 12
n2 = 12
prom1 = 36500
prom2= 38100
s1 = 5000
s2 = 6100

nc = 0.95
alfa = 0.05

gl = (((s1^2)/n1)+((s2^2)/n2))^2/((((s1^2)/n1))^2/(n1 -1)+((s2^2)/n2)^2/(n2-1))
gl
gl = ceiling(gl)

T.alfa = qt(1 - alfa/2, gl)
T.alfa
err.est = T.alfa * sqrt((s1^2)/n1+(s2^2)/n2)
err.est
LIC = (prom1 - prom2) - err.est
LSC = (prom1 - prom2) + err.est
LIC; LSC

#####################################
#Ejercicio 9.48 varianza comun

n1 = 20
n2 = 20
prom1 = 32.91
prom2= 30.47
s1 = 1.57
s2 = 1.74

nc = 0.95
alfa = 1 - nc

gl = n2 + n1 - 2
gl

T.alfa = qt(1 - alfa/2, gl)
T.alfa

Sc = sqrt(((n1-1)*s1^2 + (n2-1)*s2^2)/gl)
err.est = T.alfa * sqrt(1/n1 + 1/n2) * Sc
err.est
LIC = (prom1 - prom2) - err.est
LSC = (prom1 - prom2) + err.est
LIC; LSC

#######################
#Medias variadas Ejercicio 9.47

pre =  c(29.8,27.9,22.1,44.5,22.2,43.8,21.7,21.9,28.3,11.8)
post =  c(43.3,25.4,24,88.3,18.1,131.2,34,32.1,6.2,20.3)

Di = post - pre
Di

# Apartitr de aqui es lo mismo que el IC para una media, para la media de Di
prom = mean(Di)
s = sd(Di)
prom
s
n = length(Di)
nc = 0.95
alfa = 1 - nc

T.alfa = qt(1 - alfa/2,n - 1)
T.alfa
err.est = T.alfa*s/sqrt(n)
err.est

LIC = prom - err.est
LSC = prom + err.est
LIC; LSC

t.test(Di, conf.level = 0.95)

t.test(post, pre, paired = TRUE, conf.level = 0.95)

############################
# Ejercicio 9.44

MarcaA = c(34.400, 45.500, 36.700, 32.000, 48.400, 32.800, 38.100, 30.100)*1000
MarcaB = c(36.700, 46.800, 37.700, 31.100, 47.800, 36.400, 38.900, 31.500)*1000

Di = MarcaB - MarcaA

# Apartitr de aqui es lo mismo que el IC para una media, para la media de Di
prom = mean(Di)
s = sd(Di)
prom
s
n = length(Di)
nc = 0.99
alfa = 1 - nc

T.alfa = qt(1 - alfa/2,n - 1)
T.alfa
err.est = T.alfa*s/sqrt(n)
err.est

LIC = prom - err.est
LSC = prom + err.est
LIC; LSC

t.test(Di, conf.level = 0.99)

t.test(MarcaB, MarcaA, paired = TRUE, conf.level = 0.99)

#################################
# IC PARA DIFERENCIA DE PROPORCINOES
# Ejercicio 9.66

n1 = 250
p1 = 80/250
n2 = 175
p2 = 40/175
nc = 0.90
alfa = 1 - nc

Z.alfa = qnorm(1 - alfa/2)
Z.alfa

err.est = Z.alfa * sqrt(p1*(1-p1)/n1 + p2*(1 - p2)/n2)
err.est

LIC = (p1 - p2) - err.est
LSC = (p1 - p2) + err.est
LIC; LSC

#######################################
# IC PARA COCIENTE DE VARIANZAS
# Parámetor C (cociente de varianzas)
# C = ((s1^2)/(s2^2))/((X1^2/(n1-1))/(X2^2)/(n2-1))
# a partir de la distribucion F se ovitene el estadistico pivote del cociente de varianza C
# Ejercicio 9.77
#VolksWagen, 12 camiones analizados, en Toyota, 10, tamaños muestrales, en las pruebas, 

n1 = 12
s1 = 1 #km por litro
n2 = 10 
s2 = 0.8 # km por litro

nc = 0.98
alfa = 1 - nc

?qf # calcular cuantiles de la distribucion F, que usa la chi cuadrado

F.inf = qf(alfa/2, n1 - 1, n2 - 1)
F.sup = qf(1 - alfa/2, n1 - 1, n2 - 1)
F.inf; F.sup
# Calcular los F, de 0.01 y de 0.99
# grados de libertad del numerador, como tomamos n1 de numerador, 12 - 1 = 11
# grados de libertad del denominador, 10 - 1 = 9
# Tabla: Zzzzzzzzzzzzzzz, PERO TENEMOS R

# Estadistico pivote= F = (s1^2/s2^2)/C
# Para encontrar C, reemplazar ambos valores de F y se encontraran los intervalos

# Csup = (s1^2/s2^2)/F_alfa_sobre_2
# Cinf =(s1^2/s2^2)/(1 - F_alfa_sobre_2)
# Realizar a Mano, zzzzz, en R:

LIC = (s1^2/s2^2)/F.sup
LSC = (s1^2/s2^2)/F.inf
LIC;LSC

##############################################
#ej teorico 3
media_X <- 2
var_X <- 3

media_Y <- 3
var_Y <- 5

# Esperanza y varianza de 3X + 2Y
media_Z <- 3 * media_X + 2 * media_Y
var_Z <- 3^2 * var_X + 2^2 * var_Y

media_Z
var_Z

# Z sigue distribución normal con:
# N(media_Z, var_Z)

set.seed(1)
mX = rnorm(50, mean = 2, sd = sqrt(3))
mY = rnorm(50, mean = 3, sd = sqrt(5))

m.v.a = 3 * mX + 2 * mY
m.v.a

mean(m.v.a)

var(m.v.a)

hist(m.v.a, freq = FALSE)
curve(dnorm(x,12,sqrt(47)), col = 2, add = TRUE)

mX2= replicate(1000, rnorm(50, mean = 2, sd = sqrt(3)))
mY2= replicate(1000, rnorm(50, mean = 3, sd = sqrt(5)))

promx = apply(mX2, 2, mean)
promy = apply(mY2, 2, mean)

estim = 3*promx + 2*promy
mean(estim)
var(estim)
sd(estim)
sqrt(47/50)
############################################
#4

# Esperanza y varianza de X - Y
set.seed(1)
mX4 = rnorm(50, mean = 2, sd = sqrt(3))
mY4 = rnorm(50, mean = 3, sd = sqrt(5))

m.v.a4= mX4 - mY4
m.v.a4

mean(m.v.a4)

var(m.v.a4)

mX4.2= replicate(1000, rnorm(50, mean = 2, sd = sqrt(3)))
mY4.2= replicate(1000, rnorm(50, mean = 3, sd = sqrt(5)))

promx = apply(mX4.2, 2, mean)
promy = apply(mY4.2, 2, mean)
estim = promx - promy
mean(estim)
var(estim)
sd(estim)
sqrt(8/50)