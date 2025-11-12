###########################
# PRACTICA DE ESTADISTICA DESCRIPTIVA

#########################
# CARGA DE DATOS
library(ISLR)
Auto
datos = Auto

#########################
# ANALISIS EXPLORATORIO DE DATOS NUMERICOS

millas = datos$mpg # datos numericos: n = 392 observaciones
min(millas) # minimo
max(millas) # maximo
n = length(millas) # n = tama?o muestral

# GRAFICOS DESCRIPTIVOS
plot(millas)

# diagrama de puntos
?stripchart
stripchart(millas, method = "stack", ylim = c(0, 50))

# Histograma
?hist
hist(millas)
histo = hist(millas, breaks = 4, main = "Histograma de mpg", col = "green",
     ylab = "Frecuencias")
histo$breaks #limites de clase
histo$counts #frecuencias absolutas
histo$mids #marcas de clase

sum(histo$counts)
histo$counts / n # frecuencias relativas

frec.acum = cumsum(histo$counts) #frecuencias acumuladas

# Ojiva de frecuencias
plot(histo$mids, frec.acum, type = "l")

# MEDIDAS DESCRIPTIVAS

# medidas de localizacion: Media o promedio, Mediana, Moda

ejm = c(21, 21, 21, 22, 20, 19, 23) # edades de estudiantes

# Media aritmetica o promedio
(21+21+21+22+20+19+23)/7
sum(ejm)/7
mean(ejm)
stripchart(ejm, method = "stack")

# mediana
sort(ejm)
median(ejm)

ejm2 = c(21, 21, 21, 22, 20, 19, 23, 50)
stripchart(ejm2, method = "stack")
mean(ejm2)
median(ejm2)
sort(ejm2)

# desviaciones
ejm
ejm - 21

# Con los datos de millas por galon 
mean(millas)
median(millas)
sum(millas - mean(millas))

stripchart(millas, method = "stack", ylim = c(0, 50))
abline(v = mean(millas), col = 2)
abline(v = median(millas), col = 3)


# MEDIDAS DE DISPERSION: Rango, Varianza muestral, desviacion tipica
max = max(millas)
min = min(millas)
rango = max - min
rango 

# varianza muestral
stripchart(ejm, method = "stack")
sum((ejm - 21)^2)/ (7-1)
var(ejm) # 1.66 a?os cuadrados ???
var(millas)
# desviacion t?pica o desviacion estandar
sqrt(var(ejm))
sd(ejm)
sd(millas)
# MEDIDAS DE POSICION
?quantile
quantile(millas, probs = 0.05) #P5
quantile(millas, probs = 0.50) #P50 = mediana = segundo cuartil
quantile(millas, probs = 0.25) #P25 = Q1 =primer cuartil
quantile(millas, probs = 0.75) #P75 = Q3 = tercer cuartil

# DIAGRAMA DE CAJA O BOXPLOT
boxplot(millas)
boxplot(datos$acceleration)
box = boxplot(datos$acceleration)
box$out # identificar outliers

# PAQUETE PSYCH
library(psych)
?describe
describe(millas)
#ojo asimetria y kurtosis (campana de gauss)
#skew ca>0 curva va a la izquierda <0 va a la derecha =0 se mantiene en el centro
#kurtosis >0 la curva se alarga <0 se aplana =0 se mantiene
hist(millas)
describe(datos)

#########################
# ANALISIS EXPLORATORIO DE DATOS CATEGORICOS
?Auto
origen = datos$origin
summary(origen) # se considera a origen como numerica (ERROR)

origen = as.factor(origen) # var categorica como factor
summary(origen) # estad descriptiva de var. categorica

conteos = table(origen) #permite hacer conteos de c/nivel
addmargins(conteos) # frecuencias absolutas

proporciones = prop.table(conteos)
addmargins(proporciones) # frecuencias relativas

barplot(conteos) #graficos de barras

pie(conteos) # grafico circular o de sectores

pie(table(datos$name)) # problemas cuando hay muchas categorias

#########################
# ANALISIS EXPLORATORIO CONJUNTO: DOS VARIABLES NUMERICAS
attach(datos) # carga las variables del d.f. en memoria

plot(mpg, horsepower) # diagrama de dispersion
cor(mpg, horsepower) # correlacion lineal de pearson
#cuando es negativa es inversamente proportional, positiva es directamente proportional,=0 incorreladas
#no confundir correlacion con causalidad
#correlacion=asosiacion(comportamiento cinjunto)
#causalidad= causa-efecto(explicativo)
# signo negativo = inversamente proporcionales

#########################
# ANALISIS EXPLORATORIO CONJUNTO: DOS VARIABLES CATEGORICAS
# consideremos a year = var categorica (modelo del auto)
tabla = table(year, origen) # tabla de contingencia
tabla

addmargins(prop.table(tabla)) # tabla de probabilidad 

barplot(tabla) # grafico de barras apiladas

tabla2 = table(origen,year) # tabla de contingencia
tabla2

barplot(tabla2) # grafico de barras apiladas

#########################
# ANALISIS EXPLORATORIO CONJUNTO: VAR NUMERICA Y VAR CATEGORICA

boxplot(mpg ~ origen) # diagrama de caja por subgrupo

describeBy(mpg, origen) # analisis descriptivo por subgrupo