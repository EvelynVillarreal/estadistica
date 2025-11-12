##############################################
# CONTRASTES NO PARAMETRICOS
##############################################


##############################################
# CONTRASTES DE LOCALIZACION

# Test de signos (Mediana en muestras independientes
library(PASWR)

# Se tiene una muestra del índice de efectividad de un pesticida
?SIGN.test
efectividad <- c(0.41,0.68,0.52,0.82,0.45,0.78,0.96,0.91,0.75)

# Probar si la mediana del indice de efectividad es igual al 0.9?
SIGN.test(efectividad, md=0.9,alternative = "two.sided",conf.level = 0.95)
# HO no se rechaza (mediana si es igual a 0.9)

# Test de Rangos con signos de Wilcoxon
# Se tiene una muestra de las notas de un examen de ingreso
examen <- c(80,78,78,77,76,76,88,89,89,90,95,65,60,60,56,56,50,45)

#Probar si la nota mediana del examen es mayor a 80?
wilcox.test(examen, mu= 80, alternative = "greater", conf.level = 0.95)
# H0 no se rechaza (mediana es menor o igual a 80)

?wilcox.test()
wilcox.test(examen, mu= 80, alternative = "two.sided", conf.level = 0.95)

# Test para muestras independientes
# Probar si las notas del examen son iguales entre hombre (0) y mujer (1)
genero <- c(0,1,0,1,1,1,0,1,1,0,1,1,1,1,0,0,1,1) # muestras independientes

wilcox.test(examen, genero, alternative = "two.sided", conf.level = 0.95)
# H0 se rechaza (las medianas de las notas son distintas entre hombre y mujeres)

# Test para muestras pareadas
# suponga que se aplican dos examenes al mismo grupo, antes (0) y despues (1) 
# de una clase de refuerzo. Se desea comparar las medianas de las notas pareadas

pares = c(0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1)
wilcox.test(examen, pares, paired = TRUE, alternative = "two.sided", conf.level = 0.95)
# Las medianas de las notas son distintas entre antes y despues de la clase de refuerzo

# Contraste de Kruskall Wallis (alternativa al ANOVA)
# SE tiene una muestra de puntaje de encuesta de estado de salud autovalorado 
# para distintos grupos etnicos

indigenas <- c(78,79,89,79,81,83,95)  
mestizo <- c( 67, 78, 78, 79,80)          
afros <- c(95,99,89,87,90,92)        
otros <- c(78,80,81,78,67,70,71,73)
kruskal.test(list(indigenas,mestizo,afros,otros))
# el puntaje mediano del estado de salud autovalorado es distinto entre los grupo etnicos

library(asbio)
?pairwise.wilcox.test
pairwise.wilcox.test(c(indigenas,mestizo,afros,otros),
                     c(rep(1,7),rep(2,5),rep(3,6),rep(4,8)))

##############################################
# CONTRASTES DE BONDAD DE AJUSTE
# Se mide el espacio (en GB) que ocupan varios usuarios de una misma
# estacion de trabajo

espacio <- c(25, 30, 31, 33, 35, 35, 40, 40, 42, 42, 45, 45, 46, 46, 47, 
           47, 49, 50, 50, 55)

#estadisticos descriptivos
media <- mean(espacio)
desves <- sd(espacio)
n <- length(espacio)
n

# test de Normalidad
#Test de Kolmogorov Smirnoff
ks.test(espacio,pnorm,mean=40,sd=3) # especificando parametros

#Test de Kolmogorov Smirnoff Lilliefors
library(nortest)
lillie.test(espacio)

#Test de shapiro wilks
shapiro.test(espacio)


##############################################
# CONTRASTES PARA DATOS CATEGORICOS

# Verifique si el genero esta relacionado con la tendencia política
Tabla <- matrix(c(68,56,32,52,72,20), 2, 3, byrow=TRUE)
colnames(Tabla)<-c("Izquierda","Derecha","Centro")
rownames(Tabla)<-c("Mujer","Hombre")
Tabla

barplot(Tabla,beside=TRUE)

#Hacemos la prueba usando el comando chisq.test
Test<-chisq.test(Tabla,correct=FALSE)
Test
Test$expected

# TEST CHI CUADRADO DE HOMOGENEIDAD
# Pruebe si la tasa de accidentes es el mismo en los 4 turnos de trabajo
chisq.test(c(47,52,57,63), p=c(1/4,1/4,1/4,1/4))
