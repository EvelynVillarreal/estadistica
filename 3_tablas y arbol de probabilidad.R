# EJERCICIOS DE PROBABILIDADES

# EJER. 2.80
datos = matrix(c(21,48,36,26,30,19), nrow = 2)
rownames(datos) = c("H", "NH")
colnames(datos) = c("NF", "FM", "FE")
datos

tabla = as.table(datos)
tabla

addmargins(tabla) # tabla de contingencia

addmargins(prop.table(tabla)) # tabla de probabilidades

# literal a. P(H | FE)
0.1666667 /  0.2722222 
addmargins(prop.table(tabla, 2)) # condicional de columna

# literal b. P(NF | NH)
0.2666667 / 0.5166667
addmargins(prop.table(tabla, 1)) # condicional de fila

###############################
# arboles de probabilidad
# Una empresa de ingeniería analiza el siguiente sistema:
# El 10% de los sensores instalados están defectuosos.
# Un sensor defectuoso emite una alarma en el 95% de los casos.
# Un sensor no defectuoso también puede emitir una falsa alarma, en el 2% de los casos.

# Probabilidades
P_def <- 0.10
P_alarma_dado_def <- 0.95
P_alarma_dado_buen <- 0.02
P_buen <- 0.90

# Probabilidad total de alarma
P_alarma <- P_alarma_dado_def * P_def + P_alarma_dado_buen * P_buen

# Bayes
P_def_dado_alarma <- (P_alarma_dado_def * P_def) / P_alarma
P_def_dado_alarma

#ARBOL DE PROBABILIDADES
install.packages("DiagrammeR")  # Solo la primera vez
library(DiagrammeR)

# Carga la librería
library(DiagrammeR)

grViz("
digraph arbol_probabilidad {
  graph [rankdir=LR]

  # Nodo inicial
  Inicio [label='Inicio', shape=circle]

  # Primera división: Defectuoso o Bueno
  Inicio -> Defectuoso [label='0.10']
  Inicio -> Bueno [label='0.90']

  # Segunda división desde Defectuoso
  Defectuoso -> AlarmaD [label='0.95']
  Defectuoso -> NoAlarmaD [label='0.05']

  # Segunda división desde Bueno
  Bueno -> AlarmaB [label='0.02']
  Bueno -> NoAlarmaB [label='0.98']

  # Etiquetas de nodos finales
  AlarmaD     [label='Alarma\n(Defectuoso)\nP=0.095', shape=box]
  NoAlarmaD   [label='No Alarma\n(Defectuoso)\nP=0.005', shape=box]
  AlarmaB     [label='Alarma\n(Bueno)\nP=0.018', shape=box]
  NoAlarmaB   [label='No Alarma\n(Bueno)\nP=0.882', shape=box]
}
")

# Probabilidades
P_John <- 0.20
P_Tom <- 0.60
P_Jeff <- 0.15
P_Pat <- 0.05
P_falla_dado_John <- 0.005
P_nofalla_dado_John <- 0.995
P_falla_dado_Tom <- 0.01
P_nofalla_dado_Tom <- 0.99
P_falla_dado_Jeff <- 1/90
P_nofalla_dado_Jeff <- 89/90
P_falla_dado_Pat <- 0.005
P_nofalla_dado_Pat <- 0.995

# Probabilidad total de falla
P_falla <- P_falla_dado_John * P_John + P_falla_dado_Tom * P_Tom + P_falla_dado_Jeff * P_Jeff + P_falla_dado_Pat * P_Pat

# Bayes
RESULTADO_P_John_dado_falla <- (P_falla_dado_John * P_John) / P_falla

#ARBOL DE PROBABILIDADES
library(DiagrammeR)
grViz("
      digraph arbol_probabilidad{
      graph [rankdir=LR]
      
      #nodo inicial
      Inicio [label='inicio',shape=circle]
      
      #primer division
      Inicio ->John[label='0.20']
      Inicio ->Tom[label='0.60']
      Inicio ->Jeff[label='0.15']
      Inicio ->Pat[label='0.05']
      
      #segunda division desde john
      John ->fallaA[label='0.005']
      John ->nofallaA[label='0.995']
      
      #segunda division desde tom
      Tom ->fallaB[label='0.01']
      Tom ->nofallaB[label='0.99']
      
      #segunda division desde Jeff
      Jeff ->fallaC[label='0.01111']
      Jeff ->nofallaC[label='0.98889']
      
      #segunda division desde pat
      Pat ->fallaD[label='0.005']
      Pat ->nofallaD[label='0.995']
      
      # Etiquetas de nodos finales
      fallaA     [label='Falla\n(John)\nP=0.001', shape=box]
      nofallaA   [label='No Falla\n(John)\nP=0.199', shape=box]
      fallaB     [label='Falla\n(Tom)\nP=0.006', shape=box]
      nofallaB   [label='No Falla\n(Tom)\nP=0.594', shape=box]
      fallaC     [label='Falla\n(Jeff)\nP=0.0017', shape=box]
      nofallaC   [label='No Falla\n(Jeff)\nP=0.1483', shape=box]
      fallaD     [label='Falla\n(Pat)\nP=0.00025', shape=box]
      nofallaD   [label='No Falla\n(Pat)\nP=0.04975', shape=box]
      }
      ")
