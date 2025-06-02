#########################################
# PRACTICA VARIABLES ALEATORIAS CONTINUAS#
# EJERCICIO 3.33 
fx = function(x) {x^4*(1-x)^3}
fx(0) ; fx(0.5);fx(1)

?curve
curve(fx, 0, 1, main="func. densidad")

#literal a

fy = function(y) {280*y^4*(1-y)^3}
curve(fy,0,1,main="func.densidad")

?integrate

integrate(fy,0,1)
integrate(fy,0,1)$value

#literal b
P_50 = integrate(fy,0.5,1)$value
P_50
P_80 = integrate(fy,0.8,1)$value
P_80