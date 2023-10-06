# BLOQUE 3

#problema 6


alpha = 0.01
z_1_alph2 = qnorm(1 - alpha/2) # Z_(1-(alpha/2))
sigma = 1.40 #sd
tol   = 0.25 #E, tolerancia, error máximo

(2.57*1.4/0.25)**2

# n, tamaño de muestra necesario para que con una confianza del 100(1-0-01)%. 
# garantice que el error máximo que se comete al estimar el promedio, sea de 0.25 (tolerancia)
(z_1_alph2*sigma/tol)**2


#----------------------------------------------------------

#problema 7

# la confianza común es del 95%.   (1-alpha) = 0-95.  alpha = 0.05

#pruebas de hipótesis

# H0 dice que la media real es igual a la muestral
# H1 la hipótesis alternativa dice que la real es menor, mayor o diferente a la media muestral que nos dan

# rechazar o no rechazar la hipótesis nula (la hipótesis nula es decir que son iguales)

# Hay regiones para saber cuál elegir

xbar  = 9900  #media muestral
mu0   = 10000 #media poblacional
sigp  = 120   #sd
n     = 30    #TAMAÑO DE MUESTRA

(zc = (xbar-mu0) /(sigp / sqrt(n) ))

alpha = 0.05
z_alpha = qnorm(1-alpha)
-z_alpha


#nuestro zc (estadístico de prueba) dió aún menor que nuestro Z conde comenzaba ese 95% de prob (Z_1-alpha)
# rechazamos h0 (la media que nos dieron no es la real)
# concluimos que mu es menor a 10000, el fabricante es un mentiroso


#----------------------------------------------------------

#problema 8

# prueba de hipotesis asumiendo normalidad pero no sabemos la sd
# se hace con distribución T

xbar = 2.1
mu0 = 2
s = 0.3
n = 35

# t crítico
(tc = (xbar - mu0) / (s / sqrt(n)))

alpha = 0.05
(t_alpha = qt(1-alpha, n-1))


#El t crítico quedó afuera del 1.69, se salió, es aún mayor, así que rechazo h0 y digo que mu es mayor
