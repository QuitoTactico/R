# func densidad de probabilidad
# x ~ u(a,b)
# 1/(b-a)   ;   a <= x <= b
# 0         ;   else

# a)
# dardos que pueden dar como resultado algo entre 0 y 1, (continuo)
N=1000
set.seed(0624)
lanzamientos = runif(N) # N = numero de simulaciones
hist(lanzamientos, freq=F)


# b)
fr_b = sum(lanzamientos >= 0.2 & lanzamientos <= 0.4) / N
cat('La frecuencia de lanzamientos en la zona [0.2, 0.4] es:', fr_b, ', un', fr_b*100, '%')

# b)
fr_c = sum(lanzamientos >= 0.8 & lanzamientos <= 1) / N
cat('La frecuencia de lanzamientos en la zona [0.8, 1] es:', fr_c, ', un', fr_c*100, '%')

#-----------------------------------------------------

# distribución normal
# si x ~ N(M, o^2) (media y varianza)
# su gráfica es una campana de gauss, cuyo centro es la media
# la mayoría de los datos están cercanos a la media
#   mientras más se alejan de ella son más inusuales.

# 8.
# a)    media es 30, desviacion estandar es 5
#       esa mamada acumulada hasta 25, pero 1-eso para ver los de por encima, x > 25          
1-pnorm(q = 25, mean = 30, sd = 5)

# b)    ver el percentil del 90%, en qué punto se cubre todo ese porcentaje?
qnorm(p = 0.9, mean = 30, sd = 5)

# c)    x entre 28 y 32
pnorm(q = 32, mean = 30, sd = 5) - pnorm(q = 28, mean = 30, sd = 5)

#----------------------------------------------------

# 9)

# creí que era así ekisdé
a = rnorm(n = 10000, mean = 30, sd = 5)
hist(a, freq=F, main = 'sd = 5')
b = rnorm(n = 10000, mean = 30, sd = 10)
hist(b, freq=F, main = 'sd = 10')
c = rnorm(n = 10000, mean = 30, sd = 30)
hist(c, freq=F, main = 'sd = 30')

# ahora sis
x = seq(-10,10,0.01)
plot(x, dnorm(x), type= 'l')
lines(x, dnorm(x, sd=1.5), col='green')
lines(x, dnorm(x, sd=2), col='yellow')
lines(x, dnorm(x, sd=3), col='red')
legend('topright',
        legend = c(
           'sd = 1',
           'sd = 1.5',
           'sd = 2',
           'sd = 3'),
        lwd = 2,
        col=c(
               'black',
               'green',
               'yellow',
               'red'
           )
       )


#-----------------------------------------------------

# distribución exponencial
# x ~ EXP(lambda)
# lambda puede ser tasa de falla

x = seq(0,100,0.01)
plot(x, dexp(x, 0.05), type = 'l')

#probabilidad de que el bombillo dure más de 15 años
1-pexp(15,0.05)
