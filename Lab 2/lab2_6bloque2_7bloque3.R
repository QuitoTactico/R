# ESTEBAN VERGARA GIRALDO, 6 (b2), 7 (b3)

# Ejercicio 6 bloque 2

set.seed(1234)
suma_cinco_dados = function(n){
    d1 = sample(1:6, size=n, replace=T)
    d2 = sample(1:6, size=n, replace=T)
    d3 = sample(1:6, size=n, replace=T)
    d4 = sample(1:6, size=n, replace=T)
    d5 = sample(1:6, size=n, replace=T)
    return(d1+d2+d3+d4+d5)
}

# Metemos los resultados en este vector
medias = suma_cinco_dados(n=5000)/5

# Calculamos la media y la distribución estandar
media = mean(medias)
s = sd(medias)


histograma = function(presicion = 10){
    
    # Creamos un histograma con las medias, con la presicion
    #   deseada
    hist(medias, freq = F, breaks = presicion, main = 'Histograma de promedios al lanzar 5 dados')
    
    # Línea de distribución normal
    x = seq(0,7,0.01)
    lines(x, dnorm(x, mean = media, sd=s), col='green')
    
    #líneas verticales y leyenda
    abline(v = media, col = 'blue', lwd = 2)
    abline(v = media + s, col = 'red', lwd = 1.5)
    abline(v = media - s, col = 'red', lwd = 1.5)
    legend('topright',
           legend = c(
               'media = 3.5',
               'media ± sd',
               'distr normal'
           ),
           lwd = 2,
           col=c(
               'blue',
               'red',
               'green'
           )
    )
    legend('bottomright',
           legend = 'sd = 0.76',
    )
    
}

# Histograma con presición común (10 intervalos)
histograma()


# Histograma con el doble de presición (20 intervalos)
histograma(20)


#----------------------------------------------------------------------

# Ejercicio 7 bloque 3

# Generar 1000 lanzamientos de dardos
set.seed(1234)  
lanzamientos = runif(1000, min = 0, max = 1) 

# Frecuencia relativa de [0.2, 0.4]
frecuencia_central = sum(lanzamientos >= 0.2 & lanzamientos <= 0.4) / 1000

# Frecuencia relativa de [0.8, 1.0]
frecuencia_borde = sum(lanzamientos >= 0.8 & lanzamientos <= 1) / 1000

# Resultados
cat("Frecuencia relativa en [0.2, 0.4]:", frecuencia_central, "\n")
cat("Frecuencia relativa en [0.8, 1.0]:", frecuencia_borde, "\n")

hist(lanzamientos, freq = F, main = 'Histograma de lanzamiento de dardos', breaks = 20)

abline(v = 0.2, col='blue')
abline(v = 0.4, col='blue')
abline(v = 0.8, col='red')
abline(v = 1, col='red')
abline(h = 1, col='green')

legend('bottomright',
       legend = c(
           '[0.2, 0.4]',
           '[0.8, 1.0]',
           '1'
       ),
       lwd = 2,
       col=c(
           'blue',
           'red',
           'green'
       )
)



