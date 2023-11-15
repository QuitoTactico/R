#PROBLEMA 3 BLOQUE 1
#Ley de los grandes números

set.seed(1234)

n = c(10, 50, 100, 200, 500, 1000, 2000, 3000, 4000, 5000,
      6000, 7000, 8000, 9000, 10000)

mean_binom = function(n){
    x = rbinom(n, size =  1, prob = 0.5)
    mean(x)
}
mean_pois = function(n){
    x = rpois(n, lambda = 0.5)
    mean(x)
}
mean_norm = function(n){
    x = rnorm(n, mean = 0.5)
    mean(x)   
}

# Sacamos listas con los resultados de las medias con esas n
meanb = unlist(lapply(n, mean_binom))
meanp = unlist(lapply(n, mean_pois))
meann = unlist(lapply(n, mean_norm))

lgn_dat = data.frame(n = rep(n, 3), 
                     med = c(meanb,meanp,meann),
                     dist = rep(c('Binomial', 'Poisson', 'Normal'),
                                each = length(n)
                     )
)

# Graficamos
library(ggplot2)
ggplot(lgn_dat)+
    geom_line(aes(x = n, y = med, color = dist)) +
    labs(x = 'n', y = 'Media') +
    theme(text = element_text(size = 14)) +
    theme_grey(base_size = 16)


n = c(1:10 * 10 , 1:100 * 100)

#--------------------------------------------------------------------------
# problema 4 BLOQUE 2

# intervalo de confianza usando N se usa cuando los datos distribuyen de cualquier forma. Es para una muestra, quieres aproximarte con un rango al promedio real (poblacional). No conoces sd pero tienes el sd de la muestra. O puedes tener el sd real, idk.

# población y media poblacional
set.seed(10)
n = 500000
poblacion = rpois(n, lambda = 20)
media = mean(poblacion)

# sacamos la muestra de 1000
muestra = sample(poblacion, size = 1000)
media_muestral = mean(muestra)
sd_muestral = sd(muestra)

# tu media muestral tiene un error del 5%
(error = (abs(media_muestral - media)/ media)*100)


#Intervalo de confianza 95%
alpha = 0.05 # Nivel de confianza 95% -> alpha de 0.05
# alpha = error

# z tal que 1-(alpha/2) 
# dividió el error entre los dos lados
z_1alpha2 = qnorm(1 - alpha / 2)   # z_1 - alpha/2

# mu ± Z_ 1-(alp/2)
# promedio masmenos el percentil en el que se cubre la confianza/2
LI = media_muestral - (z_1alpha2 * sd_muestral / sqrt(n))
LD = media_muestral + (z_1alpha2 * sd_muestral / sqrt(n))

cat('Un intervalo de confianza aproximado al ', 100*(1-alpha),
    '% para mu (media) es [', LI, ' , ', LD , ']')

# la media real (poblacional), sí estuvo adentro de ese intervalo


#Intervalo de confianza 95%
alpha = 0.01 # Nivel de confianza 95% -> alpha de 0.05
# alpha = error

# z tal que 1-(alpha/2) 
# dividió el error entre los dos lados
z_1alpha2 = qnorm(1 - alpha / 2)   # z_1 - alpha/2


# ahora lo mismo con una confianza de 99%
LI = media_muestral - (z_1alpha2 * sd_muestral / sqrt(n))
LD = media_muestral + (z_1alpha2 * sd_muestral / sqrt(n))

cat('Un intervalo de confianza aproximado al ', 100*(1-alpha),
    '% para mu (media) es [', LI, ' , ', LD , ']')

