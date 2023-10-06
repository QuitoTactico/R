#PROBLEMA 3
#Ley de los grandes números

set.seed(1234)

n = c(10,50,100,500,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000 + 3:20 *5000)



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

meanb = unlist(lapply(n, mean_binom))
meanp = unlist(lapply(n, mean_pois))
meann = unlist(lapply(n, mean_norm))

lgn_dat = data.frame(n = rep(n, 3), 
                     med = c(meanb,meanp,meann),
                     dist = rep(c('Binomial', 'Poisson', 'Normal'),
                                each = length(n)
                                )
                     )

#install.packages('ggplot2')
library(ggplot2)
ggplot(lgn_dat)+
    geom_line(aes(x = n, y = med, color = dist))+
    labs(x = 'n', y = 'Media')+
    theme(text = element_text(size = 14))+
    theme_grey(base_size = 16)
          
          

# BLOQUE 2
# problema 4

# intervalo de confianza usando N se usa cuando los datos distribuyen de cualquier forma. Es para una muestra, quieres aproximarte con un rango al promedio real (poblacional). No conoces sd pero tienes el sd de la muestra. O puedes tener el sd real, idk.

set.seed(10)
n= 500000
pescaos = rpois(n, lambda = 20)
media = mean(pescaos)

muestra = sample(pescaos, size = 1000)
media_muestral = mean(muestra)
sd_muestral = sd(muestra)

# tu media muestral tiene un error del 5%
(error = (abs(media_muestral - media)/ media)*100)

#---------------------------------------------------------

#Intervalo de confianza 95%
alpha = 0.05 # Nivel de confianza 95%
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


# problema 5
# intervalo de confianza usando T se usa cuando estás seguro de que los datos distribuyen normal. No conoces sd pero tienes el sd de la muestra.

data('mtcars')
View(mtcars)

mpg = mtcars$mpg

hist(mpg)
boxplot(mpg)

mpg_mean    = mean(mpg)
mpg_n       = length(mpg)
mpg_sd      = sd(mpg)

gl = mpg_n -1 #(idk why n-1 y no usar n)

alpha = 0.05
t_1alpha2 = qt( p = 1-alpha/2, df =  mpg_n-1)  #<- this is gl

LI = mpg_mean - (t_1alpha2 * mpg_sd / sqrt(mpg_n))
LD = mpg_mean + (t_1alpha2 * mpg_sd / sqrt(mpg_n))


cat('Un intervalo de confianza al ', 100*(1-alpha),
    '% para mu (media) es [', LI, ' , ', LD , ']')
