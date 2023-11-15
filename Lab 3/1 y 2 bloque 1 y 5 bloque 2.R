# PROBLEMAS 1 Y 2 BLOQUE 1

library(readr)
ventacasas <- read.csv("C:/Users/Esteban/Downloads/-CODE-/R/Lab 3/ventacasas.csv")


area = ventacasas$Area

media_pob = mean(area)
sd_pob    = sd(area)
hist(area)
abline(v= media_pob, col='red', lwd = 2)


muestras_area = function(n){
    set.seed(1234)
    muestra = sample(area,n)
    media_muestra = mean(muestra)
    error = abs(media_muestra - media_pob)/media_pob  *100 #error del 13%
    
    hist(muestra,
         main=n,
         xlab="")
    
    abline(v= media_pob, col='green')
    abline(v= media_muestra, col='red')
    legend('topright',
           legend = c("media poblacional", "media muestral"),
           lwd = 2,
           col=c('green', 'red')
    )
    
    
    cat('La media de la muestra n =',n, 'dió una media muestral de',
        media_muestra,', esta presenta un error del' ,error,'%')
}

muestras_area(30)

# promedio de promedios, muestra de muestras

promedios10   = rep(0,5000) #vector de 5000 ceros
promedios50   = rep(0,5000) 
promedios100  = rep(0,5000) 

set.seed(1234)
for(i in 1:5000){
    muest10 = sample(area, 10)
    muest50 = sample(area, 50)
    muest100 = sample(area, 100)
    promedios10[i] = mean(muest10)
    promedios50[i] = mean(muest50)
    promedios100[i] = mean(muest100)
}

par(mfrow = c(3,1)) #representa las graficas en 3 filas y una columna
xlimits = range(promedios10)

hist(promedios10, breaks = 25, xlim = xlimits, xlab="n=10")
hist(promedios50, breaks = 25, xlim = xlimits, xlab="n=50")
hist(promedios100, breaks = 25, xlim = xlimits,xlab="n=100")
par(mfrow = c(1,1))


#-------------------------------


# mamadas acumuladas, pero ahora con precio (sabes k, lo 
# haré con area para ahorrar tiempo)

media = mean(area)
standes = sd(area)

m = 50000
n = 100
prom = rep(0,m)  # vector para promedio muestral de cada simulacion

set.seed(1234)

for(i in 1:m){
    prom[i] = mean(rnorm(n,media,standes))
}
par(mfrow = c(1,1))
hist(prom, freq=F, xlab=expression(bar(X) ))

plot(ecdf(prom), verticals=T, do.points= F, col.01line = NULL)
abline(v= media, col='purple')