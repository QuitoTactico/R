library(readr)
ventacasas <- read_csv("C:/Users/clase/Downloads/ventacasas.csv")

area = ventacasas$Area


muestras_area = function(n){
    set.seed(1234)
    muestra = sample(area,n)
    media = mean(area)
    media_muestra = mean(muestra)
    error = abs(media_muestra - media)/media  *100 #error del 13%
    
    hist(muestra,
         main="",
         xlab="Muestra tamaño n=10")
    
    abline(v= media, col='green')
    abline(v= media_muestra, col='red')
    return(c(media, media_muestra, error))
}

(muestras_area(30))

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