



#-------------------------------------------------------------------
suma_cinco_dados = function(n){
    set.seed(1234)
    d1 = sample(1:6, size=n, replace=T)
    d2 = sample(1:6, size=n, replace=T)
    d3 = sample(1:6, size=n, replace=T)
    d4 = sample(1:6, size=n, replace=T)
    d5 = sample(1:6, size=n, replace=T)
    
    return(d1+d2+d3+d4+d5)
}

medias = suma_cinco_dados(n=5e3)/5

hist(medias)

(media = mean(medias))
(s = sd(medias))
(s2 = s**2)

abline(v = media, col = 'blue', lwd = 2)
abline(v = media + s, col = 'red', lwd = 1.5)
abline(v = media - s, col = 'red', lwd = 1.5)