#a)

Tirar = function(n, p){
  #p = probabilidad de sello, la de cara será lo restante, 1-p
  set.seed(3456)
  sample(c('Cara', 'Sello'), size = n, replace = T, prob = c(1-p , p))
}

veces = 10  
tiro = Tirar(n=veces, p=0.5)
(tabla = table(tiro))
redondeado = round(tabla/veces * 100, 2)

barplot(redondeado)


Dados = function(n){
  set.seed(sample(1:9999, replace = T))
  sample(c(1:6), size = n, replace = T)
}

# prob = 1/6 = 0.1666...

vecesdado = 1e5
round( table(Dados(vecesdado))/vecesdado * 100 , 2)

undado=function(n){
  return(sum(Dados(2)))
}

dosdados = function(n){
  set.seed(sample(1:9999, replace = T))
  dados1 = Dados(n)
  dados2 = Dados(n)
  return(dados1+dados2)
}

vecesdados=1e6
(a = round(table(dosdados(vecesdados))/vecesdados * 100 ,2))
barplot(a)

' 7 tiene la diagonal más grande
x | 1 2 3 4 5 6
--|-------------
1 | 2 3 4 5 6 7
2 | 3 4 5 6 7 8
3 | 4 5 6 7 8 9
4 | 5 6 7 8 9 10
5 | 6 7 8 9 1011
6 | 7 8 9 101112
'


'
x     1   2   3   4   5   6
p(x)  2p  p   2p  p   2p  p

'
#dado pa impares, los impares tienen el doble de probabilidad de salir
hijueputa_dado_cargado = function(n){
  set.seed(sample(1:9999 , replace = T))
  #sample(1:6 , size = n, replace = T, prob = c(0.222,0.111,0.222,0.111,0.222,0.111))
  sample(1:6 , size = n, replace = T, prob = c(2,1,2,1,2,1))
}

b = (round(table(hijueputa_dado_cargado(1e5))/1e5 *100 , 2))
barplot(b)
