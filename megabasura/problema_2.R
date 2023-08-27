(vector_1 = matrix(122:137))

(matriz_1 = matrix(122:137, nrow= 4, ncol=4))

(matriz_2 = matrix(122:137, nrow= 4, ncol=4, byrow=TRUE))

(matriz_3 = matrix(vector_1, nrow=4, ncol=4))

(z = 0:10)

(vector_2 = matrix(z**3 ))

plot(z, main='basura', xlab='indice', ylab='z')

plot(vector_2, type='b', main='X^3', xlab='n', ylab='n^3')

matriz_1
matriz_1[3,2] = 135; (matriz_1)


plot(matriz_1[,2], type='b'); (matriz_1[,2])