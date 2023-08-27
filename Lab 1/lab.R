# 9,11,12,13 - bloque 1

library(readxl)

icb = read_excel('info_clientes_banco.xlsx')

#9
lista_saldos_cc = icb[icb$Cuenta_corriente==1,]$Saldo_cuenta_corriente

#10  (igual tocó hacerlo)
saldo_cc = sum(lista_saldos_cc)

#11

lista_saldos_ca = icb[icb$Cuenta_ahorros==1,]$Saldo_cuenta_ahorros
saldo_ca = sum(lista_saldos_ca)

saldo_total = saldo_cc+saldo_ca

porcentaje_cc = (saldo_cc/saldo_total) * 100
porcentaje_ca = (saldo_ca/saldo_total) * 100

#12

tabla_segmentos_FA = table(icb$Segmento)
tabla_segmentos_FR = (tabla_segmentos_FA/length(icb$Segmento)) * 100

(tabla_tipo_FA = table(icb$Tipo_Cliente))
tabla_tipo_FR = (tabla_tipo_FA/length(icb$Tipo_Cliente)) * 100

#13

barplot(tabla_segmentos_FA, main = 'Segmentos FA', xlab = 'Segmento', ylab = '# de clientes')
barplot(tabla_segmentos_FR, main = 'Segmentos FR', xlab = 'Segmento', ylab = '% de clientes')
barplot(tabla_tipo_FA, main = 'Tipo FA', xlab = 'Tipo de cliente', ylab = '# de clientes')
barplot(tabla_tipo_FR, main = 'Tipo FR', xlab = 'Tipo de cliente', ylab = '% de clientes')
