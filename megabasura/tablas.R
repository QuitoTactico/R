#hacer bloque 0,1,2

library(readxl)

Info_clientes_banco <- read_excel('Info_clientes_banco.xlsx')

asesoresFA = table( Info_clientes_banco$Asesor)

print(asesoresFA)
barplot(asesoresFA)

#----------------------------------------------------------------------

asesoresFR = asesoresFA / length(Info_clientes_banco$Asesor) * 100

print(asesoresFR)
barplot(sort(asesoresFR))

#----------------------------------------------------------------------


(asesoresCC = table(Info_clientes_banco[Info_clientes_banco$Cuenta_corriente==1,]$Asesor))
barplot(sort(asesoresCC))

(asesoresCA = table(Info_clientes_banco[Info_clientes_banco$Cuenta_ahorros==1,]$Asesor))
barplot(sort(asesoresCA))

(asesoresCV = table(Info_clientes_banco[Info_clientes_banco$Credito_vivienda==1,]$Asesor))
barplot(sort(asesoresCV))

(saldo_cc = sum(Info_clientes_banco[Info_clientes_banco$Cuenta_corriente==1,]$Saldo_cuenta_corriente))
(saldo_ca = sum(Info_clientes_banco[Info_clientes_banco$Cuenta_ahorros==1,]$Saldo_cuenta_ahorros))

saldo_total = saldo_cc + saldo_ca

porc_cc = (saldo_cc/saldo_total) * 100
porc_ca = (saldo_ca/saldo_total) * 100

#---------------------------------------------------------------------

barplot(table(Info_clientes_banco$Segmento))
barplot(table(Info_clientes_banco$Tipo_Cliente))


barplot(c(saldo_ca,saldo_cc))

#Tipos de variable:
#NUMÉRICA:    CONTINUA | DISCRETA
#CUALITATIVA:  NOMINAL | ORDINAL

#----------------------------------------------------
asesores_autos = Info_clientes_banco$Vehículo_1 + Info_clientes_banco$Vehículo_2
valores_autos = sum(Info_clientes_banco[Info_clientes_banco$Vehículo_1 == 1,]$Valor_vehículo_1,
                    Info_clientes_banco[Info_clientes_banco$Vehículo_2 == 1,]$Valor_vehículo_2)

clientes_con_cuenta = Info_clientes_banco[Info_clientes_banco$Cuenta_ahorros==1|Info_clientes_banco$Cuenta_corriente==1,]$ID_cliente

Info_clientes_banco$Valor_inmuebles_total = Info_clientes_banco$Valor_inmuble_1 + Info_clientes_banco$Valor_inmuble_2
clientes_mucha_plata = Info_clientes_banco[Info_clientes_banco$Inmueble_2==1 & Info_clientes_banco$Valor_inmuebles_total > 600000000 & Info_clientes_banco$Credito_vivienda==1,]$ID_cliente




# mean, median, quantile, sd

for(suc in c('AMERICAS','CENTRO','PRADO','VILLA')){
  cat( 'El promedio de la sucursal', suc, 'es:', mean(Info_clientes_banco[(Info_clientes_banco$Cuenta_corriente==1 & Info_clientes_banco$Sucursal==suc),]$Saldo_cuenta_corriente), '\n')
}

icb = Info_clientes_banco


#Rango intercuartil = diferencia entre Q_3 y Q_1
#Datos atípicos= Datos que se tienen una distancia mayor hasta 
#  su cuartil más cercano, que 1.5 veces el rango intercuartil
# AltGr +

boxplot(icb[icb$Cuenta_corriente==1,]$Saldo_cuenta_corriente ~ icb[icb$Cuenta_corriente==1,]$Asesor,
        main='Promedio de plata CC vs Asesor', xlab='Asesor', ylab='SaldCC', cex.axis = 0.7)
