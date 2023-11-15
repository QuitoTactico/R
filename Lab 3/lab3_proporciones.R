# Problema 9

# n = ((Z_1-alpha/2)^2 * P(1-P)) / E^2

Z = qnorm(0.975)   # cuartil a tal que p(Z < a) = 1-alpha/2
E = 0.06    # error | tolerancia
P = 0.8  # P gorro.  Si no lo dan, toca usar 0.5
# P normal es proporción poblacional, P gorro es proporción muestral

(Z**2 * P* (1-P)) / (E**2)   #Casi 171

#-----------------------------
# Problema 10

# Pruebas de hipótesis sobre proporciones

# Al final la Z de rechazo era 1.645 en adelante
# El Z_c estadístico dió 1.49, así que no está en la de rechazo, fueron sinceros


p0 = 0.06
n = 200
x = 17
pgor = x/n
zc = (pgor-p0) / sqrt(   (p0 * (1-p0)) /n  )

alpha = 0.05
z_alpha = qnorm(1-alpha)
    