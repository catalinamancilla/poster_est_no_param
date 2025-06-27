#PRUEBA WILCOXON

# Cargar la base de datos
data(faithful)

# Crear la muestra con los tiempos de erupción
Muestra=(faithful$eruptions)

# Calcular tamaño muestral
n=length(Muestra)
print(paste("Tamaño muestral original:", n))

# Crear tabla con los datos ordenados
tabla= data.frame(Muestra=sort(muestra))

# Definir nivel de significancia
alpha=0.05
print(paste("Nivel de significancia (alpha):", alpha))

# Calcular la mediana muestral
mediana=median(Muestra)
print(paste("Mediana muestral:", mediana))

# Agregar valor de la mediana a la tabla
tabla$Mediana=mediana

# Calcular diferencia Xi - Mn
tabla$'Xi-Mn'= tabla$Muestra - mediana

# Eliminar observaciones con diferencia igual a cero
tabla <- tabla[tabla$`Xi-Mn` != 0, ]

# Calcular nuevo tamaño de n
n_new=length(tabla$Muestra)
print(paste("Nuevo tamaño muestral (cuando Xi-Mn es distinto de 0, según información de póster):", n_new))

# Calcular el valor absoluto y redondearlo a 3 decimales
tabla$'|Xi-Mn|' <- abs(tabla$`Xi-Mn`)
tabla$'|Xi-Mn| redondeado' <- round(tabla$`|Xi-Mn|`, 3)

# Ordenar por el valor absoluto redondeado
tabla <- tabla[order(tabla$`|Xi-Mn| redondeado`), ]

# Asignar rango simple de 1 al n (nuevo)
tabla$Rango <- 1:n_new

# Cargar dplyr para calcular rangos promedio en caso de empates
library(dplyr)
tabla <- tabla %>%
  group_by(`|Xi-Mn| redondeado`) %>%
  mutate(Ri = mean(Rango)) %>%
  ungroup()

# Calcular Ri dividido por 2(n - 1)
tabla$'Ri/(2(n-1))'= tabla$Ri/(2*(n_new-1))

# Asigna el signo de la diferencia de Xi-Me
tabla$Signo = sign(tabla$`Xi-Mn`)

# Calcular G_alpha como el mínimo entre Ri/(2(n-1)) y (0.5 - alpha)
tabla$G <- pmin(tabla$`Ri/(2(n-1))`, (0.5-alpha))
print("Se calculo el G como el valor mínimo entre Ri/(2(n-1)) y (0.5 - alpha)")


# Calcular Signo * G
tabla$'Signo*G'= tabla$Signo*tabla$G

# Calcular el estadístico T
T_ = sum(tabla$`Signo*G`)/sqrt(n_new)
print(paste("Estadístico T:", round(T_, 4)))

# Calcular el estadístico estandarizado T*
print("A continuación se utiliza valor de la varianza asintótica citada en el póster")
var_alpha <- 0.030  # varianza asintótica para alpha = 0.05.
T_normalizada= T_/sqrt(var_alpha)
print(paste("Estadístico T estandarizado:", round(T_normalizada, 4)))

# Comparar con valor crítico
Z_alpha <- qnorm(1 - alpha / 2)
print(paste("Valor crítico Z (alfa = 0.05):", round(Z_alpha, 4)))

# Decisión final
if (abs(T_normalizada) > Z_alpha) {
  print("Se rechaza H0: la distribución no es simétrica respecto a la mediana con un nivel de significancia de 0.05 .")
} else {
  print("No se rechaza H0: no hay evidencia suficiente para decir que la distribución no es simétrica con un nivel de confianza de 0.05.")
}



