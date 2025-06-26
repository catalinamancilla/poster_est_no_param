#Prueba Mann-Whitney

#Datos sobre el crecimiento de plantas bajo diferentes tratamientos.
data(PlantGrowth)
#Se toma solo el crecimiento de los grupos ctrl y trt1
grupo1 <- PlantGrowth$weight[PlantGrowth$group == "ctrl"]
grupo2 <- PlantGrowth$weight[PlantGrowth$group == "trt1"]
#Cantidad de observaciones del primer grupo 
n <- length(grupo1)

#Se combinamos los valores de ambos grupos en un solo vector
valores <- c(grupo1,grupo2)

#Se asigna rangos a los valores combinados
#La función rank() asigna un número a cada valor indicando su posición (ordenada de menor a mayor)
#En caso de empates (valores iguales), se asigna el promedio de los rangos que corresponderían
rangos <- rank(valores)
rangos

#Extraemos los rangos correspondientes al grupo 1 (primeros n elementos del vector)
rango_g1 <- rangos[1:length(grupo1)]
#Extraemos los rangos correspondientes al grupo 2 (los siguientes elementos)
rango_g2 <- rangos[(length(grupo1)+1):length(valores)]

rango_g1  # x_i
rango_g2  # y_i

#Se calcula la sumatoria de los rangos de ambos grupos
total_xi <- sum(rango_g1)
total_yi <- sum(rango_g2)

total_xi
total_yi

#Se calcula el estadístico de prueba
T_ <- total_xi-((n*(n+1))/2)
T_

#Valor de alpha
alpha <- 0.05
#Valor critico segun tablas de valores criticos del estadistico de Mann-Whitney
T_critico <- 24

# Decisión final
if (T_ < T_critico) {
  print("Se rechaza H0: existe evidencia suficiente para afirmar que hay diferencia entre los grupos (nivel 0.05).")
} else {
  print("No se rechaza H0: no existe evidencia suficiente para afirmar que hay diferencia entre los grupos (nivel 0.05).")
}



