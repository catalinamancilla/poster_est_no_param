# Código para crear gráfico de región de rechazo para prueba de Wilcoxon

# Definir valor de alpha
alpha = 0.05

# Calcular valor crítico bilateral
z_critico <- qnorm(1 - alpha / 2)  

# Crear eje Z
z <- seq(-4, 4, length = 1000)
densidad <- dnorm(z)

# Iniciar el gráfico
plot(z, densidad, type = "l", lwd = 2, col = "black",
     main = "Prueba de Simetría de Wilcoxon",
     xlab = "Estadístico Z", ylab = "Densidad")

# Región de rechazo izquierda
polygon(c(z[z <= -z_critico], -z_critico),
        c(dnorm(z[z <= -z_critico]), 0),
        col = "lightblue1", border = NA)

# Región de rechazo derecha
polygon(c(z[z >= z_critico], z_critico),
        c(dnorm(z[z >= z_critico]), 0),
        col = "lightblue1", border = NA)

# Líneas de corte: valores críticos
abline(v = c(-z_critico, z_critico), col = "blue", lty = 2)
text(-z_critico+0.5, 0.01, expression(-Z[alpha/2]), pos = 2, col = "black")
text(z_critico-0.5, 0.01, expression(Z[alpha/2]), pos = 4, col = "black")

# Zona de no rechazo
text(0, 0.35, "Zona de no rechazo", col = "lightblue4", cex = 0.9)

# Etiquetas de las zonas de rechazo
text(-3.2, 0.07, "Rechazo H0", col = "blue4", font = 2)
text(3.2, 0.07, "Rechazo H0", col = "blue4", font = 2)

# Etiqueta para mostrar valor de alpha
text(3.2,0.35,"alpha = 0.05", col="black", front=2)


