##Construcción de funciones.
###Caso 1.

DAT <- c(13, 2, 4, 0, 36, 3, 2, 7, 1, 11)  # datos originales (inventados)
dat <- (DAT[DAT > 0]) # elimino los 0 (por los log)
N <- sum(dat) # total de individuos
p <- dat / N # abundancias relativas
H <- -sum(p * log(p))
H # resultado

# construcción de la función 
# notar las similitudes y diferencias con los pasos de arriba.
shann <- function(x) {
  z <- (x[x > 0])
  N <- sum(z)
  p <- z / N
  H <- -sum(p * log(p))
  H # la última línea es lo que mostrará la función
}

# probamos la función con el set de datos original
shann(DAT)

# probamos la función con el otro set de datos
Y <- c(67, 8, 1, 6, 8, 5, 9, 7, 10, 1, 0, 1, 2,0, 2, 1)
shann(Y)

############################################################

##Funciones de simulación y muestreo.

# simulación de datos
X <- rnorm(100, mean = 4, sd = 0.5)
hist(X)

# probabilidad de observar el valor z < 5.4 bajo la distribución simulada arriba
pnorm(5.4, mean = 4, sd = 0.5)

# valor del percentil 75 bajo la misma distribución
qnorm(0.75, mean = 4, sd = 0.5)

##############################################################

# Exploración de las utilidades de sample
AA <- c(1:10)
AA

#sólo cambio el orden: permutación
sample(AA, size = 10, replace = FALSE)

#muestreo aleatorio sin remplazo
sample(AA, size = 5, replace = FALSE)

#muestreo aleatorio con remplazo
sample(AA, size = 5, replace = TRUE)

#idem, pero de igual n que la muestra original, base del bootstrap
sample(AA, size = 10, replace = TRUE)

?sample

#############################################################

## Programación con R

# for

Dat <- runif(1000)
vec <- numeric(length = 50)
vec # vector vacío antes del loop
for (i in 1:50) {
  vec[i] <- mean(sample(Dat, 20))
}
vec # vector luego de aplicar el loop
plot(density(vec))

# for + if
# construir una distribución normal truncada
X1 <- rnorm(1000, mean = 3, sd = 5)
X2 <- numeric(1000)
for (i in 1:1000) if (X1[i] < 0) X2[i] <- NA else X2[i] <- X1[i]
hist(X2)

# while
i <- 0
while (i < 10) {
  i <- i + 1
}
i

# apply
m <- rnorm(200)
X <- matrix(m, 40, 5)

apply(X, 2, mean)
apply(X, 1, mean)


# apply vs. for vs. vectorización
# ejemplo 1
A <- c(8, 23, 11, 10)
B <- c(15, 3, 2, 1)

#con loop (MAL)
C <- numeric(4)
for (i in 1:4) C[i] <- A[i] + B[i]
C

# vectorizado (BIEN)
C <- A + B
C

# ejemplo 2
m <- rnorm(200)
X <- matrix(m, 40, 5)

# con loop (MAL)
med <- numeric(5)
for (i in 1:5) med[i] <- mean(X[, i])
med

#vectorizado (BIEN)
med <- apply(X, 2, mean)
med

#### END ####