## Caso 1. Modelos lineales randomizados.
library(performance)
library(ggplot2)
vacas <- read.table("vacas.txt", header = TRUE)

fit <- lm(fat ~ breed, data = vacas)
anova(fit)

check_model(fit)
check_heteroscedasticity(fit)
check_normality(fit)

# Guardar las F observadas
Fobs <- anova(fit)[1, 4]
Fobs

# construir una function que repita los pasos del modelo
rd.aov <- function(Y, X){
  s.Y <- sample(x = Y, size = length(Y), replace = F)   # "reshuffling"
  fit <- lm(s.Y ~ X)                   # modelo lineal
  res <- anova(fit)[1, 4]              # extracción de las F
  c(res)                               # mostrar: un vector de F
}  

# Probar la función varias veces (debe dar distinto resultado)
rd.aov(Y = vacas$fat, X = vacas$breed)
rd.aov(Y = vacas$fat, X = vacas$breed)

# Realizar réplicas para obtener la distribución de pseudo F
pseudoF <- replicate(1000, rd.aov(Y = vacas$fat, X = vacas$breed))
PF <- data.frame(pseudoF)	#transposición para que cada F quede en una columna

# histograma para ver los resultados simulados y los observados
ggplot(data=PF, aes(x = pseudoF)) + 
  geom_histogram() + 
  geom_vline(xintercept = Fobs, color = "red")

# valores P
P.breed <- length(pseudoF[pseudoF >= Fobs]) / length(pseudoF)
P.breed  

# como nuestra simulación llegó hasta 1000
# no podemos asegurar que sea 0, sino que P es al menos <0.001

#############################################################

## Caso 2: Un Monte Carlo sencillo.
esp <- read.table("espMC.txt", header = TRUE)
plot(y ~ x, data=esp, pch = 19, col = "purple4")

# calcular las distancias euclídeas entre las filas de la base de datos
# como parámetro observado se elige la media  de todas las distancias 
# (¿por qué elijo ese parámetro?)

Dobs <- mean(dist(esp))		#revisar la ayuda de la función dist
Dobs

# Realizamos una simulación de la distribución de los árboles según una 
# distribución uniforme
X.azar <- runif(n = nrow(esp), min = 0, max = 2)		
Y.azar <- runif(n = nrow(esp), min = 0, max = 2)
plot(X.azar, Y.azar)
Dazar <- mean(dist(cbind(X.azar, Y.azar)))
Dazar

# Repetimos la simulación 1000 veces y guardamos los resultados
Ds <- numeric(length(1000))
for (i in 1:1000) {
  X.azar <- runif(n=24, min=0, max=2)		
  Y.azar <- runif(n=24, min=0, max=2)
  Ds[i] <- mean(dist(cbind(X.azar, Y.azar)))
}

# visualizamos los resultados con un histograma
hist(Ds)
abline(v = Dobs, col = "red")

# calcular el valor P
P <- length(Ds[Ds < Dobs]) / length(Ds)
P

#### END ####