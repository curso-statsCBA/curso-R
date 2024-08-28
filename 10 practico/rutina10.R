##Caso1
library(boot)
cyc <- read.table("cyclop.txt", header = TRUE)

reg <- lm(pol.exp ~ flores + nectario, data = cyc)
summary(reg)

layout(matrix(1:4, 2, 2))
plot(reg)
layout(1)

# Guardamos los coeficientes observados
Bobs <- reg$coeff
Bobs

# PASO 1: crear una función para "bootstrapear"
REG <- function(datos, indices) {
  newdata <- datos[indices, ]  #sobre esta línea actuará la función boot  
  fit <- lm(pol.exp ~ flores + nectario, data = newdata)
  fit$coeff
}

# PASO 2: el bootstrap en sí
boot.reg <- boot(cyc, REG, 9999)
boot.reg
summary(boot.reg)

# histograma de los resultados
layout(matrix(1:4, 2, 2))
hist(boot.reg$t[, 1], main = "intercept") 
abline(v = Bobs[1], col="red") 
hist(boot.reg$t[, 2], main = "flores") 
abline(v = Bobs[2], col="red") 
hist(boot.reg$t[, 3], main = "nectario") 
abline(v = Bobs[3], col = "red") 
layout(1)

## PASO 3: intervalos de confianza
?boot.ci #examinar la ayuda

# prueba, los 5 intervalos distintos que calcula R
int3.95<-boot.ci(boot.reg, conf = 0.90, type = "all", index = 3)
int3.95

# Examinamos que el intervalo de confianza NO incluya al 0
# (nos salteamos la significancia de la ordenada al origen)
# intervalos para la pendiente de "flores"
int2.90 <- boot.ci(boot.reg, conf = 0.90, type = "bca", index = 2)
int2.95 <- boot.ci(boot.reg, conf = 0.95, type = "bca", index = 2)
int2.99 <- boot.ci(boot.reg, conf = 0.99, type = "bca", index = 2)
int2.999 <- boot.ci(boot.reg, conf = 0.999, type = "bca", index = 2)

int2.90
int2.95
int2.99
int2.999

# intervalos para la pendiente de "nectario"
int3.90 <- boot.ci(boot.reg, conf = 0.90, type = "bca", index = 3)
int3.95 <- boot.ci(boot.reg, conf = 0.95, type = "bca", index = 3)
int3.99 <- boot.ci(boot.reg, conf = 0.99, type = "bca", index = 3)
int3.999 <- boot.ci(boot.reg, conf = 0.999, type = "bca", index = 3)

int3.90
int3.95
int3.99
int3.999

# PASO 4: plots de diagnóstico (salteamos el de intercepto)
plot(boot.reg, index = 2, jack = T)
plot(boot.reg, index = 3, jack = T)

# el jack-after-boot en más detalle
# influencia de las observaciones individuales sobre la pendiente de 
# "flores"

jack.after.boot(boot.reg, index = 2) 

# ídem sobre la pendiente de "nectario"
jack.after.boot(boot.reg, index = 3) 

# la línea horizontal es una medida de la influencia de cada observación
# la línea vertical presenta los percentiles 5, 10, 16, 50, 84, 90 y 95 de 
# la distribución del coeficiente

#### END ####