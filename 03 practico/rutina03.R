## Caso 1. 
library(car)  
library(performance)

dat <- read.table("moscas.txt", header = TRUE, stringsAsFactors = TRUE)

plot(dat$torax)
plot(dat$vida)
plot(dat$trat,dat$vida)
plot(dat$torax,dat$vida)

#diferentes sumas de cuadrados (ejemplo didáctico)
# BAJO NINGUNA CIRCUSNTANCIA DEBEN PROBARSE DIFERENTES SUMAS DE CUADRADOS
# NO SON HERRAMIENTAS EXPLORATORIAS, SINO QUE SE ESTABLECEN EN EL DISEÑO
fit1.a <- lm(vida ~ trat*torax, data = dat)
fit1.b <- lm(vida ~ torax*trat, data = dat)

# Tipo 1
anova(fit1.a) 
anova(fit1.b)

# Tipo 2
Anova(fit1.a, type = "II")
Anova(fit1.b, type = "II")

# Tipo 3
dat$torax_c <- scale(dat$torax, center = T, scale = F)
dat$trat_c <- dat$trat
contrasts(dat$trat_c) <- contr.sum
fit1.c <- lm(vida ~ trat_c*torax_c,  data = dat)
Anova(fit1.c, type = "III")

# el tipo 3 requiere más manipulación de los datos, comparar con
Anova(fit1.a, type = "III")

# revisando el resultado 
summary(fit1.a)
layout(matrix(1:4, 2, 2))
plot(fit1.a)
layout(1)


# selección del modelo basado en significancia
fit1 <- lm(vida ~ trat * torax, data = dat)
fit2 <- lm(vida ~ trat + torax, data = dat)
fit3 <- lm(vida ~ trat, data =dat)
fit4 <- lm(vida ~ torax, data =dat)
fit5 <- lm(vida ~ 1, data =dat)

anova(fit1, fit2, fit3, fit4, fit5) #comparación secuencial
compare_performance(fit1, fit2, fit3, fit4, fit5) # criterios

# revisando el modelo final
layout(matrix(1:4, 2, 2))
plot(fit3)
layout(1)

## END