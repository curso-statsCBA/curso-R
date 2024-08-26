##Caso 1. 
# carga de datos
fum <- read.table("fumadores.txt", header = TRUE, stringsAsFactors = TRUE)
str(fum)

# inspección de datos 1: gráfico tipo Cleveland
plot(fum$ca.pulm)
plot(fum$alt)

# inspección de datos 2: gráfico bivariado
plot(fum$alt, fum$ca.pulm)

# modelo lineal (regresión)
fit<-lm(ca.pulm ~ alt, data=fum)

# El objeto fit contiene numerosos componentes, a los cuales podemos acceder de 
# diferentes formas (ver ?lm). Por ej.
fit$residuals

# resúmenes de información
summary(fit)
anova(fit)

# diagnósticos gráficos. Layout dividirá la ventana gráfica en lo que 
# indique la matriz (4 en este caso, en 2 filas y 2 columnas). Para que la 
# ventana gráfica vuelva a la normalidad usaremos layout(1)
# NOTA: ocasionalmente la ventanna gráfica de RStudio es muy pequeña
# Podemos llamar una nueva con x11()
layout(matrix(1:4,2,2))
plot(fit)
layout(1)

# diagnósticos numéricos
shapiro.test(fit$residuals)


# diagnósticos "explicados": performance
# (útil en modelos más complejos)
library(performance)
x11()
check_model(fit)
check_normality(fit)
check_heteroscedasticity(fit)
model_performance(fit)

# validación cruzada
library(caret)

fitControl <- trainControl(method = "cv", number = 10)

lmFit1 <- train(ca.pulm ~ alt,
               data =fum,
               method = "lm", 
               trControl = fitControl)

lmFit2 <- train(ca.pulm ~ alt + I(alt^2),
                data =fum,
                method = "lm", 
                trControl = fitControl)

# Root mean squared error (RMSE)
# simple R2
# mean absolute error (MAE)

lmFit1
lmFit2

################################################################################
## Caso 2. 
data(InsectSprays)
str(InsectSprays)

plot(sqrt(count) ~ spray, data = InsectSprays)

# utilizando la function lm
fit.spray1 <- lm(sqrt(count) ~ spray, data = InsectSprays)
anova(fit.spray1)
summary(fit.spray1)

# utilizando la function aov
fit.spray2 <- aov(sqrt(count)~spray, data = InsectSprays)
summary(fit.spray2)

#diagnósticos gráficos
layout(matrix(1:4,2,2))
plot(fit.spray2)
layout(1)

# diagnósticos numéricos
shapiro.test(resid(fit.spray2))
bartlett.test(resid(fit.spray2)~InsectSprays$spray)

# diagnósticos con performance
library(performance)
check_model(fit.spray2)

# Test de Tukey
# notar que trabaja sobre un objeto aov, no sobre un objeto lm
tuk <- TukeyHSD (fit.spray2)
tuk

# Otros tests a posteriori y contrastes
library(emmeans)
EMM <- emmeans(fit.spray1, specs = "spray")
EMM
contrast(EMM, method = "pairwise")
?"contrast-methods"

#### END ####
