## Uso de la función plot.
datos <- read.table("cyclop.txt", header = TRUE)

# UNA VARIABLE
plot(datos$nectario)  # gráfico tipo Cleveland
plot(~datos$nectario) # Stripchart

#con los datos ordenados de menor a mayor
plot(sort(datos$nectario))

# cambiando el tipo de gráfico 
# probar los diferentes "type": p, l, b, o, h, s, S
plot(sort(datos$nectario), type = "h") 

#agregamos una línea horizontal señalando la posición de la media
#usando la función de bajo nivel abline
m.nec <- mean(datos$nectario, na.rm = TRUE)
abline(h = m.nec)       	

# DOS VARIABLES
plot(datos$nectario, datos$pol.exp)       #forma plot(x,y)
plot(pol.exp ~ nectario, data=datos)      #forma plot(y~x)

#manipulación de los PARAMETROS GRÁFICOS
plot(datos$nectario, datos$pol.exp,
     pch= 16,                             #tipo de símbolo
     col= "blue2",                        #color del símbolo
     xlab= "profundidad del nectario",    #nombre eje x
     ylab= "polinios exportados")         #nombre eje y

#agregando una línea de regresión simple
fit<-lm(datos$pol.exp ~ datos$nectario)
summary(fit)
abline(fit,	lty = 3, lwd = 2)

#revisar donde se guardaran los gráficos
getwd()
#puede cambiarse con setwd()

# JPG
jpeg(file = "mi_grafico.jpg", 
     width = 12, height = 10,  #ancho y alto en lo que indique units
     units = "cm",             #unidades (pixeles por defecto)
     quality = 95,             #grado de no-compresión
     res = 300)                #resolución en puntos por pulgada   
plot(pol.exp ~ nectario, data=datos)  #comandos gráficos
dev.off()                      #cerrar el gráfico (importante!!)

# PDF
pdf(file = "mi_grafico.pdf",
    width = 7, height = 6,#ancho y alto en pulgadas (=2.54cm)
    paper = "a4")         #tamaño del papel, por defecto igual al gráfico
plot(pol.exp ~ nectario, data=datos)  #comandos gráficos
dev.off()                             #cerrar el gráfico

# EPS
postscript(file = "mi_grafico.eps",
           width = 7, height = 6,#ancho y alto en pulgadas (=2.54cm)
           paper = "a4",         #tamaño del papel, por defecto igual al gráfico
           horiz = TRUE)         #horizontal (FALSE) o vertical (TRUE)
plot(pol.exp ~ nectario, data=datos)  #comandos gráficos
dev.off()                             #cerrar el gráfico

# SVG
svg(file = "mi_grafico.svg",
    width = 7, height = 6)   #ancho y alto en pulgadas (=2.54cm)
plot(pol.exp ~ nectario, data=datos)  #comandos gráficos
dev.off()                             #cerrar el gráfico

#############################################################

## Otros gráficos sencillos con variables continuas.
# histogramas
hist(datos$nectario, col = "orange1", main= "histograma",
     xlab = "nectario", ylab = "frecuencia")

#QQplots
qqnorm(datos$nectario)

############################################################

## División de la ventana gráfica.
# Básica con la función layout
# Extra: colores en escala hexadecimal (eg. https://paletton.com)
layout(matrix(1:4, 2, 2))			
hist(datos$nectario, col= "#CAFF70") 		
hist(datos$flores, col= "#BCEE68")
hist(datos$frutos, col= "#A2CD5A")
hist(datos$pol.exp, col= "#6E8B3D")
layout (1) 					

# Por defecto, layout() divide el dispositivo en dimensiones regulares: 
# esto se puede modificar con las opciones widths y heights.
m <- matrix(1:4, 2, 2)
layout(m, widths=c(1, 2),
       heights = c(2, 1))
layout.show(4)
hist(datos$nectario, col = "#CAFF70")
hist(datos$flores, col = "#BCEE68")
hist(datos$frutos, col = "#A2CD5A")
hist(datos$pol.exp, col = "#6E8B3D")
layout(1)

#división de la ventana usando las opciones de par
par(mfrow = c(2, 2))
hist(datos$nectario, col = "#CAFF70")
hist(datos$flores, col = "#BCEE68")
hist(datos$frutos, col = "#A2CD5A")
hist(datos$pol.exp, col = "#6E8B3D")

par(mfcol = c(1, 4))
hist(datos$nectario, col = "#CAFF70")
hist(datos$flores, col = "#BCEE68")
hist(datos$frutos, col = "#A2CD5A")
hist(datos$pol.exp, col = "#6E8B3D")

# cerrar la ventana gráfica para desactivar par o
par(mfcol=c(1,1)) #deshacer lo anterior

# revisar las opciones gráficas de par
?par

#############################################################

## Gráficos de cajas.
peces <- read.table("peces.txt", header = TRUE)
peces$Species <- as.factor(peces$Species)

#dos formas alternativas de hacer box-plots
plot(peces$Species, peces$Weight)
boxplot(Weight ~ Species, data = peces, col = "light blue")

#############################################################

## Interacciones entre variables categóricas.
library(sciplot) 

bival <- read.table("bivalvos.txt", header = TRUE)

# modelo 
fit <- lm(huevos ~ densidad*estacion, data = bival)
anova(fit)

#GRÁFICOS DE BARRAS
bargraph.CI(x.factor = densidad, #factor (eje x)
            response = huevos,   #respuesta (eje y)
            group = estacion,    #factor optativo (distintos colores)
            legend = TRUE,
            data = bival)

#GRAFICOS DE INTERACCIÓN
lineplot.CI(x.factor = densidad, response = huevos, group = estacion,
            data = bival, legend = TRUE, 
            col = c("red", "black"),  
            pch = c(1,20),
            xlab = "densidad", 
            ylab = "número de huevos")

############################################################

## Gráficos a partir de predichos.
datos <- read.table("cyclop.txt", header = TRUE)

fit.c <- lm(pol.exp ~ flores + I(flores^2), data = datos)
summary(fit.c)

# set de datos nuevo y "suave"
FLO2<-seq(min(datos$flores, na.rm = TRUE),  # desde el mínimo
          max(datos$flores, na.rm = TRUE),  # hasta el máximo
          length.out = 200)                 # nueva longitud

# usar los mismos nombres que las variables originales
Y3 <- predict(fit.c, newdata = data.frame(flores=FLO2))

# plot
plot(datos$flores, datos$pol.exp, 
     pch = 19,
     col = rgb(0.5,0,1,0.3), 
     xlab = "flores",
     ylab = "polinarios")
points(FLO2, Y3, type = "l", lwd = 2, col = "red")

#############################################################

## Interacciones de variables continuas: superficies.
datos <- read.table("cyclop.txt", header = TRUE)

fit <- lm(formula = pol.exp ~ flores * nectario + I(flores^2) + I(nectario^2), 
          data = datos)

r.nec <- range(datos$nectario, na.rm = TRUE)
NEC <- seq(r.nec[1], r.nec[2], length.out = 100)
r.flo <- range(datos$flores, na.rm=T)
FLO <- seq(r.flo[1], r.flo[2], length.out = 100)

# Construcción de una grilla
grilla <- expand.grid(flores=FLO, nectario=NEC)

# predicción del eje Z
Z1 <- predict(fit, newdata = grilla)
Z2 <- matrix(Z1, 100, 100)

# GRAFICOS 3 D: DISTINTAS OPCIONES

# 2D, con curvas de nivel (tipo "mapa topografico")
contour(x=FLO, y=NEC, z=Z2, col = "Black")

# 2D, con un gradiente de color 
filled.contour(x=FLO, y=NEC, z=Z2, color = terrain.colors)

# 2D, gradiente de color con image y superposición
# ejecutar lína a línea para ver el cambio
varios <- rainbow(50, start = 0.1, end = 0.5) # 10 tonos de arcoiris 
image(x=FLO, y=NEC, z=Z2, col = varios, 
      xlab = "número de flores", ylab = "nectario")
contour(x = FLO, y = NEC, z = Z2, col = "red4", add = TRUE)
points(x = datos$flores, y = datos$nectario, col = "red4", pch = 19) 

# 3D, con rejilla y en perspectiva
persp(x = FLO, y = NEC, z = Z2)

#manejo de los parámetros gráficos (sólo algunos)
persp(x = FLO, y = NEC, z = Z2, 
      border = "slateblue4",       #border: color de la rejilla  
      col = rgb(0.2, 0.8, 0.8, 0.3),         #col: color de la superficie
      xlab = "número de flores",   #xlab, ylab, zlab: etiquetas ejes
      ylab ="nectario",
      zlab = "polinarios",
      theta = -45,                 #theta: ángulo de giro horizontal
      phi = 20,                    #phi: ángulo de rotación vertical
      ticktype = "detailed"        #para que ponga el valor de los ejes
) -> res                           #guardo la "forma" del gráfico 

#agregar de los puntos observados con la funcion trans3d
pru<-trans3d(x = datos$flores, y = datos$nectario, z = datos$pol.exp, pmat = res)
points(pru, col = "grey", pch = 20)

#### END ####