### Gráficos del paquete lattice
library(lattice)

fum <- read.table("fumadores.txt", header=T)

#gráficos bivariados
# Notar que ingresar los datos como una fórmula es más eficiente
xyplot(ca.pulm ~ alt, data = fum)
xyplot(ca.pulm ~ alt | edad, data = fum)
xyplot(ca.pulm ~ alt | fuma, data = fum)
xyplot(ca.pulm ~ alt | fuma*sexo, data = fum)
xyplot(ca.pulm ~ alt, groups = fuma, data = fum)

# manipular alguna de sus opciones...
xyplot(ca.pulm ~ alt, groups = fuma, data = fum, xlab = "altura", 
       ylab = "capacidad\npulmonar", col = c("red", "black"), pch = 19, 
       lwd = 2, key = list(text = list(c("no fumadores", "fumadores")), 
                           space = "bottom", 
                           points = list(pch = 19, col = c("red", "black"))))

# otros gráficos comunes
bwplot(ca.pulm ~ fuma | sexo, data = fum)
histogram(~ ca.pulm, data = fum)
densityplot(~ ca.pulm | fuma, data = fum)

#############################################################

## Gráficos del paquete ggplot2

library(ggplot2)
library(viridis)
library(patchwork)

fum <- read.table("fumadores.txt", header=T)

#utilizando ggplot
ggplot(data = fum, aes(x = edad, y = ca.pulm)) + geom_point()    

# notar como los gráficos se van acumulando
# (no hacer esto, salvo para buscar errores)
g1 <- ggplot(data = fum)  
g2 <- g1 + geom_point(mapping = aes(x = edad, y = ca.pulm))
g2    

# modificando algunos detalles comunes
# notar el + al final (equivale a una línea continua)
# notar la diferencia entre colocar variables adentro o afuera de aes()
g1 <- ggplot(data = fum) +
  geom_point(aes(x = edad, y = ca.pulm, shape = sexo), size = 3, alpha = 0.4) +
  xlab("Edad") +
  ylab("Capacidad Pulmonar") +
  theme_bw()
g1

# modificando los colores 
c1 <- ggplot(data = fum) +
  geom_point(aes(x = edad, y = ca.pulm, color = fuma), size = 3, alpha = 0.3) +
  scale_colour_manual(values=c("red", "black"))
c1  

# modificando los colores con una escala continua
c2 <- ggplot(data = fum) +
  geom_point(aes(x = edad, y = ca.pulm, color = alt), size = 3) 
c2  

# modificando los colores con una escala de viridis
c3 <- ggplot(data = fum) +
  geom_point(aes(x = edad, y = ca.pulm, color = alt), size = 3) + 
  scale_color_viridis_c(option = "magma")
c3  

# modificando tamaños con una escala continua
t1 <- ggplot(data = fum) + 
  geom_point(aes(x = edad, y = ca.pulm, size = alt, color = fuma)) 
t1

# diferencia entre mapeos locales y generales
z1 <- ggplot(data = fum) + 
  geom_point(mapping = aes(x = edad, y = ca.pulm, color = sexo)) +
  geom_smooth(mapping = aes(x = edad, y = ca.pulm))
z1

z2 <- ggplot(data = fum, aes(x = edad, y = ca.pulm, color = sexo)) + 
  geom_point() +
  geom_smooth()
z2

# Faceting (división del la ventana gráfica como en lattice)
f1 <- ggplot(data = fum) +
  geom_point(aes(x = edad, y = ca.pulm, color = fuma), size = 3) +
  theme_bw() + 
  facet_grid(. ~ sexo)
f1

f2 <- ggplot(data = fum) + 
  geom_point(aes(x = edad, y = ca.pulm, color = fuma), size = 3) + 
  scale_color_manual(values = c("red", "blue")) +
  theme_bw() + 
  facet_grid(fuma ~ .)
f2

f3 <- ggplot(data = fum) + 
  geom_point(aes(x = edad, y = ca.pulm, color = fuma), size = 2) + 
  theme_bw() + 
  scale_color_manual(values = c("red", "blue")) +
  facet_wrap(~fuma*sexo, ncol = 2)
f3

# gráficos de cajas
b1 <- ggplot(data = fum) + 
  geom_boxplot(aes(x = fuma, y = ca.pulm, fill = fuma)) +
  scale_fill_viridis_d()
b1

# histogramas
h1 <- ggplot(data = fum) + 
  geom_histogram(aes(x = ca.pulm), fill="red")
h1

# combinando gráficos (patchwork)
c3 / b1

c3 | b1

c3 / (b1 + h1)

#### END ####
