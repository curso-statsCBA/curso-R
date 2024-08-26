################################################################################
# El símbolo # desactiva el espacio a su derecha.
# Es útil para poner aclaraciones en nuestras rutinas.

# Asignar crea objetos.
# Puede usarse = en su reemplazo o -> para asignar el nombre al final.
# Para unificar el estilo de todas las rutinas aquí sólo usaré <-
n <- 15  
n   # escribir el nombre de un objeto es "invocarlo"

# al usar de nuevo el mismo nombre el objeto anterior se pierde ("pisarlo")
n <- 46 + 12  				 
n

# Obtener ayudas, el símbolo ?
# si tenemos dudas sobre el funcionamiento de una función
? lm

# Si desconocemos el nombre de una función para realizar determinada
# tarea, puede realizarse una búsqueda con ?? (sólo en los paquetes instalados)
?? "linear models"

# crear vectores. La función concatenar c
# los caracteres categóricos se ingresan con comillas
vector <- c(1, 2, 3, 4)
vector <- c("a", "b", "c", "d") 

################################################################################

# vectores de repetición. 
# esta función argumentos, separados por una coma
xx <- rep(x = "A", times = 50) 
xx

# si no se aclaran los argumentos, R asume su valor posicional
xx <- rep("A", 50)
xx

# secuencias 
seq1 <- seq(from = 0, to = 0.99, by = 0.02) # argumentos explícitos
seq1

# combinando vectores de a pares: cbind y rbind
az <- cbind(seq1, xx)
az

za <- rbind(c(1:25), rnorm(25))	
za

# construyendo una matriz: matrix()
matriz1 <- matrix(c(1:20), 4, 5) # (vector a usar, filas, columnas) 
matriz1

################################################################################

## Preparación e ingreso de datos

# la función read.table 
# opción 1 con ruta completa (notar orientación de las barras /)
# en este caso asumimos que se encuentra en el directorio RD
datos <- read.table("C:/RD/datos/peces1.txt", header=TRUE)

# opción 2 seleccionando el directorio en uso previamente desde "Archivo"
# o con setwd
setwd("C:/RD/") ## "C:/RD/" es un ejemplo...
datos <- read.table("peces1.txt", header = TRUE, stringsAsFactors = TRUE)

# opción 3 Abre una ventana de búsqueda 
# desventaja: requiere que el humano trabaje!
datos <- read.table(file.choose(), header=TRUE, stringsAsFactors = TRUE)

# opción 4 la función read.csv
datos <- read.csv("C:/RD/peces1.csv", header=TRUE, stringsAsFactors = TRUE)

# una forma práctica de ver si los datos cargaron como se espera es
# ver solo las primeras filas es con head() 
# o echar un vistazo a la estructura de los datos con str() o summary()
head(datos)
str(datos)
summary(datos)

# otras maneras de tener información básica
nrow(datos)     # número de filas
ncol(datos)     # número de columnas
names(datos)    # nombre de las columnas

################################################################################

## Indexación

# los objetos tienen diferentes dimensiones:
# 1: vectores
# 2: matrices y data.frames (filas y columnas)
# 3: arrays
# los corchetes sirven para identificar un elemento según su nombre o posición

A <- c(1, 2, 9, 67, 8)
A[3] # tercer elemento

datos[, 1]          # primer columna                         
datos[, "grupo"]    # columna llamada grupo
datos$grupo         # forma corta

datos[, 1:3]        # varias columnas                   
datos[, c("grupo", "largo.a")]

datos[2, ]          # filas                     
datos[2, 3]         # elementos

# indexando lógicamente con condiciones
datos[-2, 2]        # exclusión de una fila
datos[datos$largo.a > 15, ]
datos[datos$largo.a > 15 & datos$grupo == "A", ]

################################################################################

# Subdivisión de un conjunto de datos
dat1 <- subset(datos, datos$grupo == "A")
dat1

dat2 <- subset(datos, datos$grupo == "A" & datos$trat == 2)
dat2

# Crear una lista de bases de datos
ldat <- split(datos, f = datos$grupo) 
ldat
ldat[["B"]] # indexacion d listas

################################################################################

## Modificación y creación de columnas.

# reemplazar (al usar el mismo nombre) una variable numérica por un factor
datos$trat
datos$trat <- as.factor(datos$trat) 
datos$trat    # notar la lista de niveles del factor

# crear una nueva columna en una base de datos ya existentes
# (utilizo un nombre nuevo)
datos$pob <- c(rep("pob1", 15), rep("pob2", 15))

datos$log.largo.a <- log(datos$largo.a)

head(datos)

# crear una nueva columna uniendo clasificadores
datos$clave <- paste(datos$pob, datos$trat, sep = ".")
head(datos)

# Usando factor para arreglos más complicados
datos$pais <- factor(datos$pob, levels = c("pob2", "pob1"), 
                     labels = c("Bolivia", "Chile"))
head(datos)

################################################################################

# media
M <- mean(datos$largo.a, na.rm = TRUE)
M

# desvío estándar
S <- sd(datos$largo.a, na.rm = TRUE)
S

# varianza
V <- var(datos$largo.a, na.rm = TRUE)
V

# sobre un conjunto de datos, se obtiene una matriz de varianza-covarianza
VC <- var(datos[,c(3,4)], na.rm = TRUE)
VC

# de forma parecida, puede obtenerse una matriz de correlación
CO <- cor(datos[, c(3, 4)], use="complete.obs")  #complete.obs elimina NA
CO

# en cambio, para realizar un test de correlación
cor.test(datos$largo.a, datos$largo.b, method = "pearson")

# funciones que crean resúmenes aplicados a subconjutos son 
# tapply (de la serie apply) y aggregate, con una estructura más complicada

SG <- tapply(datos$largo.a, datos$grupo, mean, na.rm = TRUE)
SG 

MG <- aggregate(datos$largo.a, by = list(datos$grupo), FUN = mean, na.rm = T)
MG


################################################################################

library(dplyr)
# Subdivisión o *filtrado*
dat1 <- filter(datos, grupo == "A")
dat2 <- filter(datos, grupo == "A", largo.b > 100)

# Selección por colunmas *select*
dat3 <- select(datos, grupo, largo.a)

# select tiene funciones de ayuda. Por ejemplo, seleccionar todas las 
# columnas que comiencen con "larg". Ver ?select para más ejemplos.
dat4 <- select(datos, starts_with("larg"))

# Modificación de columnas *mutate*
# puede crearse un nuevo set de datos como aquí o modificarse el original
dat5 <- mutate(datos,
               prop = largo.b / largo.a, # columna nueva
               log.largo = log(largo.a)) # columna nueva

################################################################################

## Haciendo más eficiente la programación: esto es una pipa

library(magrittr)
library(dplyr)

# Usando R base, con creación de objetos intermedios
datos <- read.table("peces1.txt", header=TRUE)
largo.a_A <- subset(datos$largo.a, datos$grupo == "A")
M <- mean(largo.a_A)
M

# Usando R base, evitando los objetos intermedios
# Notar cómo se debe leer de "adentro" hacia "afuera".
M <- mean(subset(datos$largo.a, datos$grupo == "A"))
M

# Usando la pipa de magrittr %>%
# Seguimos mejor la lógica de un "lenguaje natural"
datos$largo.a %>%
  subset(datos$grupo == "A") %>%
  mean() -> M  # en magrittr los paréntesis que acompañan mean son opcionales
M

# Usando 'pipas nativas' |>
datos$largo.a |>
  subset(datos$grupo == "A") |>
  mean() -> M
M

# Usando dplyr y pipa de magrittr
filter(datos, grupo == "A") %>%
  select(largo.a) %>%
  summarize(mean(largo.a)) -> M
M

####### END #######

