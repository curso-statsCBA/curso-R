Práctico 4. Programación y construcción de funciones
================

## Introducción

Las herramientas de programación en R son usadas con dos fines
principales:  
\* Construir funciones que realicen tareas no contempladas en un paquete
de R, o combinar funciones existentes para dar origen a otras nuevas.  
\* Reducir el número de operaciones que debe realizar el operador
humano, por ejemplo al hacer simulaciones o al aplicar una misma función
a numerosos sets de datos.

 

## Construcción de funciones

Se realiza mediante la función *function*. Esta función requiere de dos
grupos de argumentos: aquellos sobre los cuales esa función actuará (los
datos, parámetros a usar, etc.) que se introducen entre paréntesis, y
las operaciones a realizar con esos argumentos, que se introducen entre
llaves.

### Caso 1

El índice de diversidad de Shannon para un sitio se define por la
fórmula

  
![H' = - \\sum{p\_i
ln(p\_i)}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;H%27%20%3D%20-%20%5Csum%7Bp_i%20ln%28p_i%29%7D
"H' = - \\sum{p_i ln(p_i)}")  

Donde p\_i es la abundancia relativa de cada especie. Primero
mostraremos el desarrollo para calcular *H’* en un conjunto de datos
consistentes en un vector donde cada valor corresponde al número de
individuos de una especie diferente. Luego, construiremos una función
para que la fórmula utilice cualquier set de datos.

``` r
DAT <- c(13, 2, 4, 0, 36, 3, 2, 7, 1, 11)  # datos originales (inventados)
dat <- (DAT[DAT > 0])                      # elimino los 0 (por los log)
N <- sum(dat)                              # total de individuos
p <- dat / N                               # abundancias relativas
H <- -sum(p * log(p))
H                                          # resultado

# construcción de la función 
# notar las similitudes y diferencias con los pasos de arriba.
shann <- function(x) {
  z <- (x[x > 0])
  N <- sum(z)
  p <- z / N
  H <- -sum(p * log(p))
  H                       # la última línea es lo que mostrará la función
}   

# probamos la función con el set de datos original
shann(DAT)

# probamos la función con el otro set de datos
Y <- c(67, 8, 1, 6, 8, 5, 9, 7, 10, 1, 0, 1, 2,0, 2, 1)
shann(Y)
```

 

## Funciones de simulación y muestreo

En muchos casos en estas rutinas usamos datos simulados utilizando, por
ejemplo, *rnorm*. Estas funciones forman una familia donde la primera
letra informa si son una distribución de probabilidad (p), de densidad
(d) o una muestra al azar (r) y la segunda parte informa sobre la
distribución (norm = normal, unif = uniforme, poiss = poisson, etc.).

``` r
# simulación de datos
X <- rnorm(100, mean = 4, sd = 0.5)
hist(X)

# probabilidad de observar el valor z < 5.4 bajo la distribución simulada arriba
pnorm(5.4, mean = 4, sd = 0.5)

# valor del percentil 75 bajo la misma distribución
qnorm(0.75, mean = 4, sd = 0.5)
```

Por otra parte, la función básica de muestreo en R es *sample*. En el
caso de que queramos que otro usuario obtenga exactamente los mismos
resultados ue nosostros en una simulación debemos fijar la “semilla” con
*set.seed()*.

``` r
# Exploración de las utilidades de sample
AA <- c(1:10)
AA

# sólo cambio el orden: permutación
sample(AA, size = 10, replace = FALSE) 
  
# muestreo aleatorio sin remplazo
sample(AA, size = 5, replace = FALSE)

# muestreo aleatorio con remplazo
sample(AA, size = 5, replace = TRUE) 

# idem, pero de igual n que la muestra original, base del bootstrap
sample(AA, size = 10, replace = TRUE) 

?sample 
```

 

## Programación con R

Una ventaja de R es la posibilidad de programar una serie de análisis
para que se ejecuten de manera sucesiva, sin necesidad de muchos
conocimientos técnicos sobre programación, ya que el lenguaje de R es
comparativamente sencillo. Una manera de realizar una serie de funciones
repetitivas es mediante bucles (“loops”). Las principales funciones que
realizan bucles son *for* e *if*. Para evitar realizar bucles pueden
utilizarse las funciones de la serie *apply*.

**for** posee la estructura general: para (cada elemento en un vector)
{realizar x acción}. Si necesitamos que los resultados de la acción sean
guardados, es necesario crear un vector vacío antes de ejecutar la
función y luego rellenarlo con el resultado del bucle.

``` r
Dat <- runif(1000)
vec <- numeric(length = 50)
vec                        # vector vacío antes del loop
for(i in 1:50) {vec[i] <- mean(sample(Dat, 20))} 
vec                        # vector luego de aplicar el loop
plot(density(vec))                    
```

**if** agrega una condición: si (condición) entonces hacer X, en caso
contrario hacer Y.

``` r
# construir una distribución normal truncada
X1 <- rnorm(1000, mean = 3, sd = 5)
X2 <- numeric(1000)
for (i in 1:1000) if (X1[i] < 0) X2[i] <- NA else X2[i] <- X1[i]
hist(X2)
```

**while** crea un bucle que se detiene al alcanzar cierta condición

``` r
i <- 0
while(i < 10) {
  i <- i + 1
}
i
```

**apply** aplica una función a las columnas o filas de una matriz. Por
ejemplo, para obtener la media de cada columna de una matriz de 5
columnas y 40 filas, con números generados al azar desde una
distribución normal. Existen funciones similares como *lapply*,
*tapply*, *sapply* (ver ayuda de la función *apply* para conocer la
utilizadad de cada una).

``` r
m <- rnorm(200)
X <- matrix(m, 40, 5)

apply(X, 2, mean)
apply(X, 1, mean)
```

Siempre que sea posible, es preferible aplicar una función *apply* (o
álgebra sencilla) en vez de un bucle hecho con *for*.

``` r
# ejemplo 1
A <- c(8, 23, 11, 10)
B <- c(15, 3, 2, 1)

#con loop (MAL)
C <- numeric(4)
for(i in 1:4) C[i] <- A[i] + B[i]
C

# vectorizado (BIEN)
C <- A + B
C

# ejemplo 2
m <- rnorm(200)
X <- matrix(m, 40, 5)

# con loop (MAL)
med <- numeric(5)
for(i in 1:5) med[i] <- mean(X[, i])
med

#vectorizado (BIEN)
med <- apply(X, 2, mean)
med
```

 

## Ejercicios (por favor, no use IAs)

1.  Construir una función que calcule el error estándar (SE) de la media
    y calcularlo para todas las variables en el archivo s\_poly.txt
    (guardándolos en un solo vector).

  
![SE = \\frac{s} {\\sqrt{n}}
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;SE%20%3D%20%5Cfrac%7Bs%7D%20%7B%5Csqrt%7Bn%7D%7D%20
"SE = \\frac{s} {\\sqrt{n}} ")  

2.  Utilizando la siguiente tabla de presencia/ausencia de especies en
    dos sitios.

| Especie | A1 | A2 | A3 | A4 | A5 | A6 | A7 | A8 | A9 | A10 | A11 | A12 | A13 | A14 | A15 |
| ------- | -- | -- | -- | -- | -- | -- | -- | -- | -- | --- | --- | --- | --- | --- | --- |
| Sitio 1 | 0  | 0  | 0  | 1  | 1  | 0  | 1  | 1  | 1  | 1   | 0   | 0   | 0   | 1   | 1   |
| Sitio 2 | 1  | 0  | 0  | 0  | 1  | 0  | 1  | 1  | 1  | 0   | 0   | 0   | 0   | 0   | 1   |

Construir una función para calcular el índice de Sørensen para datos de
presencia/ausencia, según la fórmula

  
![Q\_{s} = \\frac{2C} {A +
B}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;Q_%7Bs%7D%20%3D%20%5Cfrac%7B2C%7D%20%7BA%20%2B%20B%7D
"Q_{s} = \\frac{2C} {A + B}")  

Donde A y B es el número de especies de los sitios 1 y 2,
respectivamente, y C es el número de especies compartidas por los sitios
1 y 2. Probar la eficacia de la función con otro set de datos.

3.  Simule una población de 1000 observaciones donde dos variables (X e
    Y) se encuentren relacionadas de forma que ![Y = 0.2 - 1.5 X + 0.85
    X^2 +
    \\epsilon](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;Y%20%3D%200.2%20-%201.5%20X%20%2B%200.85%20X%5E2%20%2B%20%5Cepsilon
    "Y = 0.2 - 1.5 X + 0.85 X^2 + \\epsilon") donde X es una variable de
    distribución normal con media = 2 y varianza = 0.6 y
    ![\\epsilon](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cepsilon
    "\\epsilon") es un error de distribución normal, media = 0 y
    varianza = 0.7. Luego, ajuste un modelo lineal sobre estos datos.
    ¿Qué efecto tiene modificar el valor sd en
    ![\\epsilon](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cepsilon
    "\\epsilon")?"
