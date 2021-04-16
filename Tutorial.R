## Para obtener informacion sobre una funcion
help(solve)
?solve

## Para salir de entorno de trabajo
q()

## Para asignar valores a una variable
x <- 5

## Para craer un vector 
v <- c(1, 2, 3, 4, 5)
b <- c(TRUE, FALSE, TRUE, FALSE, TRUE)
c <- c("a", "b", "c", "d", "e")

assign("v", c(1, 2, 3, 4, 5))
assign("b", c(TRUE, FALSE, TRUE, FALSE, TRUE))
assign("c", c("a", "b", "c", "d", "e"))

## Funciones sobre vectores
min(v) ## Retorna el valor mimino del vector
max(v) ## Retorna el valor maximo del vector
length(v) ## Retorna el tamaño del vector
mean(v) ## Retorna la media de los elementos del vector
sum(v) ## Retorna la suma de los elementos del vector
prod(v) ## Retorna el producto de los elementos del vector
var(v) ## Retorna la cuasi-varianza de los elementos del vector
sort(v) ## Retorna un nuevo vector ordenado
mode(v) ## Retorna el modo del vector (tipo de dato)

## Secuencias
seq(from=1, to=10, by=.5) ## Retorna un vector con la secuencia de valores según la definicion
1:5 ## Retorna una secuencia de valores entre ambos numeros de acuerdo a su progreción natural
rep(v, times=3) ## Repite el vector dado tantas veces como se defina

## Atributos de los objetos
dim(v) ## Dimension del objeto
attr(v, "dim") ## Dimension del objeto
attr(v, "names") ## Nombre de cada una de las columnas/filas

## Vectores indice
v[1] ## Retorna el elemento 1 del vector
v[c(1,3)] ## Retorna los elementos 1 y 3 del vector
v[c(TRUE, FALSE, FALSE, TRUE)] ## Retorna solo los elementos verdaderos del vector
v[v > 2] ## Retorna los elementos mayores a 2 del vector

## Nombre de los indices
v <- c(20,22,28,24)
names(v) <- list("Juan", "Pedro", "Ernesto", "Julio") ## Define el nombre a las columnas
v["Juan"] ## Retorna el valor para la columna "Juan"

## Valores no existentes (NA)
n <- c(1,2,NA,4,5,NA) ## Retorna un vector de 6 elementos con 2 valores desconocidos
is.na(NA) ## Retorna verdadero si el valor es NA
v[is.na(v)] ## Retorna solo los valores NA
v[!is.na(v)] ## Retorna los valores no NA
v[is.na(v)] <- 0 ## Asigna 0 a los valores NA

## Factores
state <- c("tas", "sa", "qld", "nsw", "nsw", "nt", "wa", "wa", "qld", "vic", "nsw", "vic", "qld", "qld", "sa", "tas",
            "sa",  "nt",  "wa",  "vic", "qld", "nsw", "nsw", "wa",
            "sa",  "act", "nsw", "vic", "vic", "act")
stateFactor <- factor(state) ## Define los factores (valores nominales) del vector estado
levels(stateFactor) ## Retorna los niveles (valores nominales) del factor

incomes <- c(60, 49, 40, 61, 64, 60, 59, 54, 62, 69, 70, 42, 56, 61, 61, 61, 58, 51, 48, 65, 49, 49, 41, 48, 52, 46,
             59, 46, 58, 43)
tapply(incomes, stateFactor, mean) ## Aplica una función dada a cada grupo tapply(vector, factor, función)

## Matrices
a <- array(c(1,2,3,4), dim=c(2,2)) ## Crea una matriz a partir del vector dado con las dimenciones definidas [array(vector, dim=dimensiones)]
a <- array(c(1,2,3,4), dim=c(2,2), dimnames = list(c("j", "k"), c("l","m"))) ## Matrix con nombre en las dimensiones
m <- matrix(c(1,2,3,4), c(2,2)) ## Crea una matriz a partir del vector dado con las dimenciones definidas [matrix(vector, dimensiones)]
m <- matrix(c(1,2,3,4), nrow=2, ncol=2) ## Crea una matriz a partir del vector dad con las dimensiones definidas.
m[1,2] ## Retorna el elemento de la posicion fila 1 columna 2
m[1,] ## Retorna todos los elementos de la fila 1
m[,1] ## Retorna todos los elementos de la columna 1
m[1,1] <- 9 ## Asigna 9 a elemento de la fila 1 columna 1
m[1,] <- 0 ## Assigna 0 a todos los elementos de la fila 1
cbind(c(1,2,3,4), c(9,8,7,6)) ## Crea una matriz combinando los vectores en columnas (cada vector es una columna)
rbind(c(1,2,3,4), c(9,8,7,6)) ## Crea una matriz combinando los vectores en filas (cada vector es una fila)

rownames(m) <- list("Juan", "Pedro") ## Asigna nombre a las filas
colnames(m) <- list("Edad", "Hijos") ## Asigna nombre a las columnas
m["Juan", "Edad"] ## Retorna el valor de elemento definido por la fila "Juan" y la columna "Edad"

## Attributos sobre matices
nrow(m) ## Cantidad de filas
ncol(m) ## Cantidad de columnas
dim(m) ## Dimension de la matriz
length(m) ## Largo de la matriz (cantidad de elementos)
mode(m) ## Tipo de dato de la matriz

## Operaciones sobre matrices
a <- matrix(c(1,2,3,4), c(2,2))
b <- matrix(c(9,8,7,6), c(2,2))
a %o% b ## Retorna el producto exterior de las matrices
t(a) ## Retorna la traspuesta de la matriz
a * b ## Retorna una matriz con el producto elemento a elemento
a %*% b ## Retorna el producto matricial
diag(a) ## Retorna una matriz con la diagonal de la matriz original
eigen(a) ## Retorna los autovalores y autovectores de la matriz
det(a) ## Retorna el determinante de la matriz
solve(a) ## Retorna la inversa de la matriz

## Listas
l <- list(1,2,"a","b") ## Retorna una lista con los elementos dados
l <- list(names=c("Juan", "Pedro"), age=c(10,11)) ## Retorna una lista con los elmentos dados, asigando nombre a cada uno de los componentes
l[[1]] ## Retorna el primer componente de la lista
l[[1]][1] ## Retorna el primer componente de la lista y sobre este el primer elemento (se conoce que componente es un vector)
l[["names"]] ## Retorna el componente "names" 
l[["names"]][1] ## Retorna el componente "names" y sobre este el primer elemento (se conoce que el componente es un vector)
l$names ## Retorna el componente "names"
l$names[1] ## Retorna el componente "names" y sobre este el primer elemento (se conoce que el componente es un vector)

## Hojas de datos (data frames - data.frame)
state <- c("tas", "sa", "qld", "nsw", "nsw", "nt", "wa", "wa", "qld", "vic", "nsw", "vic", "qld", "qld", "sa", "tas",
           "sa",  "nt",  "wa",  "vic", "qld", "nsw", "nsw", "wa",
           "sa",  "act", "nsw", "vic", "vic", "act")
stateFactor <- factor(state)
incomes <- c(60, 49, 40, 61, 64, 60, 59, 54, 62, 69, 70, 42, 56, 61, 61, 61, 58, 51, 48, 65, 49, 49, 41, 48, 52, 46,
             59, 46, 58, 43)
incomesFactor <- factor(cut(incomes, breaks=35+10*(0:7)))
dataFrame <- data.frame(dom=state, bot=incomes, dis=incomesFactor)

## Funciones e instrucciones
sumVector1 <- function(v) {
  if (length(v) == 0) return(0)
  if (length(v) == 1) return(v[1])
  return (v[1] + sumVector1(v[2:length(v)]))
}

sumVector2 <- function(v) {
  r <- 0
  for (i in v) r <- r + i
  return(r)
}

sumVector3 <- function(v) {
  r <- 0
  i <- 1
  while (i <= length(v)) {
    r <- r + v[i]
    i <- i + 1
  }
  return(r)
}

sumVector4 <- function(v) {
  r <- 0
  i <- 1
  repeat {
    if (i > length(v)) break;
    r <- r + v[i]
    i <- i + 1
  }
  return(r)
}

