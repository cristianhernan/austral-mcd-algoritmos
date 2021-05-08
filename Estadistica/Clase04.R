##########################################################
#                   ESTAD?STICA                          #
#          Prof. MASCI - SPERANZA - DEL ROSSO            #
#	            MAESTR?A EN CIENCIA DE DATOS               #
#                FACULTAD DE INGENIER?A                  #
#                 UNIVERSIDAD AUSTRAL                    #
##########################################################

## Asignatura: ESTAD?STICA
## Docentes: Rodrigo Del Rosso - Martin Masci - Mauro Speranza

rm(list = ls()) ## LIMPIAR LA CONSOLA
.rs.restartR()
gc()

################################
####### SETEO DE CARPETA #######
################################

#setwd("....")  ### Setear ruta de trabajo

###########################
####### DESCRIPTIVA #######
###########################

# Basado en https://www.princeton.edu/~otorres/sessions/
# http://www.princeton.edu/~otorres/sessions/s2r.pdf

# Leemos el archivo

download.file(url = "http://dss.princeton.edu/training/students.xls",
              destfile = "students.xls",
              method = "auto",
              quiet = FALSE,
              mode = "wb", # esto es para que sea compatible con windows
              cacheOK = TRUE)

## Exhibir archivos en el directorio de trabajo
dir()

## Abrir el archivo con Excel
file.show("students.xls")

## Importar el dataset como xls
library(readxl)
datos <- read_excel("datasets/students.xls")

## Explorando los datos

summary(datos) # Suministra estad?stica descriptiva b?sica

edit(datos) # Abre el editor de datos
fix(datos)

str(datos) # Suministra la estructura de los datos

names(datos) # Lista las variables

attach(datos) ## quita la dependencia al dataset
detach(datos)

head(datos) # Primeras 6 filas

head(datos, n = 10) # Primeras 10 filas

head(datos, n= -10) # ?ltimas 10 filas

tail(datos) # ?ltimas 6 filas

tail(datos, n = 10) # ?ltimas 10 filas

tail(datos, n = -10) # Primeros 10 filas

datos[1:10, ] # Primeras 10 filas

#nth(datos,10) se puede usar tambien con nth

is.data.frame(datos)
is.vector(datos)

datos[1:10,"First Name"] # Primeras 10 filas de las 3 primeras variables

#toma los registros del 1 al 14 tomados de 2, solo columnas 4 y 5
datos[seq(1,14,2),c(4,5)]

# Informaci?n Faltante (Missing)

rowSums(is.na(datos)) # Cantidad de Missing por Filas

colSums(is.na(datos)) # Cantidad de Missing por Columnas

# Ejemplo de Conversi?n a Datos Faltantes

datos[datos$Age == 30,"Age"] <- NA

# Listar filas que tienen valores faltantes
datoscompletos = datos[complete.cases(datos),]
datosconna = datos[!complete.cases(datos),]

# Crear un nuevo dataset sin datos faltantes
datos1 <- na.omit(datos)

# Reemplazar un valor
datos[datos$SAT == 1787,"SAT"] <- 1800

datos[datos$Country == "Bulgaria","Country"] <- "US"

# Renombrar Variables

# Interactivamente
fix(datos)

# Comando b?sico "names"
names(datos)[3] <- "First"
names(datos)[2] <- "Last"

# Mediante paquete reshape
library(reshape)

names(datos)

reshape::rename(datos,c('First Name' = "First"))

datos <- rename(datos, c('First Name' = "First"))

datos <- rename(datos, c('Student Status' = "Status"))

datos <- rename(datos, c('Average score (grade)' = "Score"))

datos <- rename(datos, c('Height (in)' = "Height"))

datos <- rename(datos, c('Newspaper readership (times/wk)' = "Read"))

## para filtrar el ?ltimo campo mediante la utilizaci?n de length
datos <- datos[,-length(datos)]

names(datos)

datos$First
# First
# attach(datos)
# detach(datos)

# Crear secuencia de n?meros id
datos$id <- seq(dim(datos)[1])

# Crear una variable con el n?mero total de observaciones
datos$total <- dim(datos)[1]

# Recoding variables

library(car)
#esto genera variables categoricas. por ejemplo en este caso, me crea grupos
datos$Age.rec <- recode(datos$Age, "18:19='18to19';20:29='20to29';30:39='30to39'")
datos$Age.rec <- as.factor(datos$Age.rec)

# Estad?stica Descriptiva mediante paquete "pastecs"-
library(pastecs)
stat.desc(datos)

# Exportar tabla como csv
tabla <- stat.desc(datos[,c("Age","SAT","Score")])
write.csv2(x = tabla,file = "tabla descriptiva.csv")

stat.desc(datos[,c("Age","SAT","Score")], 
          basic = TRUE, 
          desc = TRUE, 
          norm = TRUE, 
          p = 0.975)

stat.desc(datos[10:14], 
          basic = TRUE, 
          desc = TRUE, 
          norm = TRUE, 
          p = 0.95)

# Estad?stica Descriptiva

fBasics::basicStats(datos$Age)

mean(datos$Age)

mean(datos$Age, na.rm = T)

mean(datos$SAT)

with(datos, mean(SAT))

median(datos$SAT)

var(datos$SAT)

sd(datos$SAT)

max(datos$SAT)

min(datos$SAT)

range(datos$SAT)
diff(range(datos$SAT))

quantile(datos$SAT)

quantile(datos$SAT, type=6) #cambia el algoritmo

quantile(datos$SAT, probs = c(0.3,0.6,0.9))

diff(quantile(datos$SAT, probs = c(0.1,0.9))) # rango interdecil

IQR(datos$SAT) #rango intercuartil


fivenum(datos$SAT) 
x11()
boxplot(datos$SAT)
#lina negre es la mediana, las lineas del cuadrado es el 50%
qqnorm(datos$SAT)
qqline(datos$SAT,col="red")


length(datos$SAT)

length(datos)

datos$SAT[which.max(datos$SAT)]

datos$SAT[which.min(datos$SAT)]

## Devuelve la edad para el valor m?ximo de SAT
datos[which.max(datos$SAT),"Age"]

# Modo por Frecuencias

table(datos$Country)

max(table(datos$Country))

names(sort(-table(datos$Country)))[1]

# Estad?stica Descriptiva mediante grupos usando tapply

mean <- tapply(datos$SAT,datos$Gender, mean)

sd <- tapply(datos$SAT,datos$Gender, sd)

median <- tapply(datos$SAT,datos$Gender, median)

max <- tapply(datos$SAT,datos$Gender, max)

cbind(mean, median, sd, max)

round(cbind(mean, median, sd, max),digits=1)

t1 <- round(cbind(mean, median, sd, max),digits=1)

t1

# Estad?stica Descriptiva por grupos mediante aggregate

aggregate(datos[c("Age","SAT")],by = list(sex = datos$Gender), mean, na.rm=TRUE)

aggregate(datos[c("Age","SAT")],datos["Gender"], mean, na.rm=TRUE)

aggregate(datos,by=list(sex=datos$Gender), mean, na.rm=TRUE)

aggregate(datos,
          by=list(sex = datos$Gender, 
                  major = datos$Major, 
                  status = datos$Status), 
          mean,
          na.rm=TRUE)

aggregate(datos$SAT,
          by=list(sex=datos$Gender, 
                  major=datos$Major, 
                  status=datos$Status), 
          mean,
          na.rm=TRUE)

aggregate(datos[c("SAT")],
          by=list(sex=datos$Gender, 
                  major=datos$Major, 
                  status=datos$Status),
          mean, 
          na.rm=TRUE)

# Histogramas

library(car)

#trae un data set de ejemplos
head(Prestige)

hist(Prestige$income,breaks = 10)
boxplot(Prestige$income)


hist(Prestige$income, 
     col=c(rainbow(10)), 
     breaks = "FD", 
     main = "Histograma")

with(Prestige, hist(income))
par(mfrow=c(1, 1))
with(Prestige, hist(income, breaks="FD", col="green")
     lines(density(income),lw=2),
     lines(density(income),adjust=0.5),lw=1)
box()

hist(Prestige$income, breaks="FD")

# Histogramas Condicionales

jpeg(filename = "Prueba.jpg")
par(mfrow=c(1, 2))
hist(datos$SAT[datos$Gender=="Female"], breaks="FD", main="Female", xlab="SAT",col="pink")
hist(datos$SAT[datos$Gender=="Male"], breaks="FD", main="Male", xlab="SAT", col="blue")
dev.off()

x11()
layout(matrix(1:4,2,2,byrow = T))
hist(datos$SAT[datos$Gender=="Female"], breaks="FD", main="Female", xlab="SAT",col="green")
hist(datos$SAT[datos$Gender=="Male"], breaks="FD", main="Male", xlab="SAT", col="green")
hist(datos$SAT[datos$Gender=="Female"], breaks="FD", main="Female", xlab="SAT",col="green")
hist(datos$SAT[datos$Gender=="Male"], breaks="FD", main="Male", xlab="SAT", col="green")


# {} indican un comando compuesto que 
# permite varios comandos con el comando 'with'

par(mfrow=c(1, 1))

with(Prestige, {
  hist(income, breaks="FD", freq=FALSE, col="green")
  lines(density(income), lwd = 2)
  lines(density(income, adjust = 0.5),lwd = 1)
  rug(income)
})

# Histogramas Superpuesto

x11()
hist(datos$SAT, 
     breaks="FD", 
     col="green")

hist(datos$SAT[datos$Gender=="Male"], 
     breaks="FD", 
     col="gray", 
     add=TRUE)

legend("topright", c("Female","Male"), fill=c("green","gray"))

# Chequear
satgender <- table(datos$SAT,datos$Gender)
satgender


#rriskDistributions
#la función es fit.cont


###############################
####### OBJETOS CREADOS #######
###############################

rm(list=ls(all=TRUE)) ## elimina todos los objetos creados
