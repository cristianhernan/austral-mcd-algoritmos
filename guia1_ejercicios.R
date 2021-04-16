#1- sobre el data frame.

#se concatena el nombre del file al directorio y luego se carga como csv
dir <- 'datasets/'
aux <- paste(dir,'WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.csv',sep="")
df <- read.csv(aux,sep=';',header=T)

#Devuelve el numero de columna.
ncol(df)

#Devuelve el numero de filas
nrow(df)

#Nos muestra los primeros N registro, en esta caso los primeros 7
head(df,7)

#retorna los nombres de las columnas
colnames(df)

#nos da un resumen descriptivo y estadistico de cada columna del dataframe
summary(df)

#nos da un resumen descriptivo y estadistico de una columna particular
summary(df$codigo)

library(dplyr)
select(df,c1,c2,c3)

