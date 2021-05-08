# agrupamiento  de paises por similitudes en sus st 
# primero visualizamos  la st de algunos paises  antes de agruparlos
# 2020-05-9  version actual:   2021-04-18
# GAD 
# aprendemos a leer desde una URL, setnames, select, 
# group_by(), summarise_all(), la funcion sapply


library(reshape2)
library(data.table) # setnames

############################ leer los datos

# time_series_covid19_deaths_global.csv    este es el archivo que a leer

URL           <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"
URL_muertes   <- paste(URL,"time_series_covid19_deaths_global.csv", sep = "")
COVID_19_h    <- read.csv(URL_muertes, sep = ",", header = T)
################################### preparo los datos
# eliminamos  columnas no usadas
library(dplyr)
# colnames(COVID_19_h)
COVID_19_h <- select(COVID_19_h, -c("Lat", "Long", "Province.State")   ) #el -c es para que quiete esas columnas

# cambio de nombres
setnames(COVID_19_h , 
         old = c( "Country.Region" ), 
         new = c( "pais"))

# agrupamos
COVID_19_h_g      <- COVID_19_h %>% group_by(pais) %>% summarise_all(~ sum(.)) #el chirimbolo de la ñ es parte de la sintacsis 

# traspuesta
casos_trasp <- t(COVID_19_h_g)# traspone filas por columnas
#el tema es que inventa columnas, en este caso los paises los pone como la primer fila


# renombro  columnas
colnames(casos_trasp ) <- casos_trasp[1,] # la primer fila pasa a ser el nombre de la columna
casos_trasp <- casos_trasp[-1,]           # elimina primer fila
###

#   seleccionamos algunos paises 
casos_trasp <- casos_trasp[,c("Argentina", "Chile", "Brazil", "Peru",  "Canada", "Ecuador" , "US", "Spain")]


plot.ts(casos_trasp)

#############

casos_trasp_df <- as.data.frame(casos_trasp)

# pasamos a tipo numerico
casos_trasp_df[,1:ncol(casos_trasp_df)] <- sapply(casos_trasp_df[,1:ncol(casos_trasp_df)],as.numeric)

# creamos las nuevas variables
for(i in (1 : (nrow(casos_trasp_df) - 1)))
{ 
  #es el log de una taza, es un coheficiente que me dice la taza de crecimiento
  #el log se usa para poder comparar, al usar log usas una escala que sea comparable
  #es el log de la taza, seria el log de los casos nuevos
  casos_trasp_df$Arg[i+1] <- log(casos_trasp_df$Argentina[i+1] / casos_trasp_df$Argentina[i]) 
  casos_trasp_df$Chi[i+1] <- log(casos_trasp_df$Chile[i+1]     / casos_trasp_df$Chile[i])
  casos_trasp_df$Bra[i+1] <- log(casos_trasp_df$Brazil[i+1]    / casos_trasp_df$Brazil[i])
  casos_trasp_df$Per[i+1] <- log(casos_trasp_df$Peru[i+1]      / casos_trasp_df$Peru[i])
  casos_trasp_df$Can[i+1] <- log(casos_trasp_df$Canada[i+1]    / casos_trasp_df$Canada[i])
  casos_trasp_df$Ecu[i+1] <- log(casos_trasp_df$Ecuador[i+1]   / casos_trasp_df$Ecuador[i])
  casos_trasp_df$Spa[i+1] <- log(casos_trasp_df$Spain[i+1]     /  casos_trasp_df$Spain[i])
}


#  eliminamos col y filas 
casos_trasp_df <- casos_trasp_df[,-(1:8)]  # elimino las primeras col

casos_trasp_df <- casos_trasp_df[-(1:61),] #entre el 1 y el 61 habia muchos nan e inf

# vemos las st segun su tasa de crecimiento
plot.ts(casos_trasp_df,  axes = TRUE, ann = TRUE, frame.plot = TRUE,  main = "variación diaria de muertes por covid-19")

plot.ts(casos_trasp_df, plot.type = 'single', col = 1:6)

casos <- t(casos_trasp_df )


##########    fin de preparacion de los datos

# agrupamos por similitudes
library(dtw)

modelo_dtw <- hclust(dist(casos, method = "dtw"))  # method = "dtw"
plot(modelo_dtw, main = "variacion diaria de muertes por covid-19")
rect.hclust(modelo_dtw, k = 4, border =  "green")
