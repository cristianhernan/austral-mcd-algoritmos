#datos estadisticos basicos de un data frame
library(dplyr)
library(readr)
#deshabilitar la notacion cientifica
options(scipen=999)


#cargamos datos del archivos
datos <- read_delim("datasets/datos_clase2.csv", 
                   ";", escape_double = FALSE, locale = locale(grouping_mark = "", 
                                                              encoding = "WINDOWS-1252"), trim_ws = TRUE)

#para escribir un dataframe
#es csv2 es para usar configuracion latina, con separador el ;
write.csv2(datos,"datos_grabados.csv",row.names=FALSE,fileEncoding = "UTF-8")

#cambiamos el tipo de pais a factor para evitar que sea char
datos$pais <- as.factor(datos$pais)

#mostramos datos estadisticos basicos de una columna
summary(datos$cantHabitantes)
#aca mostramos estadisticos de todo el datafrae
summary(datos)

#  sumarizamos los datos de nuestro Data Frame
datos %>%
  summarise(poblacion_mundial  = sum(cantHabitantes) ,
            avg_cantidad       = mean(cantHabitantes),
            min_cantidad       = min(cantHabitantes),
            max_cantidad       = max(cantHabitantes),
            cant_paises        = n_distinct(pais),
            avg_casos          = mean(casos),
            #se crea una nueva variable con una cuenta
            casos_por_cien     = 100  * sum(casos)/sum(cantHabitantes),
            sum_casos          = sum(casos),
            ds                 = sd(cantHabitantes))


#  completar con la funcion que uno quiera

library(skimr)
#ponemos esl ancho de la pantalla
options(width = 140)

res_skim <- skim(datos)

###############################
library(pastecs)
res_stat <- stat.desc(datos)

##################################

library(Hmisc)
describe(datos)

################################## SUPER EXPORTADOR DE DATOS
library(tidyverse)
library(DT)

datos %>%
  datatable(extensions = 'Buttons',
            filter     = "top",
            class      = "display nowrap compact",
            caption    = htmltools::tags$caption(
              style      = 'caption-side: bottom; text-align: center;',
              'Table 1: ', htmltools::em('estadisticas simples sobre los confirmado')),
            options = list(dom = 'Blfrtip',
                           buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                           lengthMenu = list(c(10,25,50,-1),
                                             c(10,25,50,"All"))) )

####
### visualizar valores nulls
library(naniar)
vis_miss(datos)

