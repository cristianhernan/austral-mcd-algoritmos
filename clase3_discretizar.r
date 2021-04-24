# discretizar la variable cantHabitantes
# 2020-05-9  version actual:   2020-05-09
# GAD 
library(dplyr) 
## 
## Attaching package: 'dplyr'

## The following objects are masked from 'package:stats':
## 
##     filter, lag

## The following objects are masked from 'package:base':
## 
##    
library(readr)

# trabajamos  con el DF "datos"  del script de la primer clase "01_ranking"  

datos <- read_delim("datasets/datos_01.csv", ";", 
                    escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                           grouping_mark = ""), trim_ws = TRUE)
head(datos)

## 
## -- Column specification --------------------------------------------------------
## cols(
##   pais = col_character(),
##   codigo = col_double(),
##   cantHabitantes = col_double(),
##   confirmados = col_double(),
##   porCien = col_double()
## )
datos$hab_discreto <- 1

datos <- datos  %>%  
  mutate(hab_discreto  = case_when(.$cantHabitantes <= 100000 ~ "muy chico",
                                   .$cantHabitantes <= 1000000 ~ "chico",
                                   .$cantHabitantes <= 10000000 ~ "medio",
                                   .$cantHabitantes <= 100000000 ~ "grande",
                                   TRUE ~ "muy grande"
  )) 
head(datos)
