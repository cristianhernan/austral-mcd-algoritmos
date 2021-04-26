# figura con valores relativos por cada 100 habitantes
# 2020-03-15  version anterior:2020-04-26 se solucionó el problema de 
#                            regionalizacion de China, Canada y Australia
#                            como contrapate, se consideraron los territorios
#                            de ultramar como pertenecientes a sus metropolis
# 2121/04/08 version actual                
# Autor: GAD 

# Bibliotecas a importar
check_packages <- function(packages) {
  if (all(packages %in% rownames(installed.packages()))) {
    TRUE
  } else{
    cat(
      "Instalar los siguientes packages antes de ejecutar el presente script\n",
      packages[!(packages %in% rownames(installed.packages()))],
      "\n"
    )
  }
}
packages_needed <- c("ggplot2", "ggrepel", "plotly", "sqldf",
                     "lubridate", "htmlwidgets" , "RColorBrewer",
                     "data.table", "readr" )

check_packages(packages_needed)

library(ggplot2)
library(ggrepel)
library(plotly)

## 
## Attaching package: 'plotly'

## The following object is masked from 'package:ggplot2':
## 
##     last_plot

## The following object is masked from 'package:stats':
## 
##     filter

## The following object is masked from 'package:graphics':
## 
##     layout

library(sqldf)

## Loading required package: gsubfn

## Loading required package: proto

## Loading required package: RSQLite

library(lubridate)

## 
## Attaching package: 'lubridate'

## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union

library(htmlwidgets)
library(RColorBrewer)
library(grid)
library(data.table)

## 
## Attaching package: 'data.table'

## The following objects are masked from 'package:lubridate':
## 
##     hour, isoweek, mday, minute, month, quarter, second, wday, week,
##     yday, year

# library(reshape2)

##################################### leer los datos

# time_series_covid19_confirmed_global.csv    este es el archivo que a leer
library(tidyr)
dt <- fread("datasets/Covid19Casos.csv")
dt2 <- select(dt,fallecido ,fecha_fallecimiento)
datos <- subset(dt2, fallecido  == "SI" )
defunciones <- datos %>% group_by(fecha_fallecimiento) %>% summarise(cant = n(fallecido))

head(datos)
COVID_19_h   <- read.csv("datasets/Covid19Casos.csv", sep = ",", header = T)
COVID_19_h

################################### preparo los datos
COVID_19_h$Lat  <- NULL
COVID_19_h$Long <- NULL
COVID_19_h$Province.State <- NULL
#colnames(COVID_19_h)

#CAMBIAMOS EL NOMBRE DE LA COLUMNA
setnames(COVID_19_h , 
         old = c( "Country.Region" ), 
         new = c( "pais"))

####################################   pasar de formato "anchos" a "largo"

#  tres posibilidades para hacer lo mismo, reshape, melt y gather

library(tidyr)
#IMPORTANTE PARA VERTICALIZAR LAS FECHAS, COLS EN FILAS
#DESDE LA 2 hasta la ultima col
COVID_19 <- COVID_19_h %>% gather(fecha, confirmados, 2:ncol(COVID_19_h))
# COVID_19 <-  gather(COVID_19_h, fecha, confirmados, 2:ncol(COVID_19_h))
######################################################################

# COVID_19 <-  gather(COVID_19_h, fecha, confirmados, 2:ncol(COVID_19_h))
######################################################################



# -------------------------------------------------------------------

#  el primer agrupamiento es para sumarizar los casos de Australia, Canada y China que
#  vienen separadas por region. Otros casos son los territorios de ultramar de UK, 
#  Francia, Dinamrca y Holanda que son de muy pocos habitante / casos.
#  nada de lo anterior influye en el ranking  de los 25 peores casos.
#  otra idea seria no contar los casos  de los territorios de Ultra mar por pertenecer
#  a otras regiones

COVID_19 <- COVID_19 %>% group_by(pais,fecha) %>% summarise(confirmados = sum(confirmados))
head(COVID_19)

confirmados_por_pais2 <- COVID_19 %>% group_by(pais) %>% summarise(confirmados = max(confirmados))
# el siguiente agrupamiento es para quedarde con el max de la variable acumulativa
confirmados_por_pais <- COVID_19 %>% group_by(pais) %>% summarise(confirmados = max(confirmados))
# sum(confirmados_por_pais$confirmados)

#   normalizo nombres de paises para compatibilidad de archivo habitantes
library(tidyverse) 

#aca se cambian registros para que coincidan con otro dataset
#la \\ es para que strreplace me reconozca los espacios y caracteres especiales
library(dplyr)
confirmados_por_pais <- confirmados_por_pais %>% 
  mutate(pais = str_replace(pais,  "Korea\\, South"      , "South Korea")) %>%
  mutate(pais = str_replace(pais,  "Congo \\(Kinshasa\\)", "Democratic Republic of the Congo"))%>%
  mutate(pais = str_replace(pais,  "Taiwan\\*"           , "Taiwan")) %>%
  mutate(pais = str_replace(pais,  "US"                  , "United States of America")) %>%
  mutate(pais = str_replace(pais,  "Brunei"              , "Brunei Darussalam"))       %>%
  mutate(pais = str_replace(pais,  "Cote d'Ivoire"       , "Costa de Marfil"))         %>%
  mutate(pais = str_replace(pais,  "Holy See"            , "Vatican City"))            %>%
  mutate(pais = str_replace(pais,  "Czechia"             , "Czech Republic"))          %>%
  mutate(pais = str_replace(pais,  "Diamond Princess"    , "crucero Diamond Princess"))%>%
  mutate(pais = str_replace(pais,  "MS Zaandam"          , "crucero MS Zaandam"))      %>%
  mutate(pais = str_replace(pais,  "Timor-Leste"         , "East Timor"))   

# idea:  unir todos los cruceros y unirlo como un nuevo pais
#--------------------------------------------------- Leo archivo de poblaciones
library(readr)

habitantes <- read_delim("datasets/WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.csv", 
                         ";", escape_double = FALSE, col_types = cols(cantidad = col_number()), 
                         locale = locale(grouping_mark = "", encoding = "WINDOWS-1252"), 
                         trim_ws = TRUE)

#############################################   preparo los datos de poblaciones
colnames(habitantes)

habitantes <- select(habitantes, pais, codigo , cantidad )

setnames(habitantes, "cantidad", "cantHabitantes")

#hace un merge entre dos datasets, si no le pongo col, une por la que encuentra igual
datos <- merge(habitantes, confirmados_por_pais)

# -- creo variable nueva con la tasa por cada 100 habitantes
options(scipen = 6) #para evitar notacion cientifica
datos$porCien <- datos$confirmados * 100 / datos$cantHabitantes

#  --- veo si hay algun pais que no se junta por diferir su  nombre o por no ser pais (buques en alta mar)
#  -- quedo afuera Cruise Ship por tratarse de casos controlados   asumo que es correcto 
# -- no tenerlo en cuenta  a estos casos especiales

#-------------------------- aqui trabajo  con SQL
no_estan <- sqldf("select c.pais
                     from confirmados_por_pais c 
                     where not exists (select '1'
                                      from datos d
                                      where c.pais = d.pais)")
no_estan


# -----------------------------------------------------------------------------------

# -- ordenamos
datos_t <- arrange(datos, desc(datos$porCien) )

#######
#   grabar los datos en un archivo .csv
write.csv2(datos, "confirmadosporcien.csv",  row.names = FALSE, fileEncoding = "UTF-8")


# para graficar los mas importantes
datos <- sqldf( "select *  
                   from datos_t
                   LIMIT 25 ")

###############################################
# genero  figura dinamica

g1 <- ggplot(datos ,aes(x = reorder(pais, porCien) , y = porCien, label = confirmados ) ) +
  geom_segment(size = 0.08, aes(xend = pais, yend=0))+
  coord_flip() +    # para girar el  grafico
  geom_point( size=1, color="orange") +
  ggtitle(paste0("COVID_19 - Confirmados por cada 100 habitantes - ", today()) ) +
  theme(plot.title = element_text(lineheight = 1,face ='bold'))   +
  ylab("confirmados por cada 100 habitantes") +
  xlab("") +
  labs(caption = "\nFuente: The Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = round(porCien,1)), position = position_stack(vjust = .5))

g1 <- ggplotly(g1, tooltip = c("confirmados")) %>%
  layout(legend = list(
    orientation = "h",
    x = 0.7,
    y = 0
  )
  )
g1



