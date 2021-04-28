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
#

#
url <- "https://cdn.buenosaires.gob.ar/datosabiertos/datasets/salud/casos-covid-19/"
archivo <- paste(url,"casos_covid19.csv",sep="")
library(tidyr)
covid <- fread(archivo)
covid <- select(covid,fallecido ,fecha_fallecimiento)
covid <- subset(covid, fallecido  == "si" )
covid$fecha_fallecimiento <- as.Date(covid$fecha_fallecimiento, format = "%d%b%Y")

library(dplyr)

covid_muertes <- covid %>% group_by(fecha_fallecimiento) %>% summarise(fallecimientos = n())

summary(covid_muertes)


mobility_url <- "https://covid19-static.cdn-apple.com/covid19-mobility-data/2106HotfixDev21/v3/en-us/"
aux        <- paste(mobility_url,"applemobilitytrends-", sep = "")
filedate <- paste(aux,as.character(Sys.Date()-1),".csv",sep = "")
mobility   <- read.csv(filedate, sep = ",", header = T)

head(mobility)


#el 7 es desde donde tnego que verticalizar, todas las fechas
mobilityv   <- mobility %>% gather(fecha, tasa    , 7:ncol(mobility))
mobilityv$fecha <- as.Date(mobilityv$fecha, format = "X%Y.%m.%d")

#  si queremos ver otro pais modificamos aqui
pais = "Buenos Aires"
trans = "walking"
mobilityv <- subset(mobilityv, region == pais & transportation_type == trans)
#limpio las columnas que no quiero usar
mobility <-  select( mobilityv, fecha, tasa)

mobility[mobility$fecha=='2020-05-11','tasa'] <-  mean(mobility[(mobility$fecha >= '2020-05-05' & mobility$fecha <= '2020-05-10'),'tasa' ])
mobility[mobility$fecha=='2020-05-12','tasa'] <-  mean(mobility[(mobility$fecha >= '2020-05-05' & mobility$fecha <= '2020-05-10'),'tasa' ])
mobility[mobility$fecha=='2021-03-12','tasa'] <-  mean(mobility[(mobility$fecha >= '2021-03-13' & mobility$fecha <= '2021-05-16'),'tasa' ])


#mean(mobility[(mobility$fecha >= '2021-03-13' & mobility$fecha <= '2021-03-15'),'tasa' ])

setnames(covid_muertes , old = c( "fecha_fallecimiento" ), new = c( "fecha"))

dt <- merge(covid_muertes,mobility)

head(dt)

cor(x=dt$tasa, y=dt$fallecimientos)

with(dt, plot(x=tasa, y=fallecimientos, pch=20, col='blue',
                 xlab='tasa', las=1,
                 ylab='fallecimientos'))


g1 <- ggplot(dt, aes(x = fecha, y = tasa) ) +
  geom_line(size = 0.6) +
  geom_line(linetype = "dashed") +
  
  ggtitle(paste("COVID_19 - Movilidad en ",'buenos aires',sep = "")) +
  
  scale_x_date(date_breaks = "7 day", date_labels =  "%d %b") +
  #scale_y_continuous(limits = c(0, 100), breaks = seq(1, 10, 1)) +
  theme(plot.title = element_text(lineheight = 1,face ='bold'))   +
  ylab("tasa de movilidad") +
  xlab("") +
  labs(caption = "Fuente de los datos: apple.com/covid19/mobility") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7)) 
g1

g2 <- ggplot(dt, aes(x = fecha, y = fallecimientos) ) +
  geom_line(size = 0.6) +
  geom_line(linetype = "dashed") +
  
  ggtitle(paste("COVID_19 - fallecimientos en ",'buenos aires',sep = "")) +
  
  scale_x_date(date_breaks = "10 day", date_labels =  "%d %b") +
  #scale_y_continuous(limits = c(0, 100), breaks = seq(1, 10, 1)) +
  theme(plot.title = element_text(lineheight = 1,face ='bold'))   +
  ylab("tasa de fallecimientos") +
  xlab("") +
  labs(caption = "Fuente de los datos: apple.com/covid19/mobility") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7)) 
g2
