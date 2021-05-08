library(grid)
library(data.table)
library(tidyr)
library(dplyr)
library(plotly)


url <- "https://cdn.buenosaires.gob.ar/datosabiertos/datasets/salud/casos-covid-19/"
archivo <- paste(url,"casos_covid19.csv",sep="")
covid <- fread(archivo)
covid <- subset(covid, fallecido  == "si" )
covid <- select(covid,fallecido ,fecha_fallecimiento)
covid$fecha_fallecimiento <- as.Date(covid$fecha_fallecimiento, format = "%d%b%Y")

covid_muertes <- covid %>% group_by(fecha_fallecimiento) %>% summarise(fallecimientos = n())

mobility_url <- "https://covid19-static.cdn-apple.com/covid19-mobility-data/2107HotfixDev10/v3/en-us/"
aux        <- paste(mobility_url,"applemobilitytrends-", sep = "")
filedate <- paste(aux,as.character(Sys.Date()-1),".csv",sep = "")
mobility   <- read.csv(filedate, sep = ",", header = T)

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

#setnames(covid_muertes , old = c( "fecha_fallecimiento" ), new = c( "fecha"))

dt <- merge(covid_muertes,mobility)

#cor(x=dt$tasa, y=dt$fallecimientos)

#with(dt, plot(x=tasa, y=fallecimientos, pch=20, col='blue',
#                 xlab='tasa', las=1,
#                 ylab='fallecimientos'))



fig <- plot_ly(data=dt, x=dt$fechal)
fig <- fig %>% add_trace(y = dt$fallecimientos, name = 'fallecimientos',mode = 'lines')
fig <- fig %>% add_trace(y = ~dt$tasa, name = 'mobilidad', mode = 'lines')
fig <- fig %>% layout(
  title = "COVID19 tasa de movilidad vs fallecimientos", 
  legend = list(x = 0, y = 1),
  margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4)
)
fig



