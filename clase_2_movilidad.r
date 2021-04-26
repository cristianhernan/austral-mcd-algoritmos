# graficamos la movilidad  en la ciudad de Buenos Aires
# creado: 2020-04-25   v.  2020-04-27 
# ultima modificacion: 2020-08-22 - se modifica la lectura del input
# Autor: GAD
# archivo de input se puede bajar de: https://covid19.apple.com/mobility
# clase 2
###############################################
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
packages_needed <- c("readr", "ggplot2", "plotly", "tidyverse", "data.table")
check_packages(packages_needed)

library(readr)
library(ggplot2)
library(plotly)
library(tidyverse)

#ver la incidencia entre del toque de queda y la grafica que da apple respecto a la mobilidad
#mo tomar los confirmados, solo la mortalidad.
applemobilitytrends-2021-04-22.csv

mobility_url <- "https://covid19-static.cdn-apple.com/covid19-mobility-data/2106HotfixDev18/v3/en-us/"

aux        <- paste(mobility_url,"applemobilitytrends-", sep = "")
filedate <- paste(aux,as.character(Sys.Date()-2),".csv",sep = "")
mobility   <- read.csv(filedate, sep = ",", header = T)
library(readr)
library(tidyverse)

#########   preparar los datos

mobility$geo_type              <- as.factor(mobility$geo_type)
mobility$region                <- as.factor(mobility$region)
mobility$transportation_type   <- as.factor(mobility$transportation_type)
colnames(mobility)

levels(mobility$transportation_type)

#pasamos al vertical por que es malo tener tantas columnas
library(tidyr)
#el 7 es desde donde tnego que verticalizar, todas las fechas
datos_v   <- mobility %>% gather(fecha, tasa    , 7:ncol(mobility))

datos_v$fecha <- as.Date(datos_v$fecha, format = "X%Y.%m.%d")
colnames(datos_v)

#  si queremos ver otro pais modificamos aqui
pais = "Buenos Aires"
trans = "walking"
datos <- subset(datos_v, region == pais & transportation_type == trans)
#limpio las columnas que no quiero usar
datos <-  select( datos, fecha, tasa)

###########   Figura de movilidad

g1 <- ggplot(datos, aes(x = fecha, y = tasa) ) +
  geom_line(size = 0.6) +
  geom_line(linetype = "dashed") +
  
  ggtitle(paste("COVID_19 - Movilidad en ",pais,sep = "")) +
  
  scale_x_date(date_breaks = "7 day", date_labels =  "%d %b") +
  #scale_y_continuous(limits = c(0, 100), breaks = seq(1, 10, 1)) +
  theme(plot.title = element_text(lineheight = 1,face ='bold'))   +
  ylab("tasa de movilidad") +
  xlab("") +
  labs(caption = "Fuente de los datos: apple.com/covid19/mobility") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7)) 
g1

