# primera clase de R
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
# libs necessaries
packages_needed <- c("ggplot2", "ggrepel", "plotly",
                     "lubridate", "htmlwidgets" , "RColorBrewer",
                     "sqldf", "data.table", "readr" )

#con esto evaluamos si tenemos todas la libs
check_packages(packages_needed)

#cargamos en memoria para usar las libs
library(ggplot2)
library(ggrepel)
library(plotly)
library(lubridate)
library(htmlwidgets)
library(RColorBrewer)
library(sqldf)
library(grid)
library(data.table)
library(readr)

########################## LEER FILE

URL <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"
#concateno el file con la url, con un separador vacio ""
url_archivo <- paste(URL,"time_series_covid19_confirmed_global.csv", sep = "")
#con sep ponemo el separador, y con header le digo que en la primer liena hay nombres de columnas
#formato de tabla horizontal, por cada pais nos padan la fecha
#esto es medio desastroso
COVID_19_h <- read.csv(url_archivo,sep=",",header = T)

#aca se remueve cosas que no necesito
#se pone el null a las cols que no quiero
COVID_19_h$Lat  <- NULL 
COVID_19_h$Long <- NULL
COVID_19_h$Province.State <- NULL
colnames(COVID_19_h)
#head(COVID_19_h)

library(tidyr)

#aca se pasa del formato horizontal al vertical.
COVID_19  <- COVID_19_h %>% gather(date, casos, 2:ncol(COVID_19_h))
# otra forma de hacer lo mismo COVID_19  <- gather(COVID_19_h, date, casos, 2:ncol(COVID_19_h))
# melt(COVID_19_h) , id.vars="Country.Region")

#cambio los nombres de columna
colnames(COVID_19) <- c('pais','date','casos')

#las columnas no puden comenzar como numero
#aca casteo los datos,a char, luego a data, con el formato actual
#es como el parce esact de c#
COVID_19$date <- as.Date(as.character(COVID_19$date), format = "X%m.%d.%y")

# agrupo por fecha, sumarizando todas las fechas iguales, sin importar el pais
casos_por_fecha <- COVID_19 %>% group_by(date) %>% summarise(casos = sum(casos))


# lo anterior  en SQL seria:

# casos_por_fecha <- sqldf( "select  date, sum(casos) casos
#                           from COVID_19
#                           group by date")

# -- ordenamos asc
datos <- arrange(casos_por_fecha, (casos_por_fecha$date) )

###############################################
# genero  figura dinamica
#el + es para manipular el grafico

g1 <- ggplot(datos ,aes(x = date, y = casos/1000000)) +
  geom_point( size=1, color="blue") +
  #todo esto es cosmetica
  ggtitle("COVID_19 - Casos confirmados a nivel mundial") +
  scale_x_date(date_breaks = "10 day", date_labels =  "%d %b") +
  theme(plot.title = element_text(lineheight = 1,face ='bold'))   +
  ylab("cantidad de casos en M") +
  #le saca el titulo al x pq sabemos que es fecha
  xlab("") +
  labs(caption = "\nFuente: The Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE)") +
  theme_minimal() +
  #pone en diagonal los textos de fecha
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#ACA SE TRANSFORMA EN GRAFICO DINAMICO, le da alguna funcionalidad

g1 <- ggplotly(g1, tooltip = c("casos")) %>%
  layout(legend = list(
    orientation = "h",
    x = 0.7,
    y = 1
  )
  )
g1



