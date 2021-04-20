########################## EJERCICIO 5 #####################################
# La siguiente tabla de frecuencias relativas corresponde al ingreso familiar 
# (en pesos) obtenido sobre una muestra de 50.000 encuestas.
# Nivel de Ingresos (en pesos) Porcentaje
# 0-500 4
# 500-1.000 3
# 1.000-1.500 10
# 1.500-2.000 27
# 2.000-3.000 16
# 3.000-5.000 12
# 5.000-7.000 12
# 7.000-10.000 6
# 10.000-15.000 5
# 15.000-25.000 4
# 25.000-40.000 1
# a) Calcular la distribución de frecuencias absolutas y relativas acumuladas.
# b) Hallar la media de la distribución y la mediana.
# c) Calcular la medida de dispersión más adecuada.



li <- c(0,500,1000,1500,2000,3000,5000,7000,10000,15000,25000)
ls <- c(500,1000,1500,2000,3000,5000,7000,10000,15000,25000,40000)
fr <- c(4,3,10,27,16,12,12,6,5,4,1)
n <- 50000
data <- data.frame('Li'=li,'Ls'=ls, 'fr'=fr)
#calculo de la frecuencias absolutas  fi
data$fi <-  ((data$fr*n)/100)
#calculo de las frecuencias acumuladas
data$Fi <- cumsum(data$fi)
#calculo de las frecuencias relativas actumuladas
data$Fri <- cumsum(data$fr)

#calculo MARCA DE CLASE xi
data$Xi <- (data$Li+data$Ls)/2

#MEDIA ARITMETICA
#1/n SUM (1 a 7)yi.fi
#calculo el producto entre la marca de clase y la frecuencia de cada elemento
#para obtener xi.fi
data$Xi.fi <- (data$Xi*data$fi) 
#obtengo la media total, haciendo el cociente entre la suma total del xi.fi y la suma total de la fi
media <- sum(data$Xi.fi)/sum(data$fi)

#MEDIANA 
#Cuando solamente disponemos de los datos agrupados, la mediana estará dentro del primer intervalo que
#acumule una frecuencia mayor o igual a n/2
#Me=li+(li+(n/2-fj-1)/fj)-wj
mediana <- 2000+(((n/2)-22000)/8000 )*1000

#VARIANZA

#calculamos la marca de clase al 2 por fi
data$Xi2.fi <- (data$Xi*data$Xi.fi)
#calculamos la marca de clase - media al cuadrado por fi
data$XiMedia2 <- ((data$Xi-media)^2)*data$fi

data
var2 <- sum(data$XiMedia2) / sum(data$fi)

#Desvio estandard.
destd <- sqrt(var2)

#COEFICIENTE DE VARIACION
#desviostandar / media
cv <- destd/media


#RESULTADOS:
paste('TABLA')
data

paste('A) frecuencias absolutas y relativas acumuladas: $', data$Fi,' ', data$Fri,'%' ,  sep="")
paste('B) Mediana: $ ', mediana,  'Media: $ ' , media, sep="")
paste('C) CV: ', cv,  sep="")



