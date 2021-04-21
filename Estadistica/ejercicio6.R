########################## EJERCICIO 6 #####################################
# A continuación, se presenta la distribución de frecuencias de las ventas totales 
# diarias efectuadas por una empresa textil
# 0-500       30
# 500-1.000   41
# 1.000-1.500 90
# 1.500-2.000 74 
# 2.000-2.500 56
# 2.500-3.000 43
# 3.000-3.500 36
# 3.500-4.000 18
# 4.000-5.000 12
# 
# a) Determinar analítica y gráficamente el modo, la mediana y los cuartiles.
# b) Calcular la media aritmética y la varianza.
# c) Calcular los deciles de segundo, cuarto y noveno orden.
# d) Calcular la asimetría y la kurtosis.


li <- c(0,500,1000,1500,2000,2500,3000,3500,4000)
ls <- c(500,1000,1500,2000,2500,3000,3500,4000,5000)
fi <- c(30,41,90,74,56,43,36,18,12)
n <- sum(fi) #400
data <- data.frame('Li'=li,'Ls'=ls, 'fi'=fi)
#calculo de las frecuencias acumuladas
data$Fi <- cumsum(data$fi)
#calculo MARCA DE CLASE xi
data$Xi <- (data$Li+data$Ls)/2
data
#calculo de la frecuencia relativa
data$fr <-  ((data$fi*100)/n)

#calculo de las frecuencias relativas actumuladas
data$Fr <- cumsum(data$fr)


#MEDIA ARITMETICA
#1/n SUM (1 a n)yi.fi
#calculo el producto entre la marca de clase y la frecuencia de cada elemento
#para obtener xi.fi
data$Xi.fi <- (data$Xi*data$fi) 
#obtengo la media total, haciendo el cociente entre la suma total del xi.fi y la suma total de la fi
media <- sum(data$Xi.fi)/sum(data$fi)
data
#MEDIANA 
#Cuando solamente disponemos de los datos agrupados, la mediana estará dentro del primer intervalo que
#acumule una frecuencia mayor o igual a n/2
#Me=li+(li+(n/2-fj-1)/fj)-wj
#n/2= 200 = registro 4
head(data,4)
mediana <- 1500+(((n/2)-161)/2000 )*500


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



