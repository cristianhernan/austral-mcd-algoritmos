########################## EJERCICIO 4 #####################################
#En   la   siguiente   tabla   figuran   los   años   de   servicio   acumulados   por   
#355 trabajadores de una gran empresa antes de su retiro 
#voluntario.Años de ServicioNúmero de empleados
#0-3=112
#3-6=153
#6-9=42
#9-12=27
#12-15=21
#a)Hallar el promedio de los años de servicios acumulados por los empleados de 
#la empresa y el desvío estándar muestral.
#b)Obtener la mediana, la moda y los cuartiles.
#c)Determinar los coeficientes de asimetría y kurtosis.


li <- c(0,3,6,9,12)
ls <- c(3,6,9,12,15)
frec <- c(112,153,42,27,21)
data <- data.frame('Li'=li,'Ls'=ls, 'fi'=frec)
#calculo de la frecuencias relativas %
data$fr <-  ((data$fi*100)/355)
#calculo de las frecuencias acumuladas
data$Fi <- cumsum(data$fi)
#calculo de las frecuencias relativas actumuladas
data$Fri <- cumsum(data$fr)
#calculo de la media por cada distribucion (MARCA DE CLASE xi)
data$Xi <- (data$Li+data$Ls)/2

#MEDIANA 
#Cuando solamente disponemos de los datos agrupados, la mediana estará dentro del primer intervalo que
#acumule una frecuencia mayor o igual a n/2
#Me=li+(li+(n/2-fj-1)/fj)-wj

mediana <- 3+(((355/2)-112)/153)*3

#MODA
#Cuando contamos con datos agrupados, existe una clase modal, la cual es la que posee mayor frecuencia
#Mo=Li+(d1/d1+d2)*wi
moda <- 3+((153-112)/((153-112)+153-42))*3

#PERCENTILES
p25 <- 0+((((355*25)/100)-0)/112)*3

p50 <- 3+((((355*50)/100)-112)/153)*3

p75 <- 6+((((355*75)/100)-265)/42)*3

p85 <- 6+((((355*85)/100)-265)/42)*3

p100 <- 12+((((355*100)/100)-334)/21)*3



#MEDIA ARITMETICA
#1/n SUM (1 a 7)yi.fi

#calculo el producto entre la marca de clase y la frecuencia de cada elemento
#para obtener xi.fi
data$Xi.Fi <- (data$Xi*data$fi) 


#obtengo la media total, haciendo el cociente entre la suma total del xi.fi y la suma total de la fi
media <- sum(data$Xi.Fi)/sum(data$fi)

#VARIANZA
#N= suma de las frecuencias = 355
#marcas de clase xi.fi
data$Xi.Fi

#calculamos la marca de clase al 2 por fi
data$Xi2.Fi <- (data$Xi*data$Xi.Fi)
#calculamos la marca de clase - media al cuadrado por fi
data$XiMedia2 <- ((data$Xi-media)^2)*data$fi

var2 <- sum(data$XiMedia2) / sum(data$fi)

#Desvio estandard.
destd <- sqrt(var2)

#COEFICIENTE DE ASIMETRIA
data$Xi_me3 <- ((data$Xi-media)^3)*data$fi
mc3 <- 1/sum(data$fi)*sum(data$Xi_me3)
cas <- mc3/(sqrt(var2)^3)

#coeficiente de asimetria 1.075658 > 0 = asimetrica positiva

#Coeficiente de Curtosis

data$Xi_me4 <- ((data$Xi-media)^4)*data$fi
mc4 <- 1/335*sum(data$Xi_me4)
k <- mc4/(var2)^2

#RESULTADOS:
paste('TABLA')
data

paste('A) Promedio años de servicio: ', media, ' años', sep="")
paste('Desvio estandard: ', destd, ' años', sep="")

paste('B) Mediana: ', mediana, ' Moda: ', moda, 'Media: ' , media, sep="")
paste('p25: ', p25, ' p50: ', p50, 'p75: ' , p75, sep="")
paste('C) Asimetria: ', cas, ' Curtosis: ',k, sep="")








