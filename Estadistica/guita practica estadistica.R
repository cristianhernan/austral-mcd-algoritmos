library(modeest)
library(stats)

########################## EJERCICIO 1 #####################################

#Los  siguientes  datos  corresponden  al  salario  de  bolsillo  en  pesos 
#de  los empleados de una determinada empresa
#A- Hallar  la  media  aritmética,  varianza  y  desvío  estándar  muestral.
#Determinar en cada caso las unidades en que se expresa cada medida.
#B- Obtener la mediana y el modo.
#C- Hallar el rango, los cuartiles y la distancia intercuartil.
#D- Complete  la  siguiente  frase:  “El  75%  de  los  empleados  gana  más  de  .....”. Justifique

sueldos_netos <- c(600,650,700,800,1200,500,680,1500,1400,1500,700,600,2000,3500,600,700,800,750,1000,1000,600,600,900,850,1500,600,600,700,800,1000)
mean(sueldos_netos)
#media = 977,66 pesos
var(sueldos_netos)
#varianza = 357066.8 pesos2
sd(sueldos_netos)
#desvio std= 597.66 pesos
median(sueldos_netos)
#mediana = 775
mfv(sueldos_netos)
#moda = 600 pesos
range(sueldos_netos)
#rango = 500 - 3500 pesos
IQR(sueldos_netos)
#rango_interquartil = 387.5 pesos 
quantile(sueldos_netos,0.75)
#cuartil 75% = 1000 pesos
quantile(sueldos_netos,0.50)
#cuartil 50% = 775  pesos = a la mediana
quantile(sueldos_netos,0.25)
#cuartil 25% = 612.5  pesos, por lotanto el 75 % de los empleados gana mas de 612.5 pesos


########################## EJERCICIO 2 #####################################
#Los  siguientes  datos  corresponden  a  los  porcentajes  de  rentabilidad  
#de  acciones correspondientes a 20 grandes empresas
#A- Hallar  la  media  aritmética,  varianza  y  desvío  estándar  muestral.
#Determinar en cada caso las unidades en que se expresa cada medida.
#B- Obtener la mediana y el modo.

rent_acciones <- c(27.9,11.6,17.6,18.5,26.6,20.7,15.6,12.4,13.2,22.4,18.5,16.8,22.9,24.3,18.6,25.0,24.2,15.3,21.5,18.4)
mean(rent_acciones)
#media = 19.6 %
var(rent_acciones)
#varianza = 387.5 %2
sd(rent_acciones)
#desvio std= 4.711 %
median(rent_acciones)
#mediana = 18.55 %
mfv(rent_acciones)
#moda = 18.5 %


########################## EJERCICIO 3 #####################################
#Los siguientes datos corresponden a la superficie
#implantada y la producción de cereales y oleaginosas en las provincias pampeanas

#a)Hallar  la  media,  la  varianza  y  el  desvío  estándar  
#muestral  para  la  superficie implantada y la producción para el período 1993-1999.
#b)Hallar la mediana y el modo para cada una de las series.
#c)Hallar para ambas variables el rango, los cuartiles y la distancia intercuartil.
#d)¿Cuál de las series tiene mayor variabilidad?

sup_implantada_m2 <- c(17,18,20,20,24,22,22)
#A--------------------
mean(sup_implantada_m2)
#media = 20.42 m2
var(sup_implantada_m2)
#varianza = 5.95
sd(sup_implantada_m2)
#desvio st= 2.43975

#B---------------------
median(sup_implantada_m2)
#median = 20 m2
mfv(sup_implantada_m2)
# modas = 20 y 22 m2


produccion_tons <- c(38,39,46,41,48,53,57)
mean(produccion_tons)
#media = 46 tons
var(produccion_tons)
#varianza =  387.5 tons2
sd(produccion_tons)
#desviacion std= 7.72 tons

#B---------------------
median(sup_implantada_m2)
#median = 20 m2
mfv(sup_implantada_m2)
# modas = 20 y 22 m2

#RANGO
#diferencia entre el valor maximo y minimo observado.
#rango <- max(data$fi) - min(data$fi)
#rango
#132

#RANGO Intercuartílico
#El Rango Intercuartílico es la diferencia entre el tercer y el primer cuartil,
#o lo que es lo mismo, entre el percentil 75 y el percentil 25:
#rango_int <- p75-p25
#rango_int
#3.712054


