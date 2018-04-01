#trading market, stocks
#retail 
#trades
#forecast
#timeseries forecasting 

#A candlestick is a chart that displays the high, low, opening and closing prices of a security for a specific period.

#Long Trade= Profit from a rising market
# Buy low, sell high. 

#Short Trade= Profit from a falling market
# Buy, sell and buy again a lower price.

rm(list=ls())

library(ggplot2)
library(zoo)
library(xts)
library(quantmod)

#definir una funcion de cambio
shift<-function(x,n){
  c(x[-(seq(n))],rep(NA,n))
}
  
#Cargar la informacion.

setwd("...")

data<-read.csv("EURUSD60.csv",header = TRUE)
sapply(data, class)

data$Date <-as.character(data$Date)
data$Date<-strptime(data$Date,format = "%Y.%m.%d %H:%M")
data$Date<-as.POSIXct(data$Date)

data$bull<-1
data$bull<-ifelse(data$Open>data$Close,0,data$bull)

data$count<-1
for (i in 2:nrow(data)) {
  if(data[i,]$bull==data[i-1,]$bull){
    data[i,]$count<-(data[i-1,]$count)+1
  }
}

sapply(data, class)
str(data)

head(data,20)

#### Velas ####

data$nextcount<-shift(data$count,1)
head(data,20)

# Crear una tabla que contenga el numero de eventos. Por ejemplo, despues de 3 velas consecutivas
# Cuantas veces el patron continua vs Cuantas veces el patron cambia.

mytable<- table(data$count,data$nextcount)
mytable

#Calcular las probabilidades de los eventos
percent.table<-prop.table(mytable,1)

#Tabla de probabilidades
print.table(local({percent.table[percent.table==0]<-NA;percent.table}))


#Comprobacion de la metodologia
# Despues de "x" velas consecutivas, canjea (trade) en la direccion opuesta.

data$nextopen<-shift(data$Open,1)
data$nextclose<-shift(data$Close,1)


# filtro para 7 velas consecutivas.
trades<-subset(data,data$count>=7)

# Marca cual direccion se necesita para comerciar.
trades$direction<-1
trades$direction<-ifelse(trades$bull==1,-1,trades$direction)

# Fija el costo del comercio a cero.
trades$cost<-0.000

#Calculo de las ganancias y los beneficios totales.
trades$profit<-trades$direction*(trades$nextclose-trades$nextopen)-trades$cost
trades$balance<-cumsum(trades$profit)

plot(trades$balance) # Se esta perdiendo dinero. 

#Modificas el costo del comercio y se vuelve a calcular la rentabilidad

trades$cost<-0.002
trades$profit<-trades$direction*(trades$nextclose-trades$nextopen)-trades$cost
trades$balance<-cumsum(trades$profit)

plot(trades$balance) # Se pierde aun más dinero.


############ ciclicidad comercial ############ La relacion de precios del euro y usd es ciclico?

# Agregar un numero de fila, para que se pueda realizar la operacion sin tomar en cuenta el tiempo no uniforme.
data$row<-seq(1,nrow(data),1)

#Suavizar el grafico de precios
data$smoothed<-ksmooth(data$row,data$Close,"normal",bandwidth = 10)$y

plot(data[1:600,]$Close)
lines(data[1:600,]$smoothed,col="red")

# Encontrar los picos y valles.

peaks <- which(diff(diff(data$smoothed)>=0)<0)+1
troughs <- which(diff(diff(data$smoothed)>0)>0)+1

data$minmax<-0
data$minmax<-ifelse(data$row %in% peaks, 1, data$minmax)
data$minmax<-ifelse(data$row %in% troughs, -1, data$minmax)


ggplot(data[1:1000,],aes(row))+
  geom_point(aes(y=Close,colour="Close"),size=1.5)+
  geom_line(aes(y=smoothed,colour="smoothed"),size=1.2)+
  geom_point(data=subset(data, data$minmax==1 & data$row<=1000),
             aes(y=smoothed),color="red",size=4)+
  geom_point(data=subset(data, data$minmax==-1 & data$row<=1000),
             aes(y=smoothed),color="green",size=4)+
  theme_bw()+
  theme(legend.position = "none")



# Graficar la ciclicidad del tipo de cambio.
# Diferencia de precio vs Diferencia de tiempo.
tmp1<- subset(data,data$minmax !=0)
tmp1$pricediff<-c(NA,abs(diff(tmp1$smoothed)))
tmp1$candlediff<-c(NA,diff(tmp1$row))


# A partir del siguiente grafico se puede deducir si es viable comerciar o no.
plot(tmp1$pricediff,tmp1$candlediff)
abline(v=0.015,col="red")
abline(h=30,col="red")

#Percentile para futuro uso.
pricediff_perc<-ecdf(tmp1$pricediff)(0.015)
candlediff_perc<-ecdf(tmp1$candlediff)(30)

pricediff_value<- quantile(tmp1$pricediff,pricediff_perc,na.rm = T)
candlediff_value<-quantile(tmp1$candlediff,candlediff_perc,na.rm = T)

# Histrograma de los valores de las diferencias de precios y los cortes realizados.

hist(tmp1$pricediff,breaks = 50,col = "grey", border = "black",xlim =c(0,0.05))
abline(v=pricediff_value,col="red")


hist(tmp1$candlediff,breaks = 30,col = "grey", border = "black",xlim =c(0,100))
abline(v=candlediff_value,col="red")

# Filtro de las oportunidades comerciales.
tmp2<-subset(tmp1,tmp1$pricediff>=pricediff_value & tmp1$candlediff<=candlediff_value)
data$tradeopenprice<-shift(data$Open,1)
data$tradecloseprice<-shift(data$Close,10)

#Subconjunto de datos para el cambio de divisa. 
trades<-subset(data,data$row %in% tmp2$row)

#Establecer la direccion para el comercio.
trades$direction<-1
trades$direction<-ifelse(trades$minmax==1,-1,trades$direction)

# Calculo de las ganancias y perdias en el cambio de divisas.
trades$cost<-0.000
trades$profit<-trades$direction*(trades$tradecloseprice-trades$tradeopenprice)-trades$cost
trades$balance<-cumsum(trades$profit)

#Grafica de los resultados
plot(trades$balance)


# Calculo de las ganancias y perdias en el cambio de divisas.
trades$cost<-0.002
trades$profit<-trades$direction*(trades$tradecloseprice-trades$tradeopenprice)-trades$cost
trades$balance<-cumsum(trades$profit)

#Grafica de los resultados
plot(trades$balance)




























  
  
  
  
  

