#Exercicios de Series Temporais

library(lubridate)
library(forecast)
library(tseries)

#Ler os dados 

dat = read.csv("Consumo.csv", header = TRUE)

#Fazer objeto ts
#Ver detalhes importantes em https://otexts.com/fpp2/ts-objects.html

consumo = ts(dat$co, start = c(1984, 1), frequency = 12)

#Plotando a serie

plot.ts(consumo)


#Função de Autocorrelação

acf(consumo,lag.max=48)

#Função de Autocorrelação Parcial

pacf(consumo, lag.max=48)

#Função de Autocorrelação com diferença na série
acf(diff(consumo, 1), lag=48)


#Função de Autocorrelação  Parcial com diferença na série
pacf(diff(consumo, 1), lag=48)



#Modelo Preliminar

mod = Arima(consumo, order = c(0,1,0), seasonal = c(0,1,0),
            lambda = "auto")


acf(residuals(mod), lag.max=48)


pacf(residuals(mod), lag.max=48)



mod2 = Arima(consumo, order = c(0,1,0), seasonal = c(0,1,1),
            lambda = "auto")



acf(residuals(mod2), lag.max=48)

pacf(residuals(mod2), lag.max=48)


mod3 = Arima(consumo, order = c(2,1,0), seasonal = c(0,1,1),
             lambda = "auto")


acf(residuals(mod3), lag.max=48)

pacf(residuals(mod3), lag.max=48)

checkresiduals(mod3)


mod.auto = auto.arima(consumo, lambda = "auto")

checkresiduals(mod.auto)


mod.auto$aic

mod3$aic



acf(diff(consumo, 1, lag=12), lag=48)


