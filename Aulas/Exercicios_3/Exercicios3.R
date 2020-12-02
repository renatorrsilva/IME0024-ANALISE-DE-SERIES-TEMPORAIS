#Exercicios de Series Temporais

library(lubridate)
library(forecast)
library(tseries)

#Ler os dados 

dat = read.csv("Poluicao.csv", header = TRUE)

#Fazer objeto ts
#Ver detalhes importantes em https://otexts.com/fpp2/ts-objects.html

co = ts(dat$co, start = c(1997, 1), frequency = 365)

#Plotando a serie

plot.ts(co)


#Função de Autocorrelação

acf(co,lag.max=98)

#Função de Autocorrelação Parcial

pacf(co, lag.max=98)


#Modelo Preliminar

mod = Arima(co, order = c(3,0,0), lambda = "auto")

checkresiduals(mod)

pacf(residuals(mod), lag.max=75)

acf(residuals(mod), lag.max=25)

Box.test (residuals(mod), lag = 73, type = "Ljung", fitdf = 7)

forecast(mod, h=12,biasadj=TRUE) %>% autoplot()


mod.a = auto.arima(co,max.order = 30, lambda = "auto")

checkresiduals(mod.a)

pacf(residuals(mod.a), lag.max=25)

acf(residuals(mod.a), lag.max=25)



##o3


o3 = ts(dat$o3, start = c(1997, 1), frequency = 365)

#Plotando a serie

plot.ts(o3)

#Função de Autocorrelação

acf(o3, lag.max=98)

#Função de Autocorrelação Parcial

pacf(o3, lag.max=98)

mod=  Arima(o3, order = c(1,0,0), lambda = "auto")

pacf(residuals(mod), lag.max=98)

acf(residuals(mod), lag.max=98)

Box.test (residuals(mod), lag = 73, type = "Ljung", fitdf = 2)

checkresiduals(mod)



