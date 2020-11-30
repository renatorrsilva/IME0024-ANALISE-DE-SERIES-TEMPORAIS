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

#Augmented Dickey-Fuller Test (Teste Para verificar necessidade de diferenças)

adf.test(co, alternative = c("stationary"))


#Função de Autocorrelação

acf(co,lag.max=25)

#Função de Autocorrelação Parcial

pacf(co, lag.max=25)


#Modelo Preliminar

mod.a = auto.arima(co,max.order = 10, lambda = "auto")

checkresiduals(mod.a)


mod=  Arima(co, order = c(25,0,0), lambda = "auto", fixed=c(NA,NA, NA, 
                                                             0,0,0,
                                                             0,0,0,
                                                             0,0,0,
                                                             NA,0,NA,
                                                             0,0,0,
                                                             0,0,0,
                                                             0,0,0,
                                                             NA,
                                                             NA))

checkresiduals(mod)


pacf(residuals(mod), lag.max=25)

acf(residuals(mod), lag.max=25)

Box.test (residuals(mod), lag = 73, type = "Ljung", fitdf = 7)

forecast(mod, h=12,biasadj=TRUE) %>% autoplot()


##SO2


so2 = ts(dat$so2, start = c(1997, 1), frequency = 365)

#Plotando a serie

plot.ts(so2)



#Função de Autocorrelação

acf(so2, lag.max=25)

#Função de Autocorrelação Parcial

pacf(so2, lag.max=25)


#Modelo Preliminar


mod.a = auto.arima(so2, lambda="auto", max.order = 20)

checkresiduals(mod.a)

mod=  Arima(so2, order = c(21,0,0), lambda = "auto", fixed=c(NA,NA, NA, 
                                                            0,NA,NA,
                                                            0,0,NA,
                                                            NA,0,NA,
                                                            NA,NA,NA,
                                                            0,0,0,
                                                            NA,NA,NA,
                                                            NA))


checkresiduals(mod)


pacf(residuals(mod), lag.max=25)

acf(residuals(mod), lag.max=25)

Box.test (residuals(mod), lag = 73, type = "Ljung", fitdf = 15)

forecast(mod, h=12,biasadj=TRUE) %>% autoplot()



##o3


o3 = ts(dat$o3, start = c(1997, 1), frequency = 365)

#Plotando a serie

plot.ts(o3)

#Função de Autocorrelação

acf(o3, lag.max=25)

#Função de Autocorrelação Parcial

pacf(o3, lag.max=25)

mod.a = auto.arima(o3,max.order = 15, lambda = "auto")


checkresiduals(mod.a)


mod=  Arima(o3, order = c(13,0,0), lambda = "auto", fixed=c(NA,0,0,
                                                             0,0,0,
                                                             0,0,0,
                                                             0,0,0,
                                                             NA,
                                                             NA))

pacf(residuals(mod), lag.max=25)

acf(residuals(mod), lag.max=25)

Box.test (residuals(mod), lag = 73, type = "Ljung", fitdf = 3)

