#Exercicios de Series Temporais

library(lubridate)
library(forecast)
library(tseries)

#Ler os dados 

dat = read.csv("Poluicao.csv", header = TRUE)

#Fazer objeto ts
#Ver detalhes importantes em https://otexts.com/fpp2/ts-objects.html

no2 = ts(dat$no2, start = c(1997, 1), frequency = 365)

#Plotando a serie

plot.ts(no2)


#Função de Autocorrelação

acf(no2,lag.max=25)

#Função de Autocorrelação Parcial

pacf(no2, lag.max=25)


#Modelo Preliminar

mod = Arima(no2, order = c(17,0,0), lambda = "auto",
            include.mean = FALSE,
            fixed =c(NA,0,NA,
                     0,0,0,
                     0,0,NA,
                     NA,0,0,
                     NA,NA,NA,
                     0,NA))

checkresiduals(mod)

pacf(residuals(mod), lag.max=25)

acf(residuals(mod), lag.max=25)

Box.test (residuals(mod), lag = 73, type = "Ljung", fitdf = 8)



mod.a = auto.arima(no2,max.order = 50, lambda = "auto")

checkresiduals(mod.a)

pacf(residuals(mod.a), lag.max=25)

acf(residuals(mod.a), lag.max=25)

#Serie de IPI


dat = read.csv("IPI.csv", header = TRUE)


ipialiment = ts(dat$ipialiment, start = c(1985, 1), frequency = 12)


plot.ts(ipialiment)

acf(ipialiment)

pacf(ipialiment)


acf(diff(ipialiment,1,lag=12))


pacf(diff(ipialiment,1,lag=12))


mod = Arima(ipialiment, order = c(0,0,0), seasonal=c(0,1,1), lambda="auto")


pacf(residuals(mod))

acf(residuals(mod))

mod2 = Arima(ipialiment, order = c(1,0,0), seasonal=c(0,1,1), 
             lambda="auto")

pacf(residuals(mod2))

acf(residuals(mod2))

checkresiduals(mod2)


mod3 = Arima(ipialiment, order = c(1,0,0), seasonal=c(1,1,1), 
             lambda="auto", include.drift = TRUE)

pacf(residuals(mod3))

acf(residuals(mod3))
             
             
Box.test (residuals(mod3), lag = 24, type = "Ljung")



mod4 = Arima(ipialiment, order = c(9,0,0), seasonal=c(1,1,1), 
             fixed = c(NA,0,0,
                       0,0,0,
                       NA,0,NA,
                       NA,NA,NA),
             lambda="auto", include.drift = TRUE)

Box.test (residuals(mod4), lag = 24, fitdf = 6, type = "Ljung")

pacf(residuals(mod4))

acf(residuals(mod4))


mod.b = auto.arima(ipialiment,max.order = 50, lambda = "auto")




