library(readr)
library(ggplot2)
library(forecast)
library(ggfortify)
library(fBasics)

#problem1
groceries <- read_csv("Desktop/dsc 425/module 1/groceries.csv")
groceriestst<-ts(groceries$ToothPaste,start= 2008.1,frequency = 1)
lag.plot(groceriestst,lag = 1)
acf(groceriestst,lag.max=15,plot=TRUE)
Box.test(groceriestst,lag =1, type = "Ljung-Box")
#problem6
acf(groceriestst)
fit = Arima(groceriestst, order =c(0,0,1))
fit

summary = (fit)

v_t = a_t +0.6*a_t-1+219.4313
library(lmtest)
coeftest(fit)

#problem2
#a
library(readr)
library(lubridate)
Intel <- read_csv("Desktop/dsc 425/module 2/Intel-1986-2007.csv")
View(Intel)
Intel$Dates=as.Date(date_decimal(Intel$Date),tz = "UTC")
Intel=Intel[,-c(2)]
Intelts = ts(log(Intel$`Adj Price`),start = (1987-01-01),frequency = 1)
lag.plot(Intelts,lag=1, pch = 1, col="blue", cex=0.5,main = "lag plot of the log-prices series ")

#b
acf(Intelts,lag.max=15,plot =FALSE)
#c
autoplot(Intelts)
acf(Intelts,lag.max=10,plot =TRUE)

#d

Intellogreturn=diff(log(Intel$`Adj Price`))
acf(Intellogreturn,lag.max=15,plot =TRUE)
Box.test(Intellogreturn,lag =1, type = "Ljung-Box")

#problem 5
library(readr)
NAPM <- read_csv("Desktop/dsc 425/module 3/NAPM.csv")
NAPMts<- ts(NAPM$index, start = 1980.1,frequency = 12)
autoplot(NAPMts,main= "NAPM Time seris",xlab = "year")

deNAPMts  = decompose(NAPMts)
plot(deNAPMts)

acf(NAPMts,lag.max=15,plot =TRUE)
Box.test(NAPMts,lag =15, type = "Ljung-Box")
s = NAPMts[-length(NAPMts)]
fit = Arima(s,order = c(2,0,0))
fit

#mean = phi_0/1-phi_1

library(lmtest)

coeftest(fit)


0.1/0.6
