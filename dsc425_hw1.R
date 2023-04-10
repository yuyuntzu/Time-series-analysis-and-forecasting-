install.packages("ggplot2")
install.packages("forecast")
install.packages("ggfortify")
install.packages("fBasics")
library(ggplot2)
library(forecast)
library(ggfortify)
library(fBasics)

library(readr)

help("qplot")
coil <- read_csv("Desktop/dsc 425/crudeoil.csv")
coil$date=as.Date(coil$date,"%d-%b-%y")
coilts = ts(coil$price,start = (2004-01-02),frequency = 7)
autoplot(coilts)

basicStats(coilts)
coilsmooth = ma(coilts,order = 30,centre = T)
coilsmooth
autoplot(coilsmooth)
combined = ts.union(coilts, coilsmooth)
autoplot(window(combined))
autoplot(combined, facets=T) 
#30 day moving average 

coil <- read_csv("Desktop/dsc 425/crudeoil.csv")
coil$date=as.Date(coil$date,"%d-%b-%y")
f30 = rep(1/30, 30)  
f30
coilts = ts(coil$price,start = 2004.1,frequency = 52)
coilts
mAve = filter(coilts, f30, sides=2)   

qplot(time(coilts), coilts, geom="line",ylab = "price",main = "oil price") + 
  geom_line(aes(x=time(mAve), y=mAve), col="red")



#Loess
price=coil$price
date=coil$date
ds = data.frame(price,date)
ds

loess30 = loess(coil$price~ as.numeric(coil$date),data = ds, span =0.30)
smoothed30 = predict(loess30)

ggplot(data=ds, aes(x=date, y=price)) + geom_line() + geom_smooth(col="red")+ 
  geom_line(aes(x=date, y=mAve), col="blue")

help("ggplot")


#Compute the percentage change rate of spot prices using the formula 
#rate = (pt - pt-1) /pt-1, where pt is the oil price 

n = nrow(coil)
coilrate=diff(coil$price)/coil$price[-n]
plot(coil$date[-n],coilrate,type = "l")

#Analyze the distribution of the rate using a normal quantile plot. 
#Discuss the results. 
#Compute the symmetry and kurtosis of the rate distribution


qplot(coilrate,geom="histogram",main=" rate distribution")
qqnorm(coilrate)
qqline(coilrate)

rate = data.frame(coil$date[-n],coilrate)
names(rate)=c("date","rate")
library(fBasics)
kurtosis(rate$rate)
skewness(rate$rate)
library(tseries)
jarque.bera.test(rate$rate)

#compute the log-rate of change of the series 

n = nrow(coil)

coilratelog=diff(log(coil$price))

plot(coilratelog,type = "l")
plot(coil$date[-n],coilratelog,type = "l")

help("skewness")
#qqnorm(coilratelog)
#qqline(coilratelog)

#ratelog = data.frame(coil$date[-n],coilratelog)
#names(ratelog)=c("date","rate")
#library(fBasics)
#kurtosis(ratelog$rate)
#skewness(ratelog$rate)
#library(tseries)
#jarque.bera.test(ratelog$rate)


#question 2
library(readr)
groceries <- read_csv("Desktop/dsc 425/module 1/groceries.csv")
groceries$Date=as.Date(groceries$Date,"%d-%b-%y")
groceries
groceriestst <-ts(groceries$ToothPaste,start= 2008.1,frequency = 52)
groceriestsp <- ts(groceries$PeanutButter,2008.1,frequency = 52)
groceriestsb <- ts(groceries$Biscuits,2008.1,frequency = 52)


qplot(time(groceriestst),groceriestst,geom="line",main="weekly sales",xlab="date",ylab="sales")

p1=qplot(groceries$Date,groceriests,geom="line",main="weekly sales",xlab="date",ylab="sales")

groceriestst <-ts(groceries$ToothPaste,0,frequency = 4)
dec = decompose(groceriestst)
plot(dec)

p2 =qplot(groceries$Date,dec,geom="line",main="weekly sales",xlab="date",ylab="sales")

p2


#Problem 3


library(fpp2)
auscafe
time(auscafe)
autoplot(auscafe)

qplot(time(auscafe), log(auscafe),xlab = "monthly",ylab = "expenditure", geom="line",main = "monthly expenditure on cafes")

auscafediff=diff(log(auscafe))

qplot(auscafediff,geom="histogram",main=" monthly expenditure")
qqnorm(auscafediff)
qqline(auscafediff)


library(fBasics)
jarqueberaTest(auscafediff)

decauscafe = decompose(log(auscafe))
plot(decauscafe)
