install.packages("tidyverse")
install.packages("string")
library(tidyverse)
library(lubridate)
library(stringr)
library(forecast)
library(dplyr)
##setting the working directory
setwd("C:/Users/DELL/OneDrive/Documents/Kaggle Case Study/Assignment 4")
#Read the Data
train<-read.csv("Train.csv")
test<-read.csv("DemandTest.csv")
sample<-read.csv("DemandSubmission.csv")
##inspecting the data
View(train)
str(train)
## on inspecting the data we get to know that class of date column is char
## we need to convert it in date format
train$date=as.Date(train$date,"%d-%m-%Y")
class(train$date)
library(tseries)
#converting dataset in time series set
ts=ts(train$mean_demand_kiloGallon,start=c(2014,01,1),end=c(2018,01,1),
      frequency = 365.25)

#ADF, ACF, PACF
adf.test(ts)
plot(ts)
par(mfrow=c(1,2))
acf(ts,main="ACF")
pacf(ts,main="PACF")
##trying differencing
ts1<-diff(ts,differences = 1)
adf.test(ts1)
plot(ts1)
acf(ts1,main="ACF")
pacf(ts1,main="PACF")

##using arima model

arima<-Arima(y=ts,order=c(2,1,0),
             seasonal = c(0,1,0))
arima

futForecast <- forecast(arima, h=113)
str(futForecast)
plot(futForecast)
#using arima model with differencing time series
arima1<-Arima(y=ts1,order=c(2,1,0),
              seasonal = c(0,1,0))
arima1

futForecast1 <- forecast(arima1, h=113)
str(futForecast1)
plot(futForecast1)

arima2<-Arima(y=ts1,order=c(2,0,0),
              seasonal = c(0,1,0))
arima2

futForecast2 <- forecast(arima2, h=113)
str(futForecast2)
plot(futForecast2)


arima3<-Arima(y=ts,order=c(4,0,0),
              seasonal = c(0,1,0))
arima3

futForecast3 <- forecast(arima3, h=113)
str(futForecast3)
plot(futForecast3)


#taking the forecast using the best model
head(futForecast3$mean,30)

test$meanChemDemand<-(futForecast3$mean)
sample$mean_demand_kiloGallon <- test$meanChemDemand
submission1 <- sample

stlf<-stlf(y=ts,h=113)
stlf

fts=data.frame(stlf)
date=test$date
mean_demand_kiloGallon=fts$Point.Forecast
submit4=cbind(date,mean_demand_kiloGallon)
colnames(submit4)=c('date','mean_demand_kiloGallon')
write.csv(submit4,"Submissionstlf4.csv",row.names = FALSE)

