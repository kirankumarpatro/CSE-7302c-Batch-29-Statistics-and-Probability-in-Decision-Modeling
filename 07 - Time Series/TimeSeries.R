rm(list=ls(all=TRUE))
dirpath = paste0(REGRESSION_DIR,"2017-04-22 Batch28/Day4/")
setwd(dirpath)

#TIME SERIES FORECASTING

library("forecast")
library("stats")

#For monthly time series data, 
#you set frequency=12, 
#while for quarterly time series data, 
#you set frequency=4

#You can also specify the first 
#year that the data was collected, 
#and the first interval in that year 
#by using the 'start'
#parameter in the ts() function. For example, if the first data 
#point corresponds to the second quarter of 1986, you would set 
#start=c(1986,2).

# Read in the US Air Carrier Traffic - Revenue Passenger Miles dataset
#   Unit: Thousand Miles
#   Data Source: http://www.bts.gov/xml/air_traffic/src/index.xml
#   and https://datamarket.com/data/set/281x/us-air-carrier-traffic-statistics-revenue-passenger-miles 

miles = read.csv("us-air-carrier-traffic-statistic.csv")
miles

milestimeseries <- ts(miles, frequency = 12, start = c(1996,1))
milestimeseries

plot(milestimeseries)


#GE stock price over 910 days
GEStock = read.csv("GEstock-3year.csv")
GEStock

getimeseries <- ts(GEStock)
getimeseries

plot(getimeseries)

#Simulated data to understand ACF & PACF

par(mfrow=c(1,1))
time <- c(1:100)
growth <- time
plot(growth~time)

par(mfrow=c(1,2))

growth <- ts(growth)
acf(growth)
pacf(growth)

par(mfrow=c(1,1))
time <- ts(c(1:100))
growth <- sin(0.25*pi*time)
plot(growth~time, type="l")

par(mfrow=c(1,2))

#growth <- ts(sin(growth))
acf(growth)
pacf(growth)

par(mfrow=c(1,1))
time <- c(1:100)
growth <- runif(100, min=0, max=1)
plot(growth~time, type="l")

par(mfrow=c(1,2))

growth <- ts(runif(growth))
acf(growth)
pacf(growth)

#Decomposition

milestimeseriescomponents <- 
  decompose(milestimeseries)

plot(milestimeseriescomponents)
milestimeseriescomponents$seasonal
milestimeseriescomponents$trend

milestimeseriesSeasonally <- 
  milestimeseries - milestimeseriescomponents$seasonal
milestimeseriesSeasonally



#ACF and PACF of real world data

par(mfrow=c(1,3))
plot.ts(milestimeseries)
acf(milestimeseries, lag.max=20)
pacf(milestimeseries, lag.max=20)
#acf(milestimeseries, lag.max=20, ci.type="ma")


# Differencing and ACF, PACF on
# Stationary and Non-Stationary Data

par(mfrow=c(1,2))
acf(getimeseries, lag.max=20)
pacf(getimeseries, lag.max=20)

getimeseriesdiff1 <- diff(getimeseries, differences=1)
getimeseriesdiff1

acf(getimeseriesdiff1, lag.max=20)
pacf(getimeseriesdiff1, lag.max=20)

#ndiffs(getimeseries)
#ndiffs(milestimeseries)

#nsdiffs(milestimeseries)



#Moving averages

library(TTR)

par(mfrow=c(1,1))
milestimeseries
plot(milestimeseries)

smamiles <- SMA(milestimeseries, n=2)
smamiles

wmamiles <- WMA(milestimeseries, n=2)
wmamiles

emamiles <- EMA(milestimeseries, n=2)
emamiles

par(mfrow=c(1,1))
plot(milestimeseries, type="l", col="red")
lines(smamiles, col="black", lwd=2)
lines(wmamiles, col="blue")
lines(emamiles, col="brown")

MAPESMA <- mean(abs(milestimeseries[2:200]-smamiles[2:200])/abs(milestimeseries[2:200]))*100
MAPEWMA <- mean(abs(milestimeseries[2:200]-wmamiles[2:200])/abs(milestimeseries[2:200]))*100
MAPEEMA <- mean(abs(milestimeseries[2:200]-emamiles[2:200])/abs(milestimeseries[2:200]))*100

MAPESMA
MAPEWMA
MAPEEMA

#Effect of K

milestimeseriesSMA3 <- 
  SMA(milestimeseries,n=3)

milestimeseriesSMA8 <- SMA(milestimeseries,
                           n=8)

par(mfrow = c(1, 2))
plot.ts(milestimeseriesSMA3)
plot.ts(milestimeseriesSMA8)

par(mfrow = c(1, 1))

#Moving average without trend and seasonality

plot(milestimeseries)

milesforecast <- 
  HoltWinters(milestimeseries, 
              beta=FALSE, 
              gamma=FALSE)

milesforecast
milesforecast$fitted

plot(milesforecast)
milesforecast$SSE

#Let us now 
#assume there is no 
#seasonality, but there 
#is trend

#We can specify the first 
#value and slope

#Additive, trend and seasonality models



milesforecast <- 
  HoltWinters(milestimeseries)
milesforecast
milesforecast$fitted

plot(milesforecast)
milesforecast$SSE
milesresiduals <- residuals(milesforecast)
milesresiduals
plot(milesresiduals)
acf(milesresiduals)
pacf(milesresiduals)

library("forecast")

#it predicts seasonal peaks well


milesforecast2 <- 
  forecast.HoltWinters(milesforecast, 
                       h=40)

milesforecast2

plot.forecast(milesforecast2,
              shadecols="oldstyle")

# forecast with NO trend and seasonality



milesforecast <- 
  HoltWinters(milestimeseries, 
              beta=FALSE, 
              gamma=FALSE)
milesforecast <- 
  forecast.HoltWinters(milesforecast,
                       h=8)
milesforecast

plot.forecast(milesforecast,
              shadecols="oldstyle")

#ARIMA


plot(milestimeseries)
milestimeseriesdiff1 <- 
  diff(milestimeseries, 
       differences=1)
plot.ts(milestimeseriesdiff1)

milestimeseriesdiff2 <- 
  diff(milestimeseries, 
       differences=2)
plot.ts(milestimeseriesdiff2)

milestimeseriesfit <- auto.arima(milestimeseries,ic='aic')
milestimeseriesfit

par(mfrow = c(1, 2))
acf(milestimeseriesfit$residuals)
pacf(milestimeseriesfit$residuals)
Box.test(milestimeseriesfit$residuals, lag=20, type="Ljung-Box")

par(mfrow = c(1, 1))
milestimeseriesforecasts <- forecast.Arima(milestimeseriesfit, 
                  h=40)
plot.forecast(milestimeseriesforecasts)
milestimeseriesforecasts



# GDP forecast
# India's GDP growth rate
#GDP <- read.csv("GDP_AnnualGrowthRate_India.csv")
#GDP
#GDPtimeseries <- ts(GDP)
#GDPtimeseries

#plot(GDPtimeseries)

#GDPtimeseriescomponents <- 
#  decompose(GDPtimeseries)

#plot(GDPtimeseriescomponents)
# GDPtimeseriescomponents$seasonal
# GDPtimeseriescomponents$trend
# 
# GDPtimeseriesSeasonally <- 
#   GDPtimeseries - GDPtimeseriescomponents$seasonal
# 
# plot(GDPtimeseries)
# acf(GDPtimeseries, lag.max=20)
# pacf(GDPtimeseries, lag.max=20)
# acf(GDPtimeseries, lag.max=20, ci.type="ma")
# 
# auto.arima(GDPtimeseries)
# GDPtimeseries <- 
#   auto.arima(GDPtimeseries,
#              ic='aic')
# GDPtimeseries
# GDPtimeseriesforecasts <- 
#   forecast.Arima(GDPtimeseries, 
#                  h=5)
# plot.forecast(GDPtimeseriesforecasts)
# GDPtimeseriesforecasts
# 
# GDPtimeseriesarima <- arima(GDPtimeseries, order=c(2,1,2))
# class(GDPtimeseries)
# 
# GDPtimeseriesforecasts1 <-
#   forecast.Arima(GDPtimeseriesarima, h=5)
# plot.forecast(GDPtimeseriesforecasts1)
# GDPtimeseriesforecasts1

#acf(GDPtimeseriesarima$residuals)
#pacf(GDPtimeseriesarima$residuals)
#Box.test(GDPtimeseriesarima$residuals, lag=20, type="Ljung-Box")
