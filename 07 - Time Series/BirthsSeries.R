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


#a data set of the number of births per 
#month in New York city, 
#from January 1946 to December 1959

births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
births

birthstimeseries <- ts(births, 
                       frequency=12, 
                       start=c(1946,1))
birthstimeseries

plot(birthstimeseries)

#Regression on time

par(mfrow=c(1,1))
births <- data.frame(births)
births$time <- seq(1:168)
View(births)
plot(births$births, type="l")
lm1 <- lm(births$births ~ births$time)   #Linear Fit with time
lm2 <- lm(births$births ~ 
            poly(births$time, 2, raw=TRUE)) #Quadratic Fit with time
lm3 <- lm(births$births ~ 
            poly(births$time, 3, raw=TRUE)) #Cubic Fit with time

points(births$time, predict(lm1), 
       type="l", col="red", lwd=2)  #plot Linear Fit
points(births$time, predict(lm2), 
       type="l", col="green", lwd=2)   #plot Quadratic Fit
points(births$time, predict(lm3), 
       type="l", col="blue", lwd=2)   #plot Cubic Fit

#Create a seasonal factor variable
births$seasonal <- as.factor(rep(c(1:12),14))
View(births)


#Do Linear, Quadratic & Cubic fit along with Seasonality
lm1s <- lm(births ~ ., data=births)
lm2s <- lm(births ~ poly(time, 2, raw=TRUE)+
             seasonal, data=births)
lm3s <- lm(births ~ poly(time, 3, raw=TRUE)+
             seasonal, data=births)

plot(births$births, type="l")
points(births$time, predict(lm1s), 
       type="l", col="red", lwd=2)
points(births$time, predict(lm2s), 
       type="l", col="blue", lwd=2)

plot(births$births, type="l")
points(births$time, predict(lm3s), 
       type="l", col="green", lwd=2)

# tslm() allows us to do the same fit easily, if the input data is structured as a timeseries object

tslmfit <- tslm(birthstimeseries ~ poly(trend,3) + season)
plot(birthstimeseries)
points(tslmfit$fitted.values, 
       type="l", col="green", lwd=2)


#Another crude approach
#Seasonal adjustment as a multiplicative factor
births$SeasonalFactor <- births$births/predict(lm1)
##births$month <- rep(seq(1:12),14)
View(births)
head(births)

seasonalAdustFactor <- tapply(births$SeasonalFactor, 
                   births$seasonal, mean)
seasonalAdustFactor

birthspr <- predict(lm1)*rep(seasonalAdustFactor,14)

plot(births$births, type="l")
points(births$time, birthspr,   type="l", col="red", lwd=2)

#Seasonal adjustment as a additive factor
births$mae <- births$births-predict(lm1)
View(births)
head(births)

seasonalAdd <- tapply(births$mae, 
                      births$seasonal, mean)
seasonalAdd

birthspr <- predict(lm1)+rep(seasonalAdd,14)

plot(births$births, type="l")
points(births$time, birthspr, type="l", col="green", lwd=2)



#Separation of Seasonal and Trend components using decompose function
birthstimeseriescomponents <-   decompose(birthstimeseries)

plot(birthstimeseriescomponents)
birthstimeseriescomponents$seasonal
birthstimeseriescomponents$trend

birthstimeseriesSeasonally <- 
  birthstimeseries - birthstimeseriescomponents$seasonal
birthstimeseriesSeasonally



plot(birthstimeseries)
acf(birthstimeseries, lag.max=20)
pacf(birthstimeseries, lag.max=20)
#acf(birthstimeseries, lag.max=20, ci.type="ma")


#ma <- birthstimeseriescomponents$random
#trend <- birthstimeseriescomponents$trend
#seas <- birthstimeseriescomponents$seasonal

#par(mfrow=c(2,2))
#plot(birthstimeseries)
#plot(trend)
#plot(seas)
#plot(ma)

#par(mfrow=c(3,2))
#acf(ma, na.action=na.pass)
#pacf(ma, lag.max=20, na.action=na.pass)
#acf(trend, na.action=na.pass)
#pacf(trend, lag.max=20, na.action=na.pass)
#acf(seas, na.action=na.pass)
#pacf(seas, lag.max=20, na.action=na.pass)



#Moving average without trend and seasonality

par(mfrow = c(1, 1))
plot(birthstimeseries)

birthsforecast <- 
  HoltWinters(birthstimeseries, 
              beta=FALSE, 
              gamma=FALSE)

birthsforecast
birthsforecast$fitted

plot(birthsforecast)
birthsforecast$SSE

#Let us now 
#assume there is no 
#seasonality, but there 
#is trend

#We can specify the first 
#value and slope

#Additive, trend and seasonality models

birthsforecast <- 
  HoltWinters(birthstimeseries)
birthsforecast
birthsforecast$fitted

plot(birthsforecast)
birthsforecast$SSE


birthsforecast2 <- 
  forecast.HoltWinters(birthsforecast, 
                       h=8)

birthsforecast2

plot.forecast(birthsforecast2)
plot.forecast(birthsforecast2, 
              shadecols=terrain.colors(3))
plot.forecast(birthsforecast2,
              shadecols="oldstyle")

# forecast with NO trend and seasonality

birthsforecast <- 
  HoltWinters(birthstimeseries, 
              beta=FALSE, 
              gamma=FALSE)
birthsforecast <- 
  forecast.HoltWinters(birthsforecast,
                       h=8)
birthsforecast

plot.forecast(birthsforecast,
              shadecols="oldstyle")


#ARIMA
plot(birthstimeseries)
birthstimeseriesdiff1 <-  diff(birthstimeseries, differences=1)
plot.ts(birthstimeseriesdiff1)

birthstimeseriesdiff2 <-  diff(birthstimeseries, differences=2)
plot.ts(birthstimeseriesdiff2)

auto.arima(birthstimeseries)


#Parsimonious models
birthsArima <- 
  auto.arima(birthstimeseries, ic='aic')
            
birthsArima
birthstimeseriesforecasts <- 
  forecast.Arima(birthsArima,  h=5)
                
plot.forecast(birthstimeseriesforecasts)
