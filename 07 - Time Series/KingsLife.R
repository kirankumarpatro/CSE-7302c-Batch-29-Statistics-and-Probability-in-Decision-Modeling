
kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)
kingstimeseries <- ts(kings)

plot(kingstimeseries)
title("Age of death of Kings of England from 1087")

acf(diff(kingstimeseries))
pacf(diff(kingstimeseries))

kingsArima <- auto.arima(kingstimeseries)
kingsArima
# Forecast the life of next 5 kings
kingsforecast <- forecast.Arima(kingsArima, h=5)  

plot.forecast(kingsforecast)