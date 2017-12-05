
# Lets plot the full dataset 
plot(Nile)
#Dataset has 100 points

# Fit auto.arima to the first 96 points 
fitNile <- auto.arima(Nile[1:96])
fitNile

#Now we predict last 4 points using the fit
plot(forecast(fitNile,h=4))
lines(97:100,Nile[97:100],col="red")
