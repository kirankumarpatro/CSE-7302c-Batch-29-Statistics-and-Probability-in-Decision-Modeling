setwd("F:\\Users\\AnandJayaraman\\Documents\\Research\\Talks n Presentations\\Insofe\\RegressionTimeSeries\\2017-01-14\\Day3")

# Read in Flier Response Data
flierresponse <- read.csv("FlierResponse.csv", header = T, sep = ",")
flierresponse
str(flierresponse)
flierresponse$Response <- as.factor(flierresponse$Response)
str(flierresponse)
flierresponseglm <- glm(Response~Age, data = flierresponse, family = "binomial")
flierresponseglm
summary(flierresponseglm)

#plot the residuals
par(mfrow=c(2,2))
plot(flierresponseglm)

#Understand the Residuals
fittedValues <- flierresponseglm$fitted.values
residualVal <- flierresponseglm$residuals
ActualResponse <- as.numeric(flierresponse$Response)
par(mfrow=c(1,1))
plot(fittedValues,residualVal,col=c("blue","red")[ActualResponse])
abline(h=0,lty=2,col="grey")
lines(lowess(fittedValues,residualVal),col="black",lwd=2)

