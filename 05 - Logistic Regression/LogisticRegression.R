
dirpath <- paste0(INSOFE_DIR,"RegressionTimeSeries\\2017-04-22 Batch28\\Day3")
setwd(dirpath)

library(MASS)
library(ggplot2)

# LOGISTIC REGRESSION

# Read in Flier Response Data
flierresponse <- read.csv("FlierResponse.csv", header = T, sep = ",")
flierresponse
str(flierresponse)
flierresponse$Response <- as.factor(flierresponse$Response)
str(flierresponse)
flierresponseglm <- glm(Response~Age, data = flierresponse, family = "binomial")
flierresponseglm
summary(flierresponseglm)
logLik(flierresponseglm)
deviance(flierresponseglm)
AIC(flierresponseglm)

flierresponseglm <- glm(Response~1, data = flierresponse, family = "binomial")
flierresponseglm
summary(flierresponseglm)

#=======================================================================

#-------Term Deposit Example -----

subscribetermdeposit <- read.csv("bank-full.csv", header = T, sep = ";")
#subscribetermdeposit
str(subscribetermdeposit)
subscribetermdepositglm <- glm(y ~ age + job + marital + education + default
                               + balance + housing + loan + contact + day
                               + month + duration + campaign + pdays
                               + previous + poutcome, data = subscribetermdeposit,
                               family = "binomial")
subscribetermdepositglm
#predict(subscribetermdepositglm,newdata=Test)
summary(subscribetermdepositglm)

stepOut<- stepAIC(subscribetermdepositglm, direction = "both")
car::vif(stepOut)
predict(stepOut, new)

#=======================================================================

# CASE STUDY - The Framingham Heart Study

# Read in the Framingham dataset
framingham = read.csv("framingham.csv")

# Look at structure
str(framingham)

# Load the library caTools
library(caTools)

# Randomly split the data into training and testing sets
set.seed(1000)
split = sample.split(framingham$TenYearCHD, SplitRatio = 0.70)

# Split up the data using subset
train = subset(framingham, split==TRUE)
test = subset(framingham, split==FALSE)

#Check the frequency of CHD in both sets
cat(sum(train$TenYearCHD)/nrow(train),sum(test$TenYearCHD)/nrow(test))

# Logistic Regression Model
framinghamLog = glm(TenYearCHD ~ ., data = train, family=binomial)
summary(framinghamLog)

#There seems to be missing values. Lets find out which variables have missing values.
colSums(is.na(framingham))


missingVar=c("education","cigsPerDay","BPMeds","totChol","BMI","heartRate","glucose")

#Lets see how many of the rows have 3 or more variables missing
which(rowSums(is.na(framingham[,missingVar]))>=3)

#Lets remove those datapoints
rowindx <- which(rowSums(is.na(framingham[,missingVar]))>=3)
framingham <- framingham[-rowindx,]

# Create the function to compute Mode.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# A function to replace missing values with the mode
replaceNAwithMode <- function(v) {
  modeval <- getmode(v)
  v[is.na(v)] <- modeval
  v
}
# A function to replace missing values with the median
replaceNAwithMedian <- function(v) {
  v[is.na(v)] <- median(v,na.rm=T)
  v
}


#Lets fill all the missing categorical variables with the Mode
framingham$education <- replaceNAwithMode(framingham$education)
framingham$BPMeds <- replaceNAwithMode(framingham$BPMeds)

#Lets fill all the missing numerical variables with the Median
ggplot(framingham,aes(x=cigsPerDay))+geom_density() +geom_vline(aes(xintercept=median(cigsPerDay,na.rm=T)),color='red',linetype='dashed')
framingham$cigsPerDay <- replaceNAwithMedian(framingham$cigsPerDay)

ggplot(framingham,aes(x=totChol))+geom_density() +geom_vline(aes(xintercept=median(totChol,na.rm=T)),color='red',linetype='dashed')
framingham$totChol <- replaceNAwithMedian(framingham$totChol)

ggplot(framingham,aes(x=BMI))+geom_density() +geom_vline(aes(xintercept=median(BMI,na.rm=T)),color='red',linetype='dashed')
framingham$BMI <- replaceNAwithMedian(framingham$BMI)

ggplot(framingham,aes(x=heartRate))+geom_density() +geom_vline(aes(xintercept=median(heartRate,na.rm=T)),color='red',linetype='dashed')
framingham$heartRate <- replaceNAwithMedian(framingham$heartRate)

ggplot(framingham,aes(x=glucose))+geom_density() +geom_vline(aes(xintercept=median(glucose,na.rm=T)),color='red',linetype='dashed')
framingham$glucose <- replaceNAwithMedian(framingham$glucose)

#Lets redo the regression again!
set.seed(1000)
split = sample.split(framingham$TenYearCHD, SplitRatio = 0.70)

# Split up the data using subset
train = subset(framingham, split==TRUE)
test = subset(framingham, split==FALSE)


framinghamFull = glm(TenYearCHD ~ ., data = train, family=binomial)
summary(framinghamFull)

framinghamLog <- stepAIC(framinghamFull)
summary(framinghamLog)

#Accuracy on the training set
predictTrain = predict(framinghamLog, type="response", newdata=train)

# Confusion matrix with threshold of 0.5
table(train$TenYearCHD, predictTrain > 0.5)
t1 <- table(train$TenYearCHD, predictTrain > 0.5)

# Accuracy on Train Set
(t1[1,1]+t1[2,2])/sum(t1)

# Predictions on the test set
predictTest = predict(framinghamLog, type="response", newdata=test)

# Confusion matrix with threshold of 0.5
table(test$TenYearCHD, predictTest > 0.5)
t2<-table(test$TenYearCHD, predictTest > 0.5)
# Accuracy on Test Set
(t2[1,1]+t2[2,2])/sum(t2)


#  True Positive Rate
t2[2,2]/(t2[2,1] + t2[2,2])


#Is there a different threshold that will maximize Kappa value or Accuracy Value
for (p in seq(0.2,0.8,0.05)) {
  t1 <- table(train$TenYearCHD, predictTrain > p)
  totalAcc <- sum(diag(t1))/sum(t1)
  ActualTruPerc<- sum(train$TenYearCHD)/length(train$TenYearCHD)
  ActualFalPerc<- 1 - ActualTruPerc
  PredTrue <- sum(predictTrain > p)/length(predictTrain)
  PredFalse <- 1-PredTrue
  RandomAcc <- ActualTruPerc*PredTrue + ActualFalPerc*PredFalse
  
  Kappa <- (totalAcc-RandomAcc)/(1-RandomAcc)
  cat(p, Kappa,totalAcc, "\n")
}


# Confusion matrix with threshold of 0.9
table(test$TenYearCHD, predictTest > 0.9)
# Confusion matrix with threshold of 0.7
table(test$TenYearCHD, predictTest > 0.7)
# Confusion matrix with threshold of 0.5
table(test$TenYearCHD, predictTest > 0.5)
# Confusion matrix with threshold of 0.3
table(test$TenYearCHD, predictTest > 0.3)
# Confusion matrix with threshold of 0.1
table(test$TenYearCHD, predictTest > 0.1)

# Test set AUC 
library(ROCR)
ROCRpred = prediction(predictTest, test$TenYearCHD)
as.numeric(performance(ROCRpred, "auc")@y.values)
ROCRperf <- performance(ROCRpred, "tpr", "fpr")
par(mfrow=c(1,1))
plot(ROCRperf, colorize = TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))


# # RECODE AGE AND REBUILD MODEL
# 
# # Read in the Framingham dataset
# framingham = read.csv("framingham.csv")
# 
# # Look at structure
# str(framingham)
# 
# # Recode Age
# lowRisk <- which(framingham$age < 40)
# medRisk <- which(framingham$age >= 40 & framingham$age < 50)
# highRisk <- which(framingham$age >= 50)
# framingham[lowRisk,"age"] <- "1"
# framingham[medRisk,"age"] <- "2"
# framingham[highRisk, "age"] <- "3"
# framingham$age <- as.factor(framingham$age)
#   
# # Load the library caTools
# library(caTools)
# 
# # Randomly split the data into training and testing sets
# set.seed(1000)
# split = sample.split(framingham$TenYearCHD, SplitRatio = 0.70)
# 
# # Split up the data using subset
# train = subset(framingham, split==TRUE)
# test = subset(framingham, split==FALSE)
# 
# # Logistic Regression Model
# framinghamLog = glm(TenYearCHD ~ ., data = train, family=binomial)
# summary(framinghamLog)
# 
# #Accuracy on the training set
# predictTrain = predict(framinghamLog, type="response", newdata=train)
# 
# # Confusion matrix with threshold of 0.5
# table(train$TenYearCHD, predictTrain > 0.5)
# 
# # Accuracy
# (2170+30)/(2170+30+357+9)
# 
# # Predictions on the test set
# predictTest = predict(framinghamLog, type="response", newdata=test)
# 
# # Confusion matrix with threshold of 0.5
# table(test$TenYearCHD, predictTest > 0.5)
# 
# # Accuracy
# (915+12)/(915+12+158+7)
# 
# # Test set AUC 
# library(ROCR)
# ROCRpred = prediction(predictTest, test$TenYearCHD)
# as.numeric(performance(ROCRpred, "auc")@y.values)
