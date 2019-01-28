
library(forecast)
library(ggplot2)
y=read.csv("Data.csv")
head(y,3)
tail(y,3)
y.ts=ts(y$Adj.Close,start=c(2000,12),end=c(2018,11),frequency =12 )
y.ts
##Part one
autoplot(y.ts)+geom_point()

## Part two

######################Scenario one

# Create training and testing setsusing function window

train1.start=c(2000,12)
train1.end=c(2007,12)

test1.start=c(2008,1)
test1.end=c(2009,12)

train1.ts=window(y.ts,start=train1.start, end=train1.end)
test1.ts=window(y.ts,start=test1.start,end=test1.end)

nTrain1=length(train1.ts)
nTest1=length(test1.ts)

##################################################################################################
# M1 = SEASONAL NAIVE MODEL
##################################################################################################
# Build a seasonal naive model using training set and label it as M1
M1=snaive(train1.ts,h=nTest1,level=95,lambda="auto")

round(accuracy(M1,test1.ts)[,c("RMSE","MAPE")],2)
checkresiduals(M1)

##################################################################################################
# M2 = REGRESSION MODEL
##################################################################################################
###built in the excel

##################################################################################################
# M3 = SMOOTHING MODEL
##################################################################################################

# Build the best smoothing model
M3=ets(train1.ts,lambda="auto")

M3F=forecast(M3,h=nTest1,level=95)

round(accuracy(M3F,test1.ts)[,c("RMSE","MAPE")],2) # rounded to 2nd decimal digit
checkresiduals(M3)
##################################################################################################
# M4 = ARIMA MODEL
##################################################################################################

# Build the best arima model
M4=auto.arima(train1.ts,lambda="auto")

M4
# Generate forecast
M4F=forecast(M4,h=nTest1,level=95)

# We may print the forecasts and corresponding 95 prediction intervals [lower bound,Upper bound]
M4F

# Accuracy metrics
round(accuracy(M4F,test1.ts)[,c("RMSE","MAPE")],2) # rounded to 2nd decimal digit

# residual diagnostics
checkresiduals(M4)

######################Scenario Two

# Create training and testing setsusing function window

train2.start=c(1992,1)
train2.end=c(2017,9)

test2.start=c(2017,10)
test2.end=c(2018,9)

train2.ts=window(y.ts,start=train2.start, end=train2.end)
test2.ts=window(y.ts,start=test2.start,end=test2.end)

nTrain2=length(train2.ts)
nTest2=length(test2.ts)

##################################################################################################
# M5 = SEASONAL NAIVE MODEL
##################################################################################################
# Build a seasonal naive model using training set and label it as M1
M5=snaive(train2.ts,h=nTest2,level=95,lambda="auto")

round(accuracy(M5,test2.ts)[,c("RMSE","MAPE")],2)
checkresiduals(M5)
##################################################################################################
# M6 = REGRESSION MODEL
##################################################################################################
###built in the excel

##################################################################################################
# M7 = SMOOTHING MODEL
##################################################################################################

# Build the best smoothing model
M7=ets(train2.ts,lambda="auto")

M7F=forecast(M7,h=nTest2,level=95)

round(accuracy(M7F,test2.ts)[,c("RMSE","MAPE")],2) # rounded to 2nd decimal digit
checkresiduals(M7)

##################################################################################################
# M8 = ARIMA MODEL
##################################################################################################

# Build the best arima model
M8=auto.arima(train2.ts,lambda="auto")

M8
# Generate forecast
M8F=forecast(M8,h=nTest2,level=95)

# We may print the forecasts and corresponding 95 prediction intervals [lower bound,Upper bound]
M8F

# Accuracy metrics
round(accuracy(M8F,test2.ts)[,c("RMSE","MAPE")],2) # rounded to 2nd decimal digit

# residual diagnostics
checkresiduals(M8)


##################################################################################################
# M = Champion model to forecast future values for the next  years 
##################################################################################################


Mbest=snaive(y.ts,level=95,lambda="auto")

MF.snaive=forecast(Mbest,h=12,level=95)

autoplot(MF.snaive)+geom_point()

autoplot(MF.snaive)+autolayer(fitted(MF.snaive))




