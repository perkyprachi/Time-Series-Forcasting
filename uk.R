TransportData<- read.csv(file.choose(),header = TRUE)

View(TransportData)
myts   <- ts(TransportData$Total, start=c(1996, 1), end=c(2005, 4), frequency=4)
myts

# plot series
plot(myts)


###Seasonal Decomposition

#A time series with additive trend, seasonal, and irregular components can be decomposed using the stl() function. 
#Note that a series with multiplicative effects can often by transformed into series with additive effects through a 
#log transformation (i.e., newts <- log(myts))
plot(decompose(myts, type = c("multiplicative")))
?decompose()
decompose(myts, type = c("multiplicative"))

fit <- stl(myts, s.window="period")
?stl()
plot(fit)
ls(fit)
print(fit$time.series)
fit$win


# predict next quarter values
library(forecast)
fit<- ets(myts)
accuracy(fit$fitted, myts) #####MAPE= 1.75779
summary(fit)


forecast(fit, 4)
plot(forecast(fit,4))
###       Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
##2006 Q1       18193.29 17530.75 18855.83 17180.03 19206.55
##2006 Q2       24223.19 23224.10 25222.28 22695.22 25751.16
##2006 Q3       29573.43 28226.92 30919.93 27514.13 31632.73
##2006 Q4       21002.51 19964.62 22040.40 19415.20 22589.82

#Exponential Models
#Both the HoltWinters() function in the base installation, and the ets() function in the forecast package, can be used to fit exponential models
#Forecast package
# simple exponential - models level
fit1 <- HoltWinters(myts, beta=FALSE, gamma=FALSE)
ls(fit1)
install.packages("forecast")
require(forecast)
accuracy(fit1$fitted, myts)  #####MAPE= 18.10978,    MPE = 4.248278

# double exponential - models level and trend
fit2 <- HoltWinters(myts, gamma=FALSE)
accuracy(fit2$fitted, myts) ###MAPE= 23.51175

# triple exponential - models level, trend, and seasonal components
fit3 <- HoltWinters(myts)
accuracy(fit3$fitted, myts)
###MAPE= 2.917
forecast(fit3, 4)
#########Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
#2006 Q1       19009.79 18223.12 19796.46 17806.69 20212.89
#2006 Q2       24086.86 23237.50 24936.22 22787.88 25385.84
#2006 Q3       28650.73 27743.00 29558.47 27262.47 30038.99
#2006 Q4       21373.84 20411.26 22336.41 19901.71 22845.96

######Finding Autocorrelation

require(tseries)
acf(myts)  ##taking P=2
acf(myts,5)##P=1
pacf(myts,5)###taking Q=1

####Differencing needed to make the data stationary

ndiffs(myts)   #### D=1

##Checking whether Data is stationary or not by Augmented Dickey-Fuller test

adf.test(myts)

#Dickey-Fuller = -0.71971, Lag order = 3, p-value = 0.9601
#alternative hypothesis: stationary
## Data is non stationary

# fit an ARIMA model of order P, D, Q
fitA <- arima(myts, order=c(2,1,1))
summary(fitA) ####MAPE=8.1745
plot(forecast(fitA, 4))


# Automated forecasting using an ARIMA model
fitB <- auto.arima(myts)
summary(fitB)   ###MAPE=2.429914

Final_Prediction<- data.frame(forecast(fitB,4))
View(Final_Prediction)
write.csv(Final_Prediction,file = "D:/AnalitixLabs/New folder/Time Series/Final_Prediction.csv")
plot(forecast(fitB, 4))

