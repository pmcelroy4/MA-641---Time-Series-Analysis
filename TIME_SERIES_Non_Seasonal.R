#Time Series Non-Seasonal Dataset: Ethereum price
library(rugarch)
library(TSA)
library(forecast)
library(CADFtest)

#Read the data (Cleaning involved taking the monthly average of the closing price)
data <- read.csv("cleaned_file.csv", header=TRUE)
data
data1 <- data$Close

#Summary of the data
head(data)
summary(data)
Data <- ts(data1, frequency = 12, start = c(2019, 1))

#Plot the time-series
plot(Data,type='o',ylab='Closing Price', main='Ethereum Closing Price Since 2019', lwd=2, col="blue")
BoxCox.ar(Data, lambda = seq(-2, 2, 0.1), method = c("mle"))

acf(Data, main = 'ACF of Closing Price')
pacf(Data, main = 'PACF of Closing Price')
eacf(Data)



#Augmented Dicky-Fuller Test of Original Series
CADFtest(Data)

#First Difference
new <- diff(Data)
CADFtest(new)
plot(new, ylab='Closing Price', main = 'First Difference of Closing Price', type = 'l')

#Second Difference
new_data <- diff(Data, differences = 2)
CADFtest(new_data)
plot(new_data, ylab='Closing Price', main = 'Second Difference of Closing Price', type = 'l')

#Data Transformation: Log of the First Difference
transform <- diff(log(Data))
CADFtest(transform)
plot(transform, ylab='Log(Price)', main = 'First Difference of Log(Data)', type='l')

#Return Squared
return <- transform^2
CADFtest(return)
plot(return, ylab='Log(Price)', main = 'Squared First Difference of Log(Data)', type='l')

#Data Transformation: Taking the log of 2 differences
transformed_data<-diff(log(Data), differences = 2)
CADFtest(transformed_data)
plot(transformed_data,ylab='Log(Price)', main = 'Twice Defferenced Log(Data)', type='l')

#Model specification
#First Difference
acf(new, main='ACF of First Difference')
pacf(new, main='PACF of First Difference')
eacf(new)
#No Good Candidate Models

#Second Difference
acf(new_data, main='ACF of Second Difference')
pacf(new_data, main='PACF of Second Difference')
eacf(new_data)
#No good models

#Log of First Difference
acf(transform, main='ACF of Log First Difference')
pacf(transform, main='PACF of Log First Difference')
eacf(transform)
#ARIMA(1,2,3)

#(Return)^2
acf(return, main = 'ACF of Log First Difference Squared')
pacf(return, main = 'PACF of Log First Difference Squared')
eacf(return)
#GARCH(1,1), GARCH(2,1), GARCH(2,2)

#Log of Second Difference
acf(transformed_data, main='ACF of Log Second Difference')
pacf(transformed_data, main='PACF of Log Second Difference')
eacf(transformed_data)
#IMA(2,2), IMA(2,3), ARIMA(1,2,3), ARIMA(2,2,3)
# Candidate models ARIMA{(0,2,2), (0,2,3), (1,2,3), (2,2,3)}, GARCH{(1,1), (2,1), (2,2)}

#ARIMA Models
#1st Model IMA(2,2)
model1<-Arima(transformed_data, order = c(0,2,2), method='ML')
model1
plot(rstandard(model1), ylab = "Standardized Residuals", main = 'IMA(2,2)', type = "l"); abline(h = 0)

qqnorm(residuals(model1)); qqline(residuals(model1))
acf(residuals(model1))

tsdiag(model1,gof=30,omit.initial=F)

#2nd Model IMA(2,3)
model2<-Arima(transformed_data, order = c(0,2,3), method='ML')
model2
plot(rstandard(model2), ylab = "Standardized Residuals", main = 'IMA(2,3)', type = "l"); abline(h = 0)

qqnorm(residuals(model2)); qqline(residuals(model2))
acf(residuals(model2))

tsdiag(model2,gof=30,omit.initial=F)

#3rd Model ARIMA(1,2,3)
model3<-arima(transform, order = c(1,2,3), method='ML')
model3

plot(rstandard(model3), ylab = "Standardized Residuals", main = 'ARIMA(1,2,3)', type = "l"); abline(h = 0)

qqnorm(residuals(model3)); qqline(residuals(model3))
acf(residuals(model3))

tsdiag(model3,gof=30,omit.initial=F)

#4th Model ARIMA(2,2,3)
model4<-Arima(transformed_data, order = c(2,2,3), method='ML')
model4

plot(rstandard(model4), ylab = "Standardized Residuals", main = 'ARIMA(2,2,3)', type = "l"); abline(h = 0)

qqnorm(residuals(model4)); qqline(residuals(model4))
acf(rstandard(model4))

tsdiag(model4,gof=30,omit.initial=F)

#GARCH Models
#5th Model: GARCH(1,1)
model5 <- garch(x=return, order = c(1,1))
summary(model5)

plot(residuals(model5), ylab = "Standardized Residuals", main = 'GARCH(1,1)', type = "l"); abline(h = 0)

qqnorm(residuals(model5)); qqline(residuals(model5))
acf(abs(residuals(model5)),na.action=na.omit, main = "Residuals of GARCH(1,1)")
pacf(abs(residuals(model5)),na.action=na.omit, main = "Residuals of GARCH(1,1)")

gBox(model5, lags = 1:20, x=return, method = 'squared')

AIC0 <- AIC(model5)

#6th Model: GARCH(2,1)
model6 <- garch(x=return, order = c(2,1))
summary(model6)

plot(residuals(model6), ylab = "Standardized Residuals", main = 'GARCH(2,1)', type = "l"); abline(h = 0)

qqnorm(residuals(model6)); qqline(residuals(model6))
acf(residuals(model6)^2,na.action=na.omit, main = "Residuals of GARCH(2,1)")
pacf(residuals(model6)^2,na.action=na.omit, main = "Residuals of GARCH(2,1)")

gBox(model6, lags = 1:20, x=return, method = 'squared')

AIC <- AIC(model6)

#7th Model: GARCH(2,2)
model7 <- garch(return, order = c(2,2))
summary(model7)

plot(residuals(model7), ylab = "Standardized Residuals", main = 'GARCH(2,2)', type = "l"); abline(h = 0)

qqnorm(residuals(model7)); qqline(residuals(model7))
acf(residuals(model7)^2,na.action=na.omit)

gBox(model7, lags = 1:20, x=return, method = 'squared')

AIC1 <- AIC(model7)

#Model Performance Based on AIC
aics <- cbind(c(round(model1$aic,2), round(model2$aic,2), round(model3$aic,2), 
                round(model4$aic,2), round(AIC0,2), round(AIC,2), round(AIC1,2)))
models <- cbind(c('IMA(2,2)', 'IMA(2,3)', 'ARIMA(1,2,3)', 'ARIMA(2,2,3)',
                  'GARCH(1,1)', 'GARCH(2,1)', 'GARCH(2,2)'))
results <- cbind(models, aics)
colnames(results) <- c('Model', 'AIC')
results <- data.frame(results)
results

#Forecast Models
#Model 4
Model4 <- diffinv(transformed_data, lag = 1, differences = 2)
fc.m4 <- forecast(Model4, h = 25,level=c(85,99))
plot(fc.m4, main = "Forecasts from ARIMA(2,2,3)")
summary(fc.m4)

#Model 5
garch <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1), 
                                          submodel = NULL, external.regressors = NULL, 
                                          variance.targeting = FALSE), 
                    mean.model = list(armaOrder = (c(1,1)), external.regressors = NULL))
m5 <- ugarchfit(spec = garch, data = Data, solver.control = list(trace=0))
m5@fit$coef
garch_forecast = ugarchforecast(fitORspec = m5)
plot(garch_forecast, which = 1)

#Model 6
Garch <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,1), 
                                          submodel = NULL, external.regressors = NULL, 
                                          variance.targeting = FALSE), 
                    mean.model = list(armaOrder = (c(1,1)), external.regressors = NULL))
m6 <- ugarchfit(spec = Garch, data = Data, solver.control = list(trace=0))
m6@fit$coef
Garch_forecast = ugarchforecast(fitORspec = m6)
plot(Garch_forecast, which = 1)

