#install.packages('TSA')
library(TSA)

#Quesiton 6

#Part A
data("winnebago")
plot(winnebago, main = "Winnebago Sales TS", type='o',
     ylab='Sales', lwd=2, col="blue")
# We can see that there seems to be a positive trend, but this could be due to 
# the correlation between nearby points. Additionally, the variance of the sales
# seems to increase as time goes on showing properties of a stochastic trend.

#Part B
model = lm(winnebago~time(winnebago))
summary(model)

abline(model)

plot(y=rstudent(model),x=as.vector(time(winnebago)),
     main = "Winnebago Residuals vs Time", xlab='Time',
     ylab='Standardized Residuals',type='o', lwd=2, col="blue")
# Similar to what we saw with the Sales vs Time plot above, the standard residuals
# seem to increase dramatically with time. We will determine if the residuals follow
# a normal distribution and if the increase is significant.

#Part C
plot(log(winnebago), main = "Logarithm Winnebago Sales TS", type='o',
     ylab='Sales', lwd=2, col="blue")
# In this plot we can see that the variance seems much more consistent throughout
# the time series. The natural log seems to have scaled the sales TS.

#Part D
model1 = lm(log(winnebago)~time(winnebago))
summary(model1)

abline(model1)

plot(y=rstudent(model1),x=as.vector(time(winnebago)),
     main = "Log Winnebago Residuals vs Time", xlab='Time',
     ylab='Standardized Residuals',type='o', lwd=2, col="blue")
# Again similar to the log TS plot, we can see that the residuals seem like they
# were scaled in order to center the variance. It seems that the variance is 
# more equally distributed in this plot than the original residual plot.

#Part E
#Runs Test
runs(rstudent(model))
runs(rstudent(model1))
# We can see that the p-values calculated for both the original residuals and the
# log residuals that we cannot reject the null hypothesis. This means that both
# the residuals and log residuals are random sequences.

#Shapiro - Wilk Test
shapiro.test(rstudent(model))
shapiro.test(rstudent(model1))
# We can see from the results of the Shapiro-Wilk test that the original(non-log) 
# residuals follow a normal distribution with a p-value = 0.001075 < 0.05. On 
# the other hand the log residuals had a p-value = 0.3498 > 0.05 meaning we can 
# reject the null hypothesis meaning the log residuals are non-normal.

#Part F
acf(rstudent(model), main = "Sample ACF for Model", lwd=2, col="blue")
acf(rstudent(model1), main = "Sample ACF for Log Model", lwd=2, col="blue")

#Part H
#Histograms
hist(rstudent(model),xlab='Standardized Residuals', lwd=2)
hist(rstudent(model1),xlab='Standardized Residuals', lwd=2)

#QQPLOTS
qqnorm(rstudent(model) , lwd=2, col="blue" )
qqline(rstudent(model), col = "red")

qqnorm(rstudent(model1) , lwd=2, col="blue" )
qqline(rstudent(model1), col = "red")

