#Question 6
#Part A
library(TSA)

data(winnebago)
plot(winnebago, main = "Winnebago Sales TS", type='o',
     ylab='Sales', lwd=2, col="blue")

#Part B

plot((log(winnebago)), main = "Log Winnebago Sales TS", type = "o",
     ylab = "Log Sales", lwd=2, col="green")

# We can see the difference in the two TS plots. The original y-axis goes into 
# the thousands, but when scaled by the log fuction, we can see that the data
# is more centered. Additionally, the variation around the upward trend is more
# uniform across high and low values. We can see this by taking a look at the
# axes, as time goes on in the initial TS, the variance increases. However, when
# taking the log of the sales data, the variation is much more uniform over time.

#Part C
#layout(matrix(c(1,2,1,3), 2, 2, byrow = TRUE))

plot(winnebago, main = "Winnebago Sales TS", type='o',
     ylab='Sales', lwd=2, col="blue")

plot(((diff(winnebago))/winnebago), main = "Winnebago Fractional Relative Change",
     ylab = expression(paste((Y[t] - Y[t-1])/Y[t-1])))

plot(diff(log(winnebago)), main = "Log Difference Winnebago Sales TS",
     ylab = "Log Difference Sales")

# It looks as though the log difference plot has a more positive variance for
# large values. As seen in the plot, where large values occur on the Fractional
# Relative Change Plot, we can see that those same values have higher spikes. 
# We can also see this on the y-axis, the positive values on the y-axis of the
# log difference plot are larger than that of the other plot.

#Question 7 
#Part A
data("larain")

larain.transf=BoxCox.ar(larain, lambda = seq(-2, 2, 0.1), method = c("mle"))
larain.transf

# Based on the plot I would suggest a lambda = 0.2, but because 0 is in the 
# 95% confidence interval we could also use a lambda = 0.0.

#Part B
lambda <- larain.transf$mle

powerTransform <- function(y, lambda, method = "boxcox"){
  ((y)^lambda - 1) / lambda
}

model <- lm(powerTransform(larain, lambda)~time(larain))

qqnorm(rstudent(model) , lwd=2, col="blue" )
qqline(rstudent(model), col = "red")  

#Part C
plot(powerTransform(larain, lambda), main = "BoxCox LaRain TS", type='o',
     ylab='BoxCox Rain', lwd=2, col="blue")

#Part D
plot(y=powerTransform(larain, lambda),x=zlag(powerTransform(larain, lambda)),
     main = "BoxCox Transform LA rain Yt vs Yt-1", ylab='Yt', 
     xlab='Yt-1', lwd=2, col=4)

# If these transformations are aimed to make these time series stationary, then
# we should expect that there is no dependence between each data point and the
# previous data point. Removing the trend will remove the dependence between
# data points and make the process stationary with a constant mean and the 
# variation of the data points are more uniform over time.

