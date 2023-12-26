library(TSA)
#Question 6
par(mfrow=c(1,2))

# Part A

series=arima.sim(n=72,list(ar=c(0.7,-0.4)))
phi1=0.7
phi2=-0.4
ACF=ARMAacf(ar=c(phi1,phi2),lag.max=72)


AR2 <- ARMAacf(ar = c(0.7, -0.4), lag.max = 72)
plot(ACF, xaxp=c(0,20,10), xlim = c(0, 20), ylim=c(-0.5, 1), type = "h",  ylab = "ACF", main=expression(paste('Theoretical AR(2),', ~~~phi[1], '=0.7', ~~phi[2], '= - 0.4' )))
abline(h=0.2357, col="blue", lty=2)
abline(h=-0.2357, col="blue", lty=2)
abline(h=0)
# Part B same as Part D

w1=rnorm(200, 0, 1)
AR21=vector(length=72)
phi1=0.7
phi2=-.4
AR21[1]=w1[1]
AR21[2]=w1[2]
for(i in 3:72){
  AR21[i]=phi1*AR21[i-1]+phi2*AR21[i-2]+w1[i]
}

acf(ACF, xaxp=c(0,20,10),  main=expression(paste('AR(2),', ~~~phi[1], '=0.7', ~~phi[2], '= - 0.4' )))

acf(AR21,xaxp=c(0,20,10),  main=expression(paste('AR(2),', ~~~phi[1], '=0.7', ~~phi[2], '= - 0.4' )))

# We can see from the side by side plots that according to the ACf plots they
# both look to be MA(1) models, however the decaying sine pattern of both plots
# should indicate that this could be PACF. They do somewhat follow eachother, 
# however the sample AR(2) model decays much slower. The theoretical plot, as
# we can see, decay much quicker.

# Part C

AR2p <- ARMAacf(ar = c(0.7, -0.4), lag.max = 72, pacf = TRUE)
AR2p[1:3]

plot(AR2p,type = "h",  ylab = "PACF", main=expression(paste('AR(2),', ~~~phi[1], '=0.7', ~~phi[2], '= - 0.4' )))
abline(h=0.3179, col="blue", lty=2)
abline(h=-0.3179, col="blue", lty=2)
abline(h=0)

# Part E

pacf(ACF)
pacf(AR21,xaxp=c(0,20,10),  main=expression(paste('AR(2),', ~~~phi[1], '=0.7', ~~phi[2], '= - 0.4' )))

# We can see that in the theoretical PACF plot that the PACF values after the 
# first two lags become completely insignificant. They decay so quickly after
# the first two lags that we can't even see them on the plot. However, the sample
# PACF plot shows a similar pattern in that the first two lags are the only ones
# with significant values. On the other hand, at lags 3 or greater, there seems
# to be a bit of an oscillating pattern with the non-significant values.


# Question 9
data("robot")

# Part A

plot(robot, main=expression(paste( Y[t] )))
# The TS plot of the robot data does seem to be stationary. It doesn't look like
# the variance is increasing as time goes on, and there doesn't seem to be a 
# relationship between the mean and time. 

# Part B
par(mfrow = c(1,2))

acf(robot, lag.max = 20, main = "Sample ACF of Robot")
pacf(robot, lag.max = 20, main = "Sample PACF of Robot")
# Based on the ACF and PACF plots, the data does look to be stationary. If it's
# not stationary then the plots would decrease to 00 very rapidly, but as we can
# see the ACF and PACF don't stop at zero even up to the 20th lag. However, based
# on these plots we can see that the data do not follow an MA process. However, 
# the PACF plot looks much more convincing, perhaps an AR(2) or AR(3) process.

# Part C

eacf(robot, ar.max = 7, ma.max = 10)
# Based on the EACF chart we can easily determine that the robot data follows 
# an ARMA(1,1) model. The apex of the 0-triangle is where AR and Ma are at 1 
# giving us p=1 and q=1 for the ARMA model.

# Part D

rob <- armasubsets(robot, nar = 10, nma = 10, y.name = 'test')
plot(rob)
# Based on this plot, we can see that the p and q we got from the EACF are 
# correct and that our best model would be an ARMA(1,1) model. 
