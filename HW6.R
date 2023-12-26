
#Question 2
MA1 = arima.sim(n=48, list(ma=-0.8))

#Part A
estimate.ma1.mom=function(x){r=acf(x,plot=FALSE)$acf[1]; if (abs(r)<0.5) 
return((-1+sqrt(1-4*r^2))/(2*r)) else return(NA)}

estimate.ma1.mom(MA1)

#Part B
arima(MA1, order=c(0, 0, 1), method='CSS')

# The MOM estimation gave a theta=0.6029. This is extremely far off considering that
# the theta value of the simulated MA1 model is -0.8. The conditional least
# squares method gave a much better estimate for theta with -0.8485. 

#Part C
arima(MA1, order=c(0, 0, 1), method='ML')

# We can see that the theta estimate from MLE was -0.8479. This estimate is again
# much better than the MOM estimate, and slightly better than the conditional
# least squares estimate of -0.8485.

#Part D
acf(MA1)
# We can see from the plot that there is only one significant lag, consistent 
# with this being an MA(1) process.

#Part E
for (i in 1:500){
  newma1 = arima.sim(n = 48, list(ma = 0.8))
}

mean(newma1)
# Mean of the 500 sample MA(1) model is -0.01999
#a
estimate.ma1.mom(newma1)

#b
arima(newma1, order=c(0, 0, 1), method='CSS')
# The CSS estimate performs much better with a theta estimate of -0.7691, whereas
# the MOM estimate is 0.6057 which is very off from -0.8.

#c
arima(newma1, order=c(0, 0, 1), method='ML')
# The MLE estimate performs worse than the CSS estimate, the MLE resulted in a 
# theta estimate of -1.0 which is much further off from -0.8 compared to -0.7691.

# Question 3
AR2 = arima.sim(n=60, list(ar=0.6, 0.3))

#Part A
ar(AR2,order.max=2,AIC=F,method='yw')

#Part B
ar(AR2,order.max=2,AIC=F,method='ols')
# We can see that the MOM estimation and the least squares estimation select an 
# order 1 AR model. The coefficient estimates by the MOM was 0.6869, but the 
# least squares estimate was slightly better with a value of 0.6875.

#Part C
ar(AR2,order.max=2,AIC=F,method='mle')
# Order 1 was also calculated using the MLE estimation, however this estimate was
# better than the least squares with a value of 0.6787. We can see though, that 
# this estimation is better than that of the MOM estimation as well.

#Part D
pacf(AR2)
# We can see from the PACF plot that this process is in fact an AR(1) process.
# After seeing this plot, it makes sense that the estimations of the coefficients
# only gave 1 value, calculating that this is an AR(1) process.

#Part E
for (i in 1:500){
  newar2 = arima.sim(n = 48, list(ar = 0.6, 0.3))
}

mean(newar2)
#a
ar(newar2,order.max=2,AIC=F,method='yw')

#b
ar(newar2,order.max=2,AIC=F,method='ols')

#c
ar(newar2,order.max=2,AIC=F,method='mle')

#Question 4
ARMA11 = arima.sim(n=72, list(ar=0.7, ma=0.4))

#Part A
estimate.ma1.mom(ARMA11)
ar(ARMA11,order.max=1,AIC=F,method='yw')

#Part B 
arima(ARMA11, order = c(1,0,1), method='CSS')
# The MOM estimate of the ARMA(1,1) model gave an NA for the theta estimate and
# 0.8091 for the phi estimate. The Conditional Sum of Squares estimate performed
# much better with 0.6899 for phi and 0.465 for theta. 

#Part C
arima(ARMA11, order = c(1,0,1), method='ML')
# The MLE performed better than MOM as well, but gave similar results to the CSS
# estimation. The MLE resulted in a worse phi estimation with 0.6821, but a 
# slightly better estimation that CSS for theta with 0.4615. 

#Part D
eacf(ARMA11)
# As we can see from the EACF chart, this does suggest an ARMA(1, 1) model. 

#Part E
for (i in 1:500){
  newarima = arima.sim(n = 72, list(ar = 0.7, ma = 0.4))
}

mean(newarima)
#a
estimate.ma1.mom(newarima)
ar(newarima,order.max=1,AIC=F,method='yw')

#b
arima(newarima, order = c(1,0,1), method='CSS')
# The MOM estimate of the new ARMA(1,1) model gave an NA for the theta estimate and
# 0.7939 for the phi estimate. The Conditional Sum of Squares estimate performed
# better with a phi estimate of 0.7097 and theta estimate of 0.3482.

#c
arima(newarima, order = c(1,0,1), method='ML')
# The MLE performed better than MOM as well, but gave similar results to the CSS
# estimation. The MLE resulted in a phi estimate of 0.7046 and a theta estimate 
# of 0.3415. The estimates are similar, but we can see that MLE is the best.


