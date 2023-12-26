# Question 3

MA1 = arima.sim(n=36, list(ma=-0.5))

ma1 = arima(MA1, order=c(0, 0, 1), method='ML')

plot(MA1)
