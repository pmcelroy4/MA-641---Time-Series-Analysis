library(TSA)

#Question 1
#Part A
set.seed(42)
theta1 = 0.5
theta2 = 0.4

sketch <- ARMAacf(ma = list(theta1, theta2), lag.max = 10)
sketch
acf(sketch, main = "ACf plot for MA(2) with Theta = 0.5, 0.4")

w = rnorm(200, 0, 1)
MA = vector(length = 150)

for(i in 1:150){
  MA[i]=w[50+i]-(theta1*w[50+i-1])-(theta2*w[50+i-2])
}

par(mfrow=c(1,1))

plot(time(MA), MA, main = "TS of MA(2) with Theta = 0.5, 0.4", 
     ylab=expression(Y[t]), xlab='Time', type='o')

plot(y=MA,x=zlag(MA),ylab=expression(Y[t]), xlab=expression(Y[t-1]),type='p')
plot(y=MA,x=zlag(MA,2),ylab=expression(Y[t]), xlab=expression(Y[t-2]),type='p')
plot(y=MA,x=zlag(MA,3),ylab=expression(Y[t]), xlab=expression(Y[t-3]),type='p')

acf(MA)

#Part B
set.seed(42)
theta3 = 1.2
theta4 = -0.7

sketch1 = ARMAacf(ma = list(theta3, theta4), lag.max = 10)
sketch1
acf(sketch1, main = "ACf plot for MA(2) with Theta = 1.2, -0.7")

MA1 = vector(length = 150)

for(i in 1:150){
  MA1[i]=w[50+i]-(theta3*w[50+i-1])-(theta4*w[50+i-2])
}

par(mfrow=c(1,1))

plot(time(MA1), MA1, main = "TS of MA(2) with Theta = 1.2, -0.7", 
     ylab=expression(Y[t]), xlab='Time', type='o')

plot(y=MA1,x=zlag(MA1),ylab=expression(Y[t]), xlab=expression(Y[t-1]),type='p')
plot(y=MA1,x=zlag(MA1,2),ylab=expression(Y[t]), xlab=expression(Y[t-2]),type='p')
plot(y=MA1,x=zlag(MA1,3),ylab=expression(Y[t]), xlab=expression(Y[t-3]),type='p')

acf(MA1)

#Question 4
#Part A
set.seed(42)
psi1 = 0.7
theta5 = 0.4

sketchARMA = ARMAacf(ar = psi1, ma = theta5, lag.max = 10)
sketchARMA
acf(sketchARMA, main = "ACF plot for ARMA(1,1) with Phi = 0.7 Theta = 0.4")

ARMA11=arima.sim(n = 150, list(ar = c(0.7), ma = c(0.4)))

par(mfrow=c(1,1))
plot(ARMA11,xlab='Time', ylab=expression(Y[t]),type='o', 
     main=expression(paste("ARMA(1,1)",  ~~ phi,"=.7", ~~~theta, "=.4") ) )

acf(ARMA11, main = "Sample ACF plot for ARMA(1,1) with Phi = 0.7 Theta = 0.4")

#Part B
set.seed(42)
psi2 = 0.7
theta6 = -0.4

sketchARMA1 = ARMAacf(ar = psi2, ma = theta6, lag.max = 10)
sketchARMA1
acf(sketchARMA1, main = "ACF plot for ARMA(1,1) with Phi = 0.7 Theta = -0.4")

ARMA11_2=arima.sim(n = 150, list(ar = c(0.7), ma = c(-0.4)))

par(mfrow=c(1,1))
plot(ARMA11_2,xlab='Time', ylab=expression(Y[t]),type='o', 
     main=expression(paste("ARMA(1,1)",  ~~ phi,"=.7", ~~~theta, "=-.4") ) )

acf(ARMA11_2, main = "Sample ACF plot for ARMA(1,1) with Phi = 0.7 Theta = -0.4")

#Question 3
#Part A
w1=rnorm(200, 0, 1)
AR21=vector(length=150)
phi1=.5
phi2=.25
AR21[1]=w1[1]
AR21[2]=w1[2]
for(i in 3:150){
  AR21[i]=phi1*AR21[i-1]+phi2*AR21[i-2]+w1[i]
}

plot(AR21,xlab='Time', ylab=expression(Y[t]),type='o')

z <- c(1, 2.5, -1.5)

polyroot(z)

z1 <- c(1, -0.166667, -0.166667)

polyroot(z1)
