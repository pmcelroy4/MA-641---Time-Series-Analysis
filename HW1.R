#Question 3

et <- list(-1, -1, -1, -1, -1, -1, 1, 1, -1, -1)
Yt <- cumsum(et)

plot(y=Yt, x=1:10, main = "Coin Flips (H=1, T=-1) vs Time", 
     ylab = "Cumulative Sum of Flips", xlab = "Time", type='o', lwd=2, col="blue")

plot(y=Yt,x=zlag(Yt), main = "Coin Flips vs Previous Coin Flips", 
     ylab='Coin Flip Result',xlab='Previous Coin Flip Result', lwd=2, col=4)

#Question 8

w = rnorm(48,0,1)
 y = cumsum(w)
  plot.ts(y, ylim = c(-10, 10), main = "Random 'Normal' Walk")
  plot(y=y,x=zlag(y), main = "Result vs Previous Result", 
       ylab='Result',xlab='Previous Result', lwd=2, col=4)

X <- rchisq(48,2)
  plot.ts(X, ylim = c(0, 10), main = "Random 'Chi-Sq' Walk")
  plot(y=X,x=zlag(X), main = "Result vs Previous Result", 
       ylab='Result',xlab='Previous Result', lwd=2, col=4)

t <- rt(48,5)
  plot.ts(t, ylim = c(-10, 10), main = "Random 'T-Dist' Walk")
  plot(y=t,x=zlag(t), main = "Result vs Previous Result", 
       ylab='Result',xlab='Previous Result', lwd=2, col=4)


