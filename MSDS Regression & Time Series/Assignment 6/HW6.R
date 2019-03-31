#Problem 2
#rt = 0.3 + 0.8rt−1 − .5rt−2 − .2rt−3 + at + 0.5at−1 + 0.3at−2.

phi1=0.8
phi2=-0.5
phi3=-0.2
theta1=0.5
theta2=0.3

#part a
source('memory.R')
memory = memory(ar = c(phi1, phi2, phi3), ma = c(theta1,theta2), lag = 10)
tt=0:10
plot(tt,memory,type='h',xlab="Lag",ylab="psi")
lines(tt,tt*0)
title("Memory function: ARMA(3,2)")

#part b
source('auto.cov.R')
auto_cov = auto.cov(ar = c(phi1, phi2, phi3), ma = c(theta1,theta2), sigma2 = 1, lag = 10)
tt=0:10
plot(tt,auto_cov,type='h',xlab="Lag",ylab="psi")
lines(tt,tt*0)
title("Auto Cov: ARMA(3,2)")

#part c
x=arima.sim(model=list(ar= c(phi1, phi2, phi3), ma = c(theta1,theta2)),sd=1,n=600)
summary(x)
plot.ts(x, main="Time series n=600")
