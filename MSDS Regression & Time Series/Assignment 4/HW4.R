#Problem 2
#b)
wNoise = rnorm(500,0,1)                  # 500 N(0,1) variates
x <- array(dim=500) 
x[1] <- wNoise[1]
for(i in 2:500)
  x[i] = wNoise[i]*wNoise[i-1]

plot.ts(x, main="Series xt")
acf(x)

#If we look at the times series, it seems to seasonal variation in number per time unit, there are peaks from time to showing this.

#From the ACF test, we can see that the autocorrelation at lag 1 is just touching the significance bounds

#c)
Box.test(x,lag=1,type='Ljung')
Box.test(x,lag=2,type='Ljung')
Box.test(x,lag=3,type='Ljung')
Box.test(x,lag=4,type='Ljung')
Box.test(x,lag=5,type='Ljung')
Box.test(x,lag=6,type='Ljung')


#Problem 3

hawaii <- read.table('hawaii-new.dat', col.names = c('year_month', 'total', 'west', 'east'))
hawaii[1,]
tHawaii=hawaii[,2] ## total
wHawaii=hawaii[,3] ## west
eHawaii=hawaii[,4] ## east

tHawaii.ts=ts(tHawaii,frequency=12,start=c(70,1))
wHawaii.ts=ts(wHawaii,frequency=12,start=c(70,1))
eHawaii.ts=ts(eHawaii,frequency=12,start=c(70,1))

#a)
plot.ts(tHawaii.ts, ylim=c(20000,710000), main=" Monthly tourists in Hawaii 1970/01 to 1995/12")
lines(wHawaii.ts, col = 'red')
lines(eHawaii.ts, col = 'blue')
legend("topleft" , legend=c("Total", "West", "East"), col=c("black", "red", "blue"), lty=1:2, cex=0.8)

# The Total and East the plots seems to follow an upward linear trend until the year 92 aproximately, 
# where a shift down happens, a more a drastical one for the East plot. Then the Total returns to the upward trend,
# but the East remains in a horizontal trend. In the West plot, we can see that it keeps an upward trend the
# whole time, what seems to be a quadratic trend.
# Also the three present a seasonal pattern, higher on the summer months and lower on the winter ones.
# As supposed, the total plot is higher than both West and East, but as times goes, especially since year
# 88 aproximately, the West plot begins to grow more while the East begans to go down.

#b)
log.tHawaii=log(tHawaii.ts) ## log transform
plot.ts(log.tHawaii,ylab="Log total",main="Hawaii Total - log transform")
plot.ts(tHawaii.ts, main=" Monthly tourists in Hawaii 1970/01 to 1995/12")

#The trend from the log transformation is practically the same as the original time series, so nothing really
# changes when performing the log transformation.

#c)
# tot <- c(1:312)
# tHawaii.lm1=lm(log.tHawaii~tot)
# tHawaii.lm2=lm(log.tHawaii~tot+I(tot^2))
# tHawaii.lm3=lm(log.tHawaii~tot+I(tot^2)+I(tot^3))
# tHawaii.lm4=lm(log.tHawaii~tot+I(tot^2)+I(tot^3)+I(tot^4))
# tHawaii.lm5=lm(log.tHawaii~tot+I(tot^2)+I(tot^3)+I(tot^4)+I(tot^5))
# 
# summary(tHawaii.lm1)
# summary(tHawaii.lm2)
# summary(tHawaii.lm3)
# summary(tHawaii.lm4)
# summary(tHawaii.lm5)



#Problem 4
yt <- scan("lt.txt")
#b
#initialize variables from problem description
sigma <- array(500)
sigma[1] <-  2.26
st <- array(500)
st[1] <- 0.2
kt <- array(500)
vt <- array(500)
Vt <- array(500)

for(i in 1:500){
  vt[i]=yt[i]-st[i]
  Vt[i]=sigma[i]+0.25
  kt[i]=sigma[i]/Vt[i]
  st[i+1]=st[i]+kt[i]*vt[i]
  sigma[i+1]=(1-kt[i])*sigma[i]+0.01
}

upper <- array(500)
lower <- array(500)
for(i in 1:500){
  upper[i]=st[i]+2*sqrt(sigma[i])
  lower[i]=st[i]-2*sqrt(sigma[i])
}

plot.ts(st,ylim=c(-3.2,1.0),main="predicted state variables")
lines(upper,col='blue')
lines(lower,col='red')
#c
flt_sigma <- array(500)
flt_sigma[1] <- 2.26

for(i in 1:500){
  flt_sigma[i+1]=(1-kt[i])*sigma[i]
}

flt_upper <- array(500)
flt_lower <- array(500)
for(i in 1:500){
  flt_upper[i]=st[i]+2*sqrt(flt_sigma[i])
  flt_lower[i]=st[i]-2*sqrt(flt_sigma[i])
}
plot.ts(st,ylim=c(-3.2,1.0),main="filtered state variables")
lines(flt_upper,type='l',col='blue')
lines(flt_lower,type='l',col='red')

#d
smo_lt <- array(500)
smo_qt <- array(501)
smo_mt <- array(501)
smo_qt[501] <- 0
smo_mt[501] <- 0
smo_st <- array(500)
smo_sigma <- array(500)
for(i in 1:500){
  smo_lt[i]=1-kt[i]
}

for(i in 500:1){
  smo_qt[i]=vt[i]/Vt[i]+smo_lt[i]*smo_qt[i+1]
  smo_mt[i]=1/Vt[i]+smo_lt[i]*smo_lt[i]*smo_mt[i+1]
  smo_st[i]=st[i]+sigma[i]*smo_qt[i]
  smo_sigma[i]=sigma[i]-sigma[i]*sigma[i]*smo_mt[i]
}

smo_upper <- array(500)
smo_lower <- array(500)

for(i in 1:500){
  smo_upper[i]=smo_st[i]+2*sqrt(smo_sigma[i])
  smo_lower[i]=smo_st[i]-2*sqrt(smo_sigma[i])
}

plot.ts(smo_st,ylim=c(-2.7,0.5),main="smoothed state variables")
lines(smo_upper,type='l',col='blue')
lines(smo_lower,type='l',col='red')

