##Fit a parametric model for both trend and seasonality
x1 = time.pts
x2 = time.pts^2
har2 = harmonic(temp,2)
lm.fit=lm(temp~x1+x2+har2)
summary(lm.fit)
dif.fit.lm = ts((temp-fitted(lm.fit)),start=1879,frequency=12)
ts.plot(dif.fit.lm,ylab="Residual Process")

#Fit a non-parametric model for trend and linearity for seasonality
library(mgcv)
gam.fit=gam(temp~s(time.pts)+har2)
dif.fit.gam = ts((temp-fitted(gam.fit)),start=1879,frequency=12)
#Compare approaches
ts.plot(dif.fit.lm,ylab="Residual Process",col="brown")
lines(dif.fit.gam,col="blue")

#Check TS for stationary
acf(temp,lag.max=12*4,main="")
#Check residual for stationary
acf(dif.fit.lm,lag.max=12*4,main="")
acf(dif.fit.gam,lag.max=12*4,main="")
