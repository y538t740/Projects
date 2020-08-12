data = read.table("1 Atlanta Temperature data.txt",header=T)
names(data)
temp = as.vector(t(data[,2:13]))
temp = ts(temp,start=1879,frequency=12)
ts.plot(temp, ylab="Temperature")

##Create time points for fitting trends
time.pts = c(1:length(temp))
time.pts = c(time.pts - min(time.pts))/max(time.pts)

#Kernel Regression/Moving Average
##ksmooth used for kernel regression for moving average
mav.fit = ksmooth(time.pts, temp, kernel = "box")
temp.fit.mav = ts(mav.fit$y,start=1902,frequency=12)
#Is there a trend?
ts.plot(temp,ylab="Temperature")
lines(temp.fit.mav,lwd=2,col="purple")
abline(temp.fit.mav[1],0,lwd=2,col="blue")

#Parametric Regression
x1=time.pts
x2=time.pts^2
lm.fit = lm(temp~x1+x2)
summary(lm.fit)
temp.fit.lm= ts(fitted(lm.fit),start=1879,frequency=12)
ts.plot(temp,ylab="Temperature")
lines(temp.fit.lm,lwd=2,col="green")
abline(temp.fit.lm[1],0,lwd=2,col="blue")

##Non-parametric Regression
#Local Polynomial
loc.fit = loess(temp~time.pts)
temp.fit.loc=ts(fitted(loc.fit),start=1879,frequency=12)
#Splines
library(mgcv)
gam.fit=gam(temp~s(time.pts))
temp.fit.gam=ts(fitted(gam.fit),start=1879,frequency=12)
#Is there a trend
ts.plot(temp,ylab='Temperature')
lines(temp.fit.loc,lwd=2,col="brown")
lines(temp.fit.gam,lwd=2,col="red")
abline(temp.fit.loc[1],0,lwd=2,col="blue")

#Final trend comparison
all.val=c(temp.fit.mav,temp.fit.lm,temp.fit.gam,temp.fit.loc)
ylim = c(min(all.val),max(all.val))
ts.plot(temp.fit.lm,lwd=2,col='green',ylim=ylim,ylab="Temperature")
lines(temp.fit.mav,lwd=2,col="purple")
lines(temp.fit.gam,lwd=2,col="red")
lines(temp.fit.loc,lwd=2,col="brown")
legend(x=1900,y=64,legend=c("MAV","LM","GAM","LOESS"),lty=1,col = c("purple","green","red","brown"))
