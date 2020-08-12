#Get data
setwd("C:/Users/tanyi/OneDrive/Desktop/Modeling/R")
data = read.csv("MooseData.csv",header = T)
names(data) = c("year", "fairbanksPop", "mooseHarvest","totalMoose","avgSnow","wolfPop")

#Convert into TS
ts_fairbanksPop = ts(data[,"fairbanksPop"],start=1965,freq=1)
ts_mooseHarvest= ts(data[,"mooseHarvest"],start=1965,freq=1)
ts_totalMoose = ts(data[,"totalMoose"],start=1965,freq=1)
ts_avgSnow = ts(data[,"avgSnow"],start=1965,freq=1)
ts_wolfPop = ts(data[,"wolfPop"],start=1965,freq=1)

par(mfrow = c(2,2))
plot(ts_fairbanksPop)
plot(ts_wolfPop)
plot(ts_totalMoose)
plot(ts_mooseHarvest)
#The trend in totalMoose is pretty predominent, so we may use difference
par(mfrow=c(2,2))
plot(ts_totalMoose,xlab="Years",ylab="Moose Population",type="l")
plot(diff(ts_totalMoose),xlab="Years",ylab="1st Diff: Moose Pop",type="l")
acf(diff(ts_totalMoose),main="ACF Difference: Moose Population")
pacf(diff(ts_totalMoose),main="PACF Difference: Moose Population")

#Or we can use estimation by splines
library(mgcv)
totalMoose = data[,"totalMoose"]
time.pts = c(1:length(totalMoose))
time.pts = c(time.pts - min(time.pts))/max(time.pts)
gam.fit.tr = gam(totalMoose~s(time.pts))
moose.fit.gam = ts(fitted(gam.fit.tr),start=1965,freq=1)
resid.process = totalMoose-moose.fit.gam
resid.process = ts(resid.process,start=1965,freq=1)
#Plot TS
plot(ts_totalMoose,xlab="Years",ylab="Moose Population",type="l")
lines(moose.fit.gam,lwd=2,col="blue")
plot(resid.process,xlab="Years",ylab="De-Trend TS",type="l")
acf(resid.process,main="ACF")
pacf(resid.process,main="pacf")

## Order selection -- AIC 
n = length(resid.process)
norder = 6
p = c(1:norder)-1; q = c(1:norder)-1
aic = matrix(0,norder,norder)
for(i in 1:norder){
  for(j in 1:norder){
    modij = arima(resid.process,order = c(p[i],0,q[j]), method='ML')
    aic[i,j] = modij$aic-2*(p[i]+q[j]+1)+2*(p[i]+q[j]+1)*n/(n-p[i]-q[j]-2)
  }  
}
aicv = as.vector(aic)  
plot(aicv,ylab="AIC values")
indexp = rep(c(1:norder),norder)
indexq = rep(c(1:norder),each=norder)
indexaic = which(aicv == min(aicv))
porder = indexp[indexaic]-1    #2
qorder = indexq[indexaic]-1    #3

#ARMA Modeling
library(forecast)
final_model<-auto.arima(resid.process,max.p=10,max.q=10,max.d=5,ic="aic")
final_model2 = arima(resid.process, order=c(2,0,3),method="ML")
summary(final_model2)
acf(resid(final_model2),main="ACF: Residuals")
Box.test(final_model2$resid, lag = 6, type = "Box-Pierce",fitdf=5)
Box.test(final_model2$resid, lag = 6, type = "Ljung-Box",fitdf=5)

#Forecasting 4 years ahead
nfit = n-4
##Predict trend
train.totalMoose = totalMoose[1:nfit]
x=time.pts[1:nfit]
gam.fit.tr.4 = gam(train.totalMoose~s(x))
newdata = data.frame(x=time.pts[(nfit+1):n])
gam.pred=predict(gam.fit.tr.4,newdata=newdata,interval=c("prediction"))
resid.process.4 = totalMoose[1:nfit]-fitted(gam.fit.tr.4)
resid.process = ts(resid.process.4,start=1965,freq=1)
##Predict ARMA
outresid = arima(resid.process.4, order=c(2,0,3),method="ML")
outpredresid = predict(outresid,n.ahead=4)$pred
final.pred.1 = outpredresid+gam.pred



##----------------ARIMA Modeling-----------------
## Order selection -- AIC 
norder = 6
p = c(1:norder)-1; q = c(1:norder)-1
aic = matrix(0,norder,norder)
for(i in 1:norder){
  for(j in 1:norder){
    modij = arima(ts_totalMoose,order = c(p[i],1,q[j]), method='ML')
    aic[i,j] = modij$aic-2*(p[i]+q[j]+1)+2*(p[i]+q[j]+1)*n/(n-p[i]-q[j]-2)
  }  
}

aicv = as.vector(aic)  
plot(aicv,ylab="AIC values")
indexp = rep(c(1:norder),norder)
indexq = rep(c(1:norder),each=norder)
indexaic = which(aicv == min(aicv))
porder = indexp[indexaic]-1
qorder = indexq[indexaic]-1

final_model.2 = arima(ts_totalMoose, order = c(porder,1,qorder), method = "ML")
resids.2 = resid(final_model.2)
## Residual Analysis
par (mfrow=c(2,2))
plot(resids.2, ylab='Standardized Residuals')
abline(h=0)
acf(resids.2,main= 'ACF of the Model Residuals')
pacf(resids.2,main='PACF of the Model Residuals')
qqnorm(resids.2)
qqline(resids.2)

## Test for Independence for final model
Box.test(resids.2, lag = (porder+qorder+1), type = "Box-Pierce", fitdf = (porder+qorder))
Box.test(resids.2, lag = (porder+qorder+1), type = "Ljung-Box", fitdf = (porder+qorder))

## Forecasting with ARIMA 
## 4 Years Ahead: 
years=data[,"year"] 
outtotal = arima(ts_totalMoose[1:nfit], order = c(porder,1,qorder),method = "ML")
final.pred.2 = predict(outtotal,n.ahead=4)$pred
par(mfrow=c(1,1))
ymin = min(c(ts_totalMoose[(n-20):n],final.pred.1,final.pred.2))
ymax = max(c(ts_totalMoose[(n-20):n],final.pred.1,final.pred.2))
plot(years[(n-20):n], ts_totalMoose[(n-20):n],type="l", ylim=c(ymin,ymax), xlab="Time", ylab="Moose Population")
lines(years[(nfit+1):n],final.pred.1,col="red",lwd=2)
lines(years[(nfit+1):n],final.pred.2,col="blue",lwd=2)
legend(1998,16000,legend=c("Trend+ARMA","ARIMA"),col=c("red","blue"),lty=1)
#It proves the trend estimation may be better than simply removal by differencing.
