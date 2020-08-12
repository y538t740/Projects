library(quantmod)
getSymbols("IBM", from="1962-01-01",to="2017-04-19", src="yahoo")
chartSeries(IBM)
#Variance is obviously not constant
#we logmalize the trend
dat <- IBM$IBM.Adjusted
dat$logadj <- log(dat$IBM.Adjusted)
par(mfrow=c(2,1))
plot(dat$IBM.Adjusted)
plot(dat$logadj)

#Differencing to remove trend
diff.ts.price = diff(dat$logadj)
index(diff.ts.price) = index(dat$logadj)
ts.price = ts(dat$logadj,start=c(1962,1,2),frequency=365.25)
diff.ts.price = diff(ts.price)
par(mfrow=c(2,1))
acf(diff.ts.price,main="ACF: One-Lag Diff")
pacf(diff.ts.price, main="PACF: One-Lag Diff")

#Auto arima
library(forecast)
final_model<-auto.arima(diff.ts.price,max.p=10,max.q=10,max.d=5,ic="aic")
summary(final_model)

par(mfrow=c(2,2))
plot(resid(final_model), ylab='Residuals',type='o',main="Residual Plot")
abline(h=0)
acf(resid(final_model),main="ACF: Residuals")
hist(resid(final_model),xlab='Residuals',main='Histogram: Residuals')
qqnorm(resid(final_model),ylab="Sample Q",xlab="Theoretical Q")
qqline(resid(final_model))

Box.test(final_model$resid, lag = 5, type = "Box-Pierce")
Box.test(final_model$resid, lag = 5, type = "Ljung-Box")

#Forcasting with Arima
n = length(ts.price)
nfit = n-10
outprice = arima(ts.price[1:nfit], order=c(2,1,0),method="ML")
outpred = predict(outprice,n.ahead=10)
ubound = outpred$pred + 1.96*outpred$se
lbound = outpred$pred-1.96*outpred$se
ymin = min(exp(lbound))
ymax = max(exp(ubound))
index(dat)[1:2]
plot(index(dat)[(n-50):n],exp(ts.price[(n-50):n]),type="l", ylim=c(ymin,ymax), xlab="Time", ylab="Stock Price")
points(index(dat)[(nfit+1):n],exp(outpred$pred),col="red")
lines(index(dat)[(nfit+1):n],exp(ubound),lty=3,lwd= 2, col="blue")
lines(index(dat)[(nfit+1):n],exp(lbound),lty=3,lwd= 2, col="blue")

#How good is the prediction
obsprice = exp(ts.price[(nfit+1):n])
predprice = exp(outpred$pred)
#Mean squared prediction error MSPE
mean((predprice-obsprice)^2)
#Mean absolute prediction error MAE
mean(abs(predprice-obsprice))
#Mean Absolute Percentage Error MAPE
mean(abs(predprice-obsprice)/obsprice)

#Daily Prediction over a period of 10 days
outpred.10 = NULL
ubound.10 = NULL
lbound.10 = NULL
n = length(ts.price)
for (i in 1:10){
  nfit = n-(10-i-1)
  outprice = arima(ts.price[1:nfit],order=c(2,1,0),method="ML")
  pred.1 = predict(outprice,n.ahead=1)
  outpred.10 = c(outpred.10, pred.1$pred)
  ubound.10 = c(ubound.10, pred.1$pred+1.96*pred.1$se)
  lbound.10 = c(lbound.10, pred.1$pred+1.96*pred.1$se)
}
## Compute Accuracy Measures 
predprice.10 = exp(outpred.10)
### Mean Squared Prediction Error (MSPE)
mean((predprice.10-obsprice)^2)
### Mean Absolute Prediction Error (MAE)
mean(abs(predprice.10-obsprice))
### Mean Absolute Percentage Error (MAPE)
mean(abs(predprice.10-obsprice)/obsprice)
### Precision Measure (PM)
sum((predprice.10-obsprice)^2)/sum((obsprice-mean(obsprice))^2)

### Does the observed data fall outside the prediction intervals?
sum(obsprice<exp(lbound.10))+sum(obsprice>exp(ubound.10))
nfit = n-10
ymin = min(exp(c(lbound,lbound.10)))
ymax = max(exp(c(ubound,ubound.10)))
par(mfrow=c(1,1))
plot(index(dat)[(n-30):n],exp(ts.price[(n-30):n]),type="l", ylim=c(ymin,ymax), xlab="Time", ylab="Stock Price")
points(index(dat)[(nfit+1):n],exp(outpred$pred),col="red")
lines(index(dat)[(nfit+1):n],exp(ubound),lty=3,lwd= 2, col="blue")
lines(index(dat)[(nfit+1):n],exp(lbound),lty=3,lwd= 2, col="blue")
points(index(dat)[(nfit+1):n],exp(outpred.10),col="green")
lines(index(dat)[(nfit+1):n],exp(ubound.10),lty=3,lwd= 2, col="purple")
lines(index(dat)[(nfit+1):n],exp(lbound.10),lty=3,lwd= 2, col="purple")
legend(index(dat)[n-25],170,legend=c("1 Lag Ahead Prediction", "1 Lag Ahead Prediction Interval",
                                        "10 Lags Ahead Prediction","10 Lags Ahead Prediction Interval" ),col=c("green","purple","red","blue"),pch=1,lty = c(0,1,0,1))

