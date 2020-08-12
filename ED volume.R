#Read data in R
edvoldata = read.csv("EGDailyvolume.csv",header=T)
#Process Dates
year = edvoldata$Year
month = edvoldata$Month
day = edvoldata$Day
Volume = edvoldata$Volume
datemat = cbind(as.character(day),as.character(month),as.character(year))

#Date formatting
paste.dates = function(date){
  day = date[1]; month=date[2]; year=date[3]
  return(paste(day,month,year,sep="/"))
}
dates = apply(datemat,1,paste.dates)
dates = as.Date(dates, format="%d/%m/%Y")

#Plot in TS
library(ggplot2)
ggplot(edvoldata, aes(dates, Volume))+geom_line()+xlab('Time')+ylab("Daily ED Volume")
##Classic transformation of non-constant variance
Volume.tr = sqrt(Volume+3/8)
##Compare Distribution
hist(Volume, nclass=20, xlab="ED Volume",main="",col="brown")
hist(Volume.tr, nclass=20, xlab="Transformed ED Volume", main="", col="blue")

#Equally spaced time points
time.pts = c(1:length(Volume))
time.pts = c(time.pts - min(time.pts))/max(time.pts)
#Local Polynomial Trend Estimation
loc.fit = loess(Volume.tr~time.pts)
vol.fit.loc = fitted(loc.fit)
summary(loc.fit)
#Splines Trend Estimation
library(mgcv)
gam.fit = gam(Volume.tr~s(time.pts))
vol.fit.gam = fitted(gam.fit)
summary(gam.fit)
#Non-constant trend exists and the splines explain 29.6% of it
#Is there a trend:
plot(dates, Volume.tr,ylab="ED Volume")
lines(dates,vol.fit.loc,lwd=2,col="brown")
lines(dates,vol.fit.gam,lwd=2,col="red")

#Add monthly seasonality mean as seasonalirity factor
month2 = as.factor(month.abb[month])
gam.fit.seastr.1 = gam(Volume.tr~s(time.pts)+month2)
summary(gam.fit.seastr.1)
vol.fit.gam.seastr.1 = fitted(gam.fit.seastr.1)
plot(dates, Volume.tr,ylab="ED Volume")
lines(dates, vol.fit.gam.seastr.1,lwd=2,col="blue")
lines(dates, vol.fit.gam.seastr.2,lwd=2,col="red")

#Monthly seasonally did not cover enough variance, add weekly seasonality
week = as.factor(weekdays(dates))
gam.fit.seastr.2 = gam(Volume.tr~s(time.pts)+month2+week)
summary(gam.fit.seastr.2)
vol.fit.gam.seastr.2 = fitted(gam.fit.seastr.2)
##Compared the two fits
plot(dates, Volume.tr,ylab="ED Volume")
lines(dates, vol.fit.gam.seastr.1,lwd=2,col="blue")
lines(dates, vol.fit.gam.seastr.2,lwd=2,col="red")
#Does it improve the model? Use anova
lm.fit.seastr.1=lm(Volume.tr~month2)
lm.fit.seastr.2 = lm(Volume.tr~month2+week)
anova(lm.fit.seastr.1,lm.fit.seastr.2)
#Yes it does!

#The TS after removing trend and seasonality
lm.fit.seastr.2 = lm(Volume.tr~month2+week)
summary(lm.fit.seastr.2)
vol.fit.lm.seastr.2 = fitted(lm.fit.seastr.2)
#Trend Removal
resid.1 = Volume.tr-vol.fit.gam
#Seasonality Removal
resid.2 = Volume.tr-vol.fit.lm.seastr.2
#Trend and Stationary Removal
resid.3 = Volume.tr-vol.fit.gam.seastr.2
#Compare auto-correlation plots
acf(resid.1,lag.max=12*4,main="")
acf(resid.2,lag.max=12*4,main="",col="blue")
acf(resid.3,lag.max=12*4,main="",col="brown")
