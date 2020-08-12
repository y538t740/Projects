library(TSA)
#Seasonal means model
month = season(temp)
#With intercept
model1 = lm(temp~month)
summary(model1)
#Without intercept
model2 = lm(temp~month-1)
summary(model2)

##Cos-sin model
har1 = harmonic(temp,1)
model3 = lm(temp~har1)
summary(model3)
har2 = harmonic(temp,2)
model4 = lm(temp~har2)
summary(model4)

#Compare models
st1 = coef(model2)
st2 = fitted(model4)[1:12]
plot(1:12, st1,lwd=2,type="l",xlab="Month",ylab="Seasonality")
lines(1:12,st2,lwd=2,col="brown")

