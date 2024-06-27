#Loading the Data Set
data("AirPassengers")

#This tells you that the data series is in a time series format
is.ts(AirPassengers)



print(AirPassengers)

#This will plot the time series
ts.plot(AirPassengers, xlab="Year", ylab="Number of Passengers", main="Monthly totals of international airline passengers, 1949-1960")

# This will fit in a line
abline(reg=lm(AirPassengers~time(AirPassengers)))


acf(AirPassengers)

#Fitting the AR Model to the time series
AR <- arima(AirPassengers, order = c(1,0,0))
print(AR)


ts.plot(AirPassengers)
AR_fit <- AirPassengers - residuals(AR)
points(AR_fit, type = "l", col = 2, lty = 2)

#Using predict() to make a 1-step forecast
predict_AR <- predict(AR)

#Obtaining the 1-step forecast using $pred[1]
predict_AR$pred[1]

predict(AR, n.ahead = 10)

ts.plot(AirPassengers, xlim = c(1949, 1961))
AR_forecast <- predict(AR, n.ahead = 10)$pred
AR_forecast_se <- predict(AR, n.ahead = 10)$se
points(AR_forecast, type = "l", col = 2)
points(AR_forecast - 2*AR_forecast_se, type = "l", col = 2, lty = 2)
points(AR_forecast + 2*AR_forecast_se, type = "l", col = 2, lty = 2)


MA <- arima(AirPassengers, order = c(0,0,1))
print(MA)

ts.plot(AirPassengers)
MA_fit <- AirPassengers - resid(MA)
points(MA_fit, type = "l", col = 2, lty = 2)

predict_MA <- predict(MA)

predict_MA$pred[1]

predict(MA,n.ahead=10)

ts.plot(AirPassengers, xlim = c(1949, 1961))
MA_forecasts <- predict(MA, n.ahead = 10)$pred
MA_forecast_se <- predict(MA, n.ahead = 10)$se
points(MA_forecasts, type = "l", col = 2)
points(MA_forecasts - 2*MA_forecast_se, type = "l", col = 2, lty = 2)
points(MA_forecasts + 2*MA_forecast_se, type = "l", col = 2, lty = 2)

# Find correlation between AR_fit and MA_fit

cor(AR_fit, MA_fit)

AIC(AR)

AIC(MA)

BIC(AR)

BIC(MA)


