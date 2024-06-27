options(repr.plot.width=20, repr.plot.height=10)

###################
# WINE HOLT-WINTERS
###################

df <- read.csv("datasets/wine.dat", header=TRUE);
head(df)

sweetw <- df$sweetw
sweetw_ts <- ts(sweetw, start = c(1980,1), freq = 12)
plot(sweetw_ts, xlab= "Time (months)", ylab = "sales (1000 litres)", main = "Sales of Australian wine", cex.lab=5, cex.axis=5, cex.main=3)

d <- decompose(sweetw_ts, type='multiplicative')
print(d)
plot(d)

sweet_hw <- HoltWinters(sweetw_ts, seasonal='mult')
print(sweet_hw)
print(sweet_hw$coefficients)
print(sweet_hw$SSE)
plot(sweet_hw$fitted)
plot(sweet_hw)


######################
# Candies HOLT-WINTERS
######################

df <- read.csv("datasets/candy_production.csv", header=TRUE)
head(df)
# Convert the date in a proper format
df$Date <- as.Date(df$observation_date, "%Y-%m-%d")
df_ts <- ts(df$IPG3113N, frequency=12, start=c(1972,1))
print(df_ts)

df_ts_components <- decompose(df_ts)
plot(df_ts_components)

# Custom Holt-Winters
# When the seasonality parameter is omitted, we have additive HW
HW1 <- HoltWinters(df_ts,
                   alpha=0.2,
                   beta=0.1,
                   gamma=0.1)
#Visually evaluate the fits
plot(df_ts, ylab="Candy production", xlim=c(2013,2018))
lines(HW1$fitted[,1], lty=2, col="blue")
title(main = "Candy store sales (Holt-Winters) " ) 

# Prediction
HW1_pred <- predict(HW1, 24, prediction.interval = TRUE, level=0.95)

#Visually evaluate the prediction
# Plot the original data
plot(df_ts, ylab="candy production", xlim=c(2008.5,2020))

# Plot the fit
lines(HW1$fitted[,1], lty=2, col="blue")

# Plot the predicted data (24 months)
lines(HW1_pred[,1], col="red")

# Plot the confidence intervals
lines(HW1_pred[,2], lty=2, col="orange")
lines(HW1_pred[,3], lty=2, col="orange")


HW3 <- HoltWinters(df_ts, seasonal="multiplicative")
HW3_pred <- predict(HW3, 24, prediction.interval=TRUE, level=0.95)
plot(df_ts, ylab="Candy production", xlim=c(2008.5,2020))
lines(HW3$fitted[,1], lty=2, col="blue")
lines(HW3_pred[,1], col="red")
lines(HW3_pred[,2], lty=2, col="orange")
lines(HW3_pred[,3], lty=2, col="orange")

# Look at the confidence intervals... No the the right way to go!

library(forecast)


HW1_for <- forecast(HW1, h=24, level=c(80,95))


#visualize our predictions:
plot(HW1_for, xlim=c(2008.5, 2020))
lines(HW1_for$fitted, lty=2, col="purple")

acf(HW1_for$residuals, lag.max=20, na.action=na.pass)

Box.test(HW1_for$residuals, lag=20, type="Ljung-Box")
hist(HW1_for$residuals)


########################
# Holt Winters smoothing
########################

motor<-function(){
  
  Motor.dat <- read.csv("datasets/motororg.dat", header = T); attach(Motor.dat)
  
  # Convert the dataframe into a TS 
  # Note: the Complaints variable does not
  # need any qualifier because we stated the 
  # attach function earlier
  Comp.ts <- ts(complaints, start = c(1996, 1), freq = 12)
  plot(Comp.ts, xlab = "Time / months", ylab = "Complaints")
  title("Complaints time-series")
  
  
  # Invoke Holt-Winters funtion
  Comp.hw1 <- HoltWinters( complaints, beta = FALSE, gamma = FALSE) 
  
  # Note that this function returns an object
  # that can be of course plotted
  plot(Comp.hw1)
  print(Comp.hw1)
  
  # From the Holt-Winters, print the SSe variable
  print(Comp.hw1$SSE)
  
  # The estimated value of the mean number of letters of complaint per month
  # at the end of 1999 is 17.97. The value  that gives a minimum SS1PE, of
  # 2502, is 0.143. We now compare these results with those obtained if we specify
  # a value for alpha of 0.2.
  Comp.hw2 <- HoltWinters(complaints, alpha = 0.2, beta=FALSE, gamma=FALSE)
  print(Comp.hw2)
  print(Comp.hw2$SSE)
}
motor()


