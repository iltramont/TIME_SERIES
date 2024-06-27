path<-"c:/users/Enric/Downloads/"
name <- 'candy_production.csv'
filename<-paste(path, name, sep="")

df <- read.csv(filename, header=TRUE)
head(df)

# Convert the date in a proper format
df$Date <- as.Date(df$observation_date, "%Y-%m-%d")

dfts <- ts(df$IPG3113N, frequency=12, start=c(1972,1))
dfts

 # trend: the long-term trends in the data
  # seasonal: the repeated seasonal signal adder
  # random: the “left-over” components that aren’t expected from 
  # the seasonality or trend components.
  components_dfts <- decompose(dfts)
  plot(components_dfts)

# Custom HoltWinters fitting
HW1 <- HoltWinters(dfts, alpha=0.2, beta=0.1, gamma=0.1) # When the seasonality patameter is omitted, we have additive HW

#Visually evaluate the fits
plot(dfts, ylab="candy production", xlim=c(2013,2018))
lines(HW1$fitted[,1], lty=2, col="blue")
title(main = "Candy store sales (Holt-Winters) " ) 

  # Prediction
  HW1.pred <- predict(HW1, 24, prediction.interval = TRUE, level=0.95)

 #Visually evaluate the prediction
  # Plot the original data
  plot(dfts, ylab="candy production", xlim=c(2008.5,2020))

  # Plot the fit
  lines(HW1$fitted[,1], lty=2, col="blue")

  # Plot the predicted data (24 months)
  lines(HW1.pred[,1], col="red")

  # Plot the confidence intervals
  lines(HW1.pred[,2], lty=2, col="orange")
  lines(HW1.pred[,3], lty=2, col="orange")
  

HW3 <- HoltWinters(dfts, seasonal = "multiplicative")
HW3.pred <- predict(HW3, 24, prediction.interval = TRUE, level=0.95)
plot(dfts, ylab="candy production", xlim=c(2008.5,2020))
lines(HW3$fitted[,1], lty=2, col="blue")
lines(HW3.pred[,1], col="red")
lines(HW3.pred[,2], lty=2, col="orange")
lines(HW3.pred[,3], lty=2, col="orange")

# Look at the confidence intervals... No the the right way to go!

install.packages("forecast", repos='http://cran.us.r-project.org')

library(forecast)


HW1_for <- forecast(HW1, h=24, level=c(80,95))


#visualize our predictions:
plot(HW1_for, xlim=c(2008.5, 2020))
lines(HW1_for$fitted, lty=2, col="purple")

acf(HW1_for$residuals, lag.max=20, na.action=na.pass)

Box.test(HW1_for$residuals, lag=20, type="Ljung-Box")
hist(HW1_for$residuals)
