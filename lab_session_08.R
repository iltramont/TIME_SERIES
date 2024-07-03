library(dplyr)
library(tidyverse)
#install.packages("fGarch")
#install.packages("PerformanceAnalytics")
#install.packages("rugarch")
#install.packages("tseries")
#install.packages("xts")
#install.packages("FinTS")
#install.packages("urca")
library(fGarch)
library(PerformanceAnalytics)
library(rugarch)
library(tseries)
library(xts)
library(FinTS)
library(urca)



BMW <- read.csv("datasets/BMW.csv", sep=";", header=TRUE)
print(summary(BMW))
BMW$Years <- as.Date(BMW$Years, "%d/%m/%Y")
print(head(BMW))
print(class(BMW$Years))
print(BMW$Prices)

# Index TS
BMW_z <- zoo(x=BMW$Prices, order.by=BMW$Years)
print(BMW_z)
print(class(BMW_z))

# Compute returns
BMW_returns <- Return.calculate(BMW_z, method='log')[-1]

# Check for stationarity
ADF_returns <- ur.df(BMW_returns, type='drift', selectlags='AIC')
summary(ADF_returns)
# We can reject the null hypothesis: TS is stationary


# Check for persistence of volatility
dataToPlot = cbind(BMW_returns, BMW_returns^2)
colnames(dataToPlot) <- c("Returns", "Returns^2")
plot.zoo(dataToPlot)


# Check normality
options(repr.plot.width=NULL, repr.plot.height=NULL)

# Histogram
hist(BMW_returns, prob=TRUE, breaks=50)
# Add gaussian line to plot
mu <- mean(BMW_returns)
sigma <- sd(BMW_returns)
x <- seq(min(BMW_returns), max(BMW_returns), length=80)
y <- dnorm(x, mu, sigma)
# Add line
lines(x, y, col='red')

# QQ-plot
qqnorm(BMW_returns)
qqline(BMW_returns)

# Stats
table.Stats(cbind(BMW_returns))

# Jarque-Bera test. H0: data are normally distributed
jarque.bera.test(BMW_returns)
# I can reject the null: data are not normally distributed


# Check for ARCH effect
BMW_returns <- as.xts(BMW_returns)  # Extensible TS

par(mfrow=c(1, 2))
acf(BMW_returns)
acf(BMW_returns^2)
par(mfrow=c(1, 1))


# Ljung-Box test
Box.test(coredata(BMW_returns^2), type='Ljung-Box', lag=12)
# We can reject the null hypothesis: data are not dependent


# ARCH test
ArchTest(BMW_returns)
# Reject the null: there is arch effect


#####################################
# ESTIMATION OF ARCH and GARCH models
#####################################








