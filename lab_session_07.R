#########################
# Generate synthetic data
#########################

t <- seq(0, 4*pi, length.out=100)
print(t)

a <- 3  # Amplitude
b <- 2  # Width of each wave
plot(a*sin(b*t), type='l')

# Add uniform noise
n <- 100
set.seed(42)
c_unif <- runif(n) - 0.5
amp <- 2  # Noise amplitude
plot(a*sin(b*t) + amp*c_unif, type='l')
lines(a*sin(b*t), col='blue')

# Add Gaussian noise
c_norm <- rnorm(n)
plot(a*sin(b*t) + amp*c_norm, type='l')
lines(a*sin(b*t), col='blue')

# Noise with jitter
jittery <- jitter(a*sin(b*t), factor=1000)
plot(jittery, type='l')
lines(a*sin(b*t), col='blue')

# Use a second sine wave
Sine1 <- jitter(30*sin(2*t), factor=200)
plot(t, Sine1, type='l')

Sine2 <- jitter(20*sin(5*t + 2), factor=200)
plot(t, Sine2, type='l')

TwoSines <- Sine1 + Sine2
plot(t, TwoSines, type='l')

# Add a trend
LineGoingUp <- seq(0.5, 50, 0.5)
ExponentialLineGoingUp <- (3/5000000)*LineGoingUp^5
plot(t, ExponentialLineGoingUp, type='l')

TwoSinesGoingUpExponentially <- TwoSines + ExponentialLineGoingUp
plot(t, TwoSinesGoingUpExponentially, type='l')


########################
# Determing stationarity
########################

# install.packages('tseries')
# Dickey-Fuller test
# Null hypothesis: series is not stationary
library(tseries)

adf.test(TwoSinesGoingUpExponentially, alternative='stationary')
# p-value is too high: i cannot reject the null. TS is not stationary

# find d-value: number of differencing operations
Diff1TwoSinesGoingUpExponentially <- diff(TwoSinesGoingUpExponentially,
                                          differences=1)
plot(Diff1TwoSinesGoingUpExponentially, type='l')

adf.test(Diff1TwoSinesGoingUpExponentially, alternative='stationary')
# Again, p-value is too high: i cannot reject the null.
# TS is not stationary

Diff2TwoSinesGoingUpExponentially <- diff(TwoSinesGoingUpExponentially,
                                          differences=2)
plot(Diff2TwoSinesGoingUpExponentially, type='l')

adf.test(Diff2TwoSinesGoingUpExponentially, alternative='stationary')
# p-value is less than 5%: i can reject the null. TS may be stationary.

# The d value for arima is 2


# Examin the correlogram
acf(Diff2TwoSinesGoingUpExponentially, lag.max=30)

# After looking at the acf, I want to remove seasonality
# install.packages("TTR")
library(TTR)
SmoothedSines <- SMA(Diff2TwoSinesGoingUpExponentially, n=5)
plot(SmoothedSines, type='l')
# No improvement

# Decompose TS
# install.packages("TSA")
library(TSA)
PGram <- periodogram(Diff2TwoSinesGoingUpExponentially)
# I see how there is notunique frequency
PGramAsDataFrame <- data.frame(freq=PGram$freq, spec=PGram$spec)
order <- PGramAsDataFrame[order(-PGramAsDataFrame$spec),]
print(order)
top2 <- head(order, 2)
print(top2)

TimePeriod <- 1/top2[2, 1]
TimePeriod2 <- 1/top2[1, 1]
print(TimePeriod)
print(TimePeriod2)


TwoSinesGoingUpExponentiallyFreq10 <-
  ts(Diff2TwoSinesGoingUpExponentially, frequency=10)

RDecompose <- decompose(TwoSinesGoingUpExponentiallyFreq10)
plot(RDecompose)


# Perform seasonal adjustment
SineWaveDecomposed <- stl(TwoSinesGoingUpExponentiallyFreq10,
                         s.window='periodic')
plot(SineWaveDecomposed)

# install.packages("forecast")
library(forecast)
SineWaveDecomposeSeasonalAdjusted <- seasadj(SineWaveDecomposed)
plot(SineWaveDecomposeSeasonalAdjusted)

# Detract the second component

TwoSinesGoingUpExponentiallyFreq25 <-
  ts(SineWaveDecomposeSeasonalAdjusted, frequency=25)

SineWaveDecomposed2 <- stl(TwoSinesGoingUpExponentiallyFreq25,
                           s.window='periodic')
plot(SineWaveDecomposed2)

SineWaveDecomposeSeasonalAdjusted2 <- seasadj(SineWaveDecomposed2)
plot(SineWaveDecomposeSeasonalAdjusted2)

# The only thing remaining is the noise
acf(as.numeric(SineWaveDecomposeSeasonalAdjusted2), lag.max=30)
# Set parameters pa and q looking at how many lags
# exceeds the confidentiality bound


#####################
# Auto arima function
#####################
auto.arima(TwoSinesGoingUpExponentially)

# Forecasting with arima
TwoSinesAsArima <- stats::arima(SineWaveDecomposeSeasonalAdjusted2,
                                order=c(2, 1, 5))
SineWaveForecast <- forecast(TwoSinesAsArima, h=3)
print(SineWaveForecast)
SineWaveForecast <- forecast(TwoSinesAsArima, h=5, level=c(99.5))
plot(SineWaveForecast)

