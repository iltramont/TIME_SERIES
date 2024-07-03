###################
# SPECTRAL ANALYSIS
###################


# Create data
par(mfcol=c(1, 1))
set.seed(1)
x <- rnorm(2048)

spectrum(x, log=c('no'))
spectrum(x, span=65, log=c('no'))


# Simulate AR(1)
set.seed(1)
x <- w <- rnorm(1024)
alpha <- 0.9
for (t in 2:1024)
  x[t] <- x[t-1]*alpha + w[t]

plot(as.ts(x))
acf(x)
acf(x, lag.max = 500)
spectrum(x, span=51, log=c('no'))

# Change alpha to -0.9
set.seed(1)
x <- w <- rnorm(1024)
alpha <- -0.9
for (t in 2:1024)
  x[t] <- x[t-1]*alpha + w[t]

plot(as.ts(x))
acf(x)
acf(x, lag.max = 500)
spectrum(x, span=51, log=c('no'))


# Simulate AR(2)
set.seed(1)
x <- w <- rnorm(1024)
alpha_1 <- 1.0
alpha_2 <- -0.6
for (t in 3:1024)
  x[t] <- x[t-1]*alpha_1 + x[t-2]*alpha_2 + w[t]
plot(as.ts(x))
acf(x)
spectrum(x, span=51, log=c('no'))
         

###########
# Wave tank
###########
wavetank_data <- read.table('datasets/wave.dat', header=TRUE)
attach(wavetank_data)
plot(as.ts(waveht))
acf(waveht)
spectrum(waveht)
spectrum(waveht, log=c('no'), method=c('ar'))


#############
# Imotor data
#############
imotor_data <- read.table('datasets/imotor.dat', header=TRUE)
attach(imotor_data)
print(head(imotor_data))

xg_spec <- spectrum(good, span=9)
xb_spec <- spectrum(broken, span=9)

freqg <- 400*xg_spec$freq[4400:5600]
freqb <- 400*xb_spec$freq[4400:5600]

plot(freqg, 10*log10(xg_spec$spec[4400:5600]), type='l')
lines(freqb, 10*log10(xb_spec$spec[4400:5600]), type='l', col='red')

################
# Climate change
################
soi_dat <- read.table('datasets/soi.dat', header=TRUE)
attach(soi_dat)
head(soi_dat)
soi_ts <- ts(SOI, st=c(1866, 1), end=c(2006, 11), fr=12)
plot(soi_ts)
soi_spec <- spectrum(soi_ts)


#####
# VAR
#####
# install.packages('mvtnorm')
library(mvtnorm)
covariance_matrix <- matrix(c(1, 0.8, 0.8, 1), nr=2)
print(covariance_matrix)
w <- rmvnorm(1000, sigma=covariance_matrix)
print(head(w))
wx <- w[, 1]
wy <- w[, 2]
x <- y <- rep(0, 1000)
x[1] <- wx[1]
y[1] <- wy[1]
for (i in 2:1000){
  x[i] <- 0.4*x[i-1] + 0.3*y[i-1] + wx[i]
  y[i] <- 0.2*x[i-1] + 0.1*y[i-1] + wy[i]
}

# Estimate coefficients
xy_ar <- ar(cbind(x, y))  
xy_ar$ar[, ,]

# Exercise 2
set.seed(123)
t <- 200
k <- 2
p <- 2
A_1 <- matrix(c(-0.3, 0.6, -0.4, 0.5), k)
A_2 <- matrix(c(-0.1, 0.2, -0.1, 0.05), k)
A <- cbind(A_1, A_2)
print(A)
# Generate series
series <- matrix(0, k, t + 2*p)
for (i in (p+1):(t+2*p)){
  series[, i] <- A_1 %*% series[, i-1] + A_2 %*%series[, i-2] + rnorm(k, 0, 0.5)
}
series <- ts(t(series[, -(1:p)]))
names <- c('V1', 'V2')
plot.ts(series)

# install.packages('vars')
library(vars)
var_1 <- VAR(series, 2, type='none')
var_aic <- VAR(series, type='none', lag.max=5, ic='AIC')
print(summary(var_aic))
# True values:
print(A)
est_coef <- coef(var_aic)
est_coef <- rbind(est_coef[[1]][, 1], est_coef[[2]][, 1])
print(round(est_coef, 2))
