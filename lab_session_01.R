library(ggplot2)

# Time series object

prelim <- function(){
  # Generates a sequence of random numbers distributed
  # according to a normal distribution
  set.seed(123)
  t <- seq(from=1, to=100, by=1) + 10 + rnorm(100, sd=7)
  t_series <- ts(t, start=c(2000, 1), frequency=4)
  print(t_series)
  plot(t_series)
}
prelim()

prelim_2 <- function(){
  set.seed(123)
  t <- seq(from=1, to=100, by=1) + 10
  ts1 <- t + rnorm(100, sd=5)
  ts2 <- 2*t + rnorm(100, sd=5)
  ts3 <- t^2 + rnorm(100, sd=300)
  
  tsm <- cbind(ts1, ts2, ts3)
  
  tsm <- ts(tsm, start=c(2000, 1), frequency=4)
  
  plot(tsm)
}
prelim_2()

ap <- function(){
  data(AirPassengers)
  AP <- AirPassengers
  print(AP)
  print(class(AP))
  
  print(start(AP))
  print(end(AP))
  print(frequency(AP))
  
  plot(AP, ylab="Passengers (1000's)")
  
  layout(1:2)
  plot(aggregate(AP))
  boxplot(AP ~ cycle(AP))
}
ap()


CBE <- read.csv("datasets/cbe.dat", header=TRUE)
print(paste("CBE is a ", class(CBE)))
head(CBE)
ELEC <- ts(CBE[, 3], start=1958, freq=12)
BEER <- ts(CBE[, 2], start=1958, freq=12)
CHOC <- ts(CBE[, 1], start=1958, freq=12)
plot(cbind(ELEC, BEER, CHOC))
layout(1:1)

# Time series decomposition

ELEC_decom <- decompose(ELEC, type="mult")
plot(ELEC_decom)

ELEC_trend <- ELEC_decom$trend
ELEC_seasonal <- ELEC_decom$seasonal

plot(ELEC_trend)
lines(ELEC_trend * ELEC_seasonal)


# ACF

viscosity <- read.csv("datasets/viscosity.csv", header=FALSE)[, 2]
print(viscosity)
plot(viscosity)
acf(viscosity)


# Variogram
myvariogram <- function(x, lag){
  Lag <- NULL
  vark <- NULL
  vario <- NULL
  for (k in 1: lag){
    Lag[k] <- k
    vark[k] <- sd(diff(x, k))^2
    vario[k] <- vark[k] / vark[1]
  }
  return (as.data.frame(cbind(Lag, vario)))
}
variogram_prodution <- myvariogram(viscosity, length(viscosity)/4)
print(variogram_prodution)
# Plot
ggplot(variogram_prodution, aes(x=Lag, y=vario))+
  geom_col()+
  labs(x="Lag", y="Variogram", title="Variogram of viscosity")+
  theme_classic()+
  theme(plot.title=element_text(size=rel(1.5),
                                face="bold",
                                hjust=0.5),
        axis.title=element_text(size=rel(1.5)),
        legend.position="bottom")