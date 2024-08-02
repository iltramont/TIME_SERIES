data <- read.csv("datasets/blue_and_gorgonzola_cheese.csv",
                 header=TRUE)

y <- data$Production.103_lb.
x <- data$Year
plot(x, y, type="l", col.axis="blue", ylab="", xlab="")

# Linear regression
model <- lm(Production.103_lb. ~ Year, data=data)

print(model)

abline(model, col="blue", lty=2, lwd=1)

legend("bottomright",
       legend=c("Actual", "fits"),
       col=c("black", "blue"),
       lty=1:2,
       cex=0.8)
title(main="Blue and gorgonzola chees dataset",
      sub="1950-1997",
      xlab="Year",
      ylab="Production",
      col.main="red")

res <- rstandard(model)
qqnorm(res)
qqline(res)
hist(res, main="Histogram of residuals")


############
# ANOVA TEST
############

satisfaction_data <- read.csv("datasets/dataset_Patient_Satisfaction_Survey_Data.csv")
model <- lm(satisfaction ~ age + severity,
            data=satisfaction_data)
print(summary(model))
# ANOVA
anova <- aov(model)
print(summary(anova))


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
  Comp.hw1 <- HoltWinters(complaints, beta = FALSE, gamma = FALSE) 
  
  # Note that this function returns an object
  # that can be of course plotted
  plot(Comp.hw1)
  print(Comp.hw1)
  
  # From the Holt-Winters, print the SSE variable
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
