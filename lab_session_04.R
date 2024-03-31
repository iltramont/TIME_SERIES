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