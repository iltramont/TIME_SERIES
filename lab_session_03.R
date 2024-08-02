patsat_data <- read.csv("datasets/dataset_Patient_Satisfaction_Survey_Data.csv",
                        header=TRUE,
                        sep=",",
                        fileEncoding="UTF-8-BOM")
print(patsat_data)
nrows <- dim(patsat_data)[1]
print(nrows)

# Prepare X matrix
X <- data.matrix(cbind(matrix(1, nrows, 1), patsat_data[, 1:2]))
colnames(X) <- c("1s", "age", "severity")
print(X)

# Target variable
y <- patsat_data[, 3]
print(y)

# Computations
XtX <- t(X) %*% X
print(XtX)

XtX_1 <- solve(XtX)
print(XtX_1)

Xty <- t(X) %*% y
print(Xty)

beta_hat <- XtX_1 %*% Xty
print(beta_hat)

y_hat <- beta_hat[1] + beta_hat[2] * X[, 2] + beta_hat[3] * X[, 3]
print(y_hat)

# Residuals
epsilon <- y - y_hat
hist(epsilon, main="Histogram of residuals")


# We could have obtained the same results with the following built-in formula:
model <- lm(satisfaction ~ age + severity, data = patsat_data)
summary(model)
