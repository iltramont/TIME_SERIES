path<-"C:/users/Enric/Downloads/"
name <- 'housing.csv'
filename<-paste(path, name, sep="")

data <- read.csv(filename, header=TRUE, sep=";", stringsAsFactors=FALSE);
head(data)

model.Age = lm(housePrice ~ houseAge, data = data)

# Display model summary
summary(model.Age)

AIC(model.Age)

model.MRT = lm(housePrice ~ distanceMRT, data = data)

# Derive and display AIC score for the two models
cat("AIC(model.MRT):", AIC(model.MRT),"\nAIC(model.Age):", AIC(model.Age))

# Build linear regression model
model.Stores = lm(housePrice ~ nearbyStores, data = data)

# Derive and display AIC score for the three models
cat("AIC(model.MRT):", AIC(model.MRT),"\nAIC(model.Age):", AIC(model.Age),"\nAIC(model.Stores):",AIC(model.Stores))


