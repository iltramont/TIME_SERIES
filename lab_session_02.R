sunspots<-function(){
  data<-read.csv("datasets/solarspots_data.csv")
  y<-data$spots
  plot(data$year, y, type = "l",
       col.axis="blue")
  title(main = "Sunspots dataset", sub = "1700-2014",
        xlab = "Year", ylab = "Number of solar spots",
        cex.main = 2,   font.main= 4, col.main= "red",
        cex.sub = 0.75, font.sub = 3, col.sub = "green",
        col.lab ="darkblue")
  
  # Replace zeroes with NA
  data[data == 0] <- NA
  
  # Calculate geometric mean
  T<-dim(data)[1]
  y_dot<-exp(sum(log(data$spots), na.rm = TRUE)/T)
  y_dot
  
  for (lambda in 0:3)
  {
    if (lambda == 0)
    {
      y <-y_dot*log(y)
      print(y)
    }
    else
    {
      y<-(y^lambda -1)/(lambda*y_dot^(lambda-1))
      print(y)
    }
    
    plot(data$year, y, type = "l", main = "", xlab="", ylab="",
       col.axis="red")
   title(main = paste("Power transformation with lambda", lambda), 
        xlab = "Year", ylab = "Y",
        cex.main = 2,   font.main= 4, col.main= "red",
        cex.sub = 0.75, font.sub = 3, col.sub = "green",
        col.lab ="darkblue")
  }

}
sunspots()
