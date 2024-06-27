filename<-paste(path, name, sep="")
complaints <- read.csv("datasets/motororg.dat", header = T)
head(complaints)


Comp.ts <- ts(complaints, start = c(1996, 1), freq = 12)
Comp.ts


plot(Comp.ts, xlab = "Time / months", ylab = "Complaints")
title("Complaints time-series")

Comp.hw1 <- HoltWinters( Comp.ts, beta = FALSE, gamma = FALSE) 
  
# Note that this function returns an object
# that can be of course plotted
plot(Comp.hw1)


print(Comp.hw1)

Comp.hw1$SSE

Comp.hw2 <- HoltWinters(Comp.ts, alpha = 0.2, beta=0, gamma=0.1)
Comp.hw2

plot(Comp.hw2)



