options(repr.plot.width=20, repr.plot.height=10)

path<-"c:/Users/Enric/Downloads/"
name <- 'wine.dat'
filename<-paste(path, name, sep="")

df <- read.csv(filename, header=TRUE);
head(df)

sweetw <-df$sweetw
sweetw.ts <- ts(sweetw, start = c(1980,1), freq = 12)
plot(sweetw.ts, xlab= "Time (months)", ylab = "sales (1000 litres)", main = "Sales of Australian wine", cex.lab=5, cex.axis=5, cex.main=3)

d<-decompose(sweetw.ts, type='multiplicative')
d

plot(d)

sweetw.hw <- HoltWinters (sweetw.ts, seasonal = "mult")

sweetw.hw

sweetw.hw$coef ; 

sweetw.hw$SSE

plot (sweetw.hw$fitted)



plot (sweetw.hw)


