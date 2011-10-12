library(RColorBrewer)

i <- 1
res <- data.frame(matrix(NA, ncol=3, nrow=200))
n <- 10000000

for (i in 1:10) {
z <- rnorm(n)
x <- rpois(n, lambda=2)

z <- round(z,2)
#z <- round(z,1)
pc1a <- proc.time()
try(LVboxplot(z~x), TRUE)
pc1b <- proc.time()-pc1a


df <- as.data.frame(xtabs(~z+x))
df <- df[df$Freq>0,]
df$z <- as.numeric(as.character(df$z))
df$x <- as.numeric(as.character(df$x))
pc2a <- proc.time()
LVboxplot(df$z~df$x, weight=df$Freq)
pc2b <- proc.time()-pc2a

pc1b
pc2b
res[i,1] <- n
res[i,2] <- pc1b[3]
res[i,3] <- pc2b[3]
}

delays <- read.csv("/Users/heike/Documents/students/Mahbub/delays-by-scheduled-arrival.csv")
d <- subset(delays, (ArrDelay > -60) & (ArrDelay < 250))

d <- subset(delays, CRSArrTime < 2400)
LVboxplot(d$ArrDelay~d$CRSArrTime%/%100, weight=d$count, horizontal=F, ylim=c(-60,250))


LVboxplot(rnorm(10))

z <- rnorm(10000)
x <- rt(10000,df=10)
LVbagplot(z~x, col="red")

z1 <- rnorm(10000)
z2 <- rnorm(150000, mean=5)
x1 <- rnorm(10000)
x2 <- rnorm(150000, mean=5)
z <- c(z1,z2)
x <- c(x1,x2)
x<-round(x,2)
z<-round(z,2)
LVbagplot(z~x, col="red")

# 10 simulations each for varying n
gaussian, cauchy or t2
count number of outliers


