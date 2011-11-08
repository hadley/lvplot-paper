library(lvplot)
library(RColorBrewer)
cols <- c("white", brewer.pal(9, "Blues"),"black")

sessions <- read.csv("sessions.csv")

names(sessions) <- c("time","length","SIP","DIP","Dport","Sport","Npacket","Nbyte")
sessions[,3:6] <- round(65536*sessions[,3:6])

cut2.byte <- cut(log(1+sqrt(sessions[,8])), c(-1,0,seq(3,5.4,by=.1),5.6, 5.8, 6.0, 6.5, 9.0))

pdf("../images/box2.pdf", width = 10, height = 5)
boxplot(split(log(1+sqrt(sessions[,2])),cut2.byte),varwidth=T,notch=T)
title("Message Duration and Length",xlab="log(1 + sqrt{Nbyte})", ylab="log(1 + sqrt({duration})")
dev.off()

pdf("../images/lvbox2.pdf", width = 10, height = 5)
with(sessions, LVboxplot(log(1+sqrt(length))~cut2.byte, horizontal=FALSE, xlab="log(1 + sqrt{Nbyte})", ylab="log(1 + sqrt({duration})", col=cols))
dev.off()

pdf("../images/boxplots.pdf", width=6, height=2)
par(mfrow=c(2,3), mar=c(4.1, 1, 0, 1))
n <- 10000
boxplot(rnorm(n), xlab="", horizontal=TRUE)
boxplot(rexp(n), xlab="", horizontal=TRUE)
boxplot(runif(n), xlab="", horizontal=TRUE)
LVboxplot(rnorm(n), xlab="Gaussian, n=10,000", horizontal=TRUE, col=cols)
LVboxplot(rexp(n), xlab="Exponential, n=10,000", horizontal=TRUE, col=cols)
LVboxplot(runif(n), xlab="Uniform, n=10,000", horizontal=TRUE, col=cols)
dev.off()
###################
pdf("../images/t-dist.pdf", width=6, height=2)
par(mfrow=c(2,3), mar=c(4.1, 1, 0, 1))
n <- 10000
t2 <- rt(n, df=2)
t3 <- rt(n, df=3)
t9 <- rt(n, df=9)

with(census, boxplot(t2, horizontal=TRUE, xlab=""))
with(census, boxplot(t3, horizontal=TRUE, xlab=""))
with(census, boxplot(t9, horizontal=TRUE, xlab=""))
with(census, LVboxplot(t2, horizontal=TRUE, xlab="t distribution, df=2, n=10,000", col=cols))
with(census, LVboxplot(t3, horizontal=TRUE, xlab="t distribution, df=3, n=10,000", col=cols))
with(census, LVboxplot(t9, horizontal=TRUE, xlab="t distribution, df=9, n=10,000", col=cols))
dev.off()

###################

census <- read.csv("counties.csv")

pdf("../images/counties-lvpop-a.pdf", height=2, width=6)
par(mar=c(4.1, 1, 1, 1))
with(census, boxplot(totalpop, horizontal=TRUE, xlab="total population"))
dev.off()
pdf("../images/counties-lvpop-b.pdf", height=2, width=6)
par(mar=c(4.1, 1, 1, 1))
with(census, LVboxplot(totalpop, horizontal=TRUE, xlab="total population", col=cols))
dev.off()

pdf("../images/counties-lvpop-c.pdf", height=2, width=6)
par(mar=c(4.1, 1, 1, 1))
with(census, boxplot(log(totalpop), horizontal=TRUE, xlab="(log) total population"))
dev.off()

pdf("../images/counties-lvpop-d.pdf", height=2, width=6)
par(mar=c(4.1, 1, 1, 1))
with(census, LVboxplot(log(totalpop), horizontal=TRUE, xlab="(log) total population", col=cols))
dev.off()

###################

pdf("../images/counties-bag.pdf")
par(mar=c(4.1, 4.1, 1, 1))
require(aplpack)
with(census, bagplot(Latitude/10000, JanTmp/10, xlab="Latitude (in degrees)", ylab="Temperatures in January (in F)"))
dev.off()

pdf("../images/counties-lvbag.pdf")
par(mar=c(4.1, 4.1, 1, 1))
with(census, LVbagplot(Latitude/10000, JanTmp/10, xlab="Latitude (in degrees)", ylab="Temperatures in January (in F)", col=rev(cols)[-(1:2)]))
dev.off()
