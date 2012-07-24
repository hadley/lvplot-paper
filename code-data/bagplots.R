# Bag plots ------------------------------------------------------------------
library(lvplot)
source("lvbagplot.R")

library(RColorBrewer)
cols <- c("white", brewer.pal(9, "Blues"), "black")

census <- read.csv("counties.csv")

pdf("../images/counties-bag.pdf")
par(mar=c(4.1, 4.1, 1, 1))
bp <- with(census, aplpack::compute.bagplot(Latitude/10000, JanTmp/10, factor = 3, na.rm = FALSE, approx.limit = 10000, 
                                   dkmethod=2,precision=1,verbose=FALSE,debug.plots="no"))
foo <- with(census, plot(bp, xlab="Latitude (in degrees)", ylab="Temperatures in January (in F)", show.whiskers = FALSE))
dev.off()

pdf("../images/counties-lvbag.pdf")
par(mar=c(4.1, 4.1, 1, 1))
with(census, LVbagplot(Latitude/10000, JanTmp/10, method="apl", xlab="Latitude (in degrees)", ylab="Temperatures in January (in F)", col=rev(cols)[-(1:2)]))
dev.off()


# outlier identification
sub <- subset(data.frame(foo$outliers), (x > 35) & (y > 35))
points(sub$x, sub$y, col=2)

with(census, plot(-Longitude/10000, Latitude/10000))
points()



foo <- with(census, compute.bagplot(Latitude/10000, JanTmp/10, factor = 3, na.rm = FALSE, approx.limit = 10000, 
                dkmethod=2,precision=1,verbose=FALSE,debug.plots="no"))

qplot(sort(bp$hdepth), res[,3])
X <- sort(runif(25))
Y <- runif(25)
library(ggplot2)
bp2 <- aplpack::compute.bagplot(X,Y, factor = 3, na.rm = FALSE, 
                                dkmethod=2,precision=1,verbose=FALSE,debug.plots="no")
qplot(X,Y, colour=factor(bp2$hdepths), size=I(3))
qplot(X,Y, colour=factor(small$V3), size=I(3))
table(bp2$hdepths)
plot(bp2)

LVbagplot(X,Y, col=cols)
