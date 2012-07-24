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

qplot(data=census,  Latitude/10000, JanTmp/10, colour=bp$hdepths)

# outlier identification
census$outlier <- with(census, (bp$hdepths < 100) & (JanTmp/10 > 20) 
                   & (Latitude/10000>32) & 
                     (resid(lm(I(JanTmp/10)~I(Latitude/10000), data=census)) > 1.6))
 
qplot(data=census,  Latitude/10000, JanTmp/10, colour= outlier) 

census$outlier <- factor(census$outlier, levels=c("TRUE", "FALSE"))
qplot(data=census, -Longitude/10000, Latitude/10000, colour=outlier) + scale_colour_brewer(palette="Set1")
ggsave("../images/map.pdf")

