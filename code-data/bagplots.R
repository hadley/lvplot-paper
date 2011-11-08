# Bag plots ------------------------------------------------------------------

library(aplpack)
library(RColorBrewer)
cols <- c("white", brewer.pal(9, "Blues"), "black")

census <- read.csv("counties.csv")

pdf("../images/counties-bag.pdf")
par(mar=c(4.1, 4.1, 1, 1))
with(census, bagplot(Latitude/10000, JanTmp/10, xlab="Latitude (in degrees)", ylab="Temperatures in January (in F)"))
dev.off()

pdf("../images/counties-lvbag.pdf")
par(mar=c(4.1, 4.1, 1, 1))
with(census, LVbagplot(Latitude/10000, JanTmp/10, xlab="Latitude (in degrees)", ylab="Temperatures in January (in F)", col=rev(cols)[-(1:2)]))
dev.off()
