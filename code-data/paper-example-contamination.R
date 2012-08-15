library(RColorBrewer)
cols <- c("white",brewer.pal(9, "Greys"), "Red", "Pink")
reds <- brewer.pal(5, "Reds")[-1]
cols <- c(cols[1:3], reds[1], cols[4:6], reds[2], cols[7:9], reds[3], cols[10:11])



cols <- c("white", brewer.pal(9, "Blues"), "black")


frame <- data.frame(expand.grid(k=c(1:5, 5*(2:10)), n1=1500))
frame$n2 <- with(frame, n1/100*k)
frame$n1 <- with(frame, n1- n2)

dframe <- NULL
set.seed(20120416)
for (i in 1:10) {
  x <- c(rnorm(frame[i,]$n1), 4 + rnorm(frame[i,]$n2))
  dframe <- rbind(dframe, data.frame(k=frame[i,]$k, x=x))
}

library(ggplot2)
qplot(data=dframe, x, geom="histogram", binwidth=0.25) + facet_wrap(facets=~k, ncol=5)
ggsave("images/xpl-histogram.pdf", width=12, height=6)
qplot(data=dframe, factor(k), x, geom="boxplot", group=k) + xlab("Amount of contaminated data (in percent)") +ylab("")
ggsave("images/xpl-boxplot.pdf", width=10, height=5)

pdf("images/xpl-lvplot.pdf", width=10, height=5)
par(mar=c(4,2,1,1))
LVboxplot(dframe$x~dframe$k, col=cols, horizontal=FALSE, xlab="Amount of contaminated data (in percent)", ylab="")
dev.off()
