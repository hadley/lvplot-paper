frame <- data.frame(expand.grid(k=c(1:5, 5*(2:10)), n1=1500))
frame$n2 <- with(frame, n1/100*k)

dframe <- NULL
set.seed(20120416)
for (i in 1:10) {
  x <- c(rnorm(frame[i,]$n1), 4 + rnorm(frame[i,]$n2))
  dframe <- rbind(dframe, data.frame(k=frame[i,]$k, x=x))
}

library(ggplot2)
qplot(data=dframe, x, geom="histogram", binwidth=0.25) + facet_wrap(facets=~k, ncol=5)
qplot(data=dframe, factor(k), x, geom="boxplot", group=k) + xlab("Amount of contaminated data (in percent)") +ylab("")

par(mar=c(4,2,1,1))
LVboxplot(dframe$x~dframe$k, horizontal=FALSE, xlab="Amount of contaminated data (in percent)")
