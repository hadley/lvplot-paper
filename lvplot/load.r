setwd("~/statgraphics/lvplot/")
sapply(dir("R", full=TRUE), source)
LVboxplot(rpois(1000,100) ~ 1:4)