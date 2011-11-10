library(reshape2)
library(ggplot2)
library(stringr)

err_raw <- read.csv("letter-val-errors.csv")
err <- melt(err_raw, id = c("name", "i"))
err$sigma <- as.numeric(str_replace(err$variable, "X", ""))
err$variable <- NULL


breaks <- 10^(2:7)
labels <- format(breaks, sci = F, trim = T)

qplot(i, value, data = err, geom = "line", linetype = factor(sigma)) +
  geom_text(aes(label = sigma, x = i + 0.25), data = subset(err, i == 20),
    hjust = 0) + 
  geom_point() + 
  scale_y_log10(breaks = breaks, labels = labels) + 
  theme_bw() + 
  labs(x = "Letter value", y = "Number of observations") +
  opts(legend.position = "none") +
  xlim(1, 21)
  
ggsave("../images/letter-val-errors.pdf", width = 6, height = 4)
