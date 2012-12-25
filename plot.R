library(ggplot2)

x <- read.csv("trace.out", header=TRUE)
z <- ggplot(x, aes(Time, QSize, color=Sojourn))

pdf("dequeueing.pdf")
z + geom_point(alpha = 1/5)
dev.off()
