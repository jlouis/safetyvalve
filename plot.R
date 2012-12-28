library(ggplot2)

x <- read.csv("trace.out", header=TRUE)
z <- ggplot(x, aes(Time, QSize, color=Sojourn))

pdf("dequeueing.pdf")
z + geom_point(size=0.5) + geom_jitter(size=0.8)
dev.off()

y <- ggplot(x, aes(QSize, Sojourn))
pdf("qs_per_sojourn.pdf")
y + geom_point() + geom_jitter(size=0.8)
dev.off()
