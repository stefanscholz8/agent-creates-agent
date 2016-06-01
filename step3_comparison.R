library(ggplot2)

6.1698 
1.09528 * 60 - 6.1698

1.317403 * 60 - 60
((1.075141 * 60 * 60 - 1.317403 * 60 - 3600) / 60 - 3) * 60

data <- data.frame(Time = c(6.1698, 59.547, 79.04418, 3791.463), Method= c("ACA", "ACA", "Matching", "Matching"), Type = c("Calculations", "Value set", "Value set", "Calculations"))
p <- ggplot(data=data, aes(x=Method, y=Time, fill=Type)) + geom_bar(stat="identity") + coord_flip()
p <- p + scale_y_continuous("Time in seconds", breaks=c(0, 900, 1800, 2700, 3600, 4500)) + scale_fill_grey(start = 0.6, end = 0.4)
p
ggsave("timecomp.pdf", device="pdf", height=1, width=4, units='in', scale=2)
