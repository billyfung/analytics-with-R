#Tufte graphs in R
install.packages(c("ggplot2", "ggthemes", "PerformanceAnalytics", "psych", 
                   "reshape2", "plyr", "RCurl", "devtools"))
x <- 1967:1977
y <- c(0.5,1.8,4.6,5.3,5.3,5.7,5.4,5,5.5,6,5)
plot(y ~ x, axes=F, xlab="", ylab="", pch=16, type="b")
axis(1, at=x, label=x, tick=F, family="serif")
axis(2, at=seq(1,6,1), label=sprintf("$%s", seq(300,400,20)), tick=F, las=2, family="serif")
abline(h=6,lty=2)
abline(h=5,lty=2)
text(max(x), min(y)*2.5,"Per capita\nbudget expanditures\nin constant dollars", adj=1, family="serif")
text(max(x), max(y)/1.08, labels="5%", family="serif")

x <- 1967:1977
y <- c(0.5,1.8,4.6,5.3,5.3,5.7,5.4,5,5.5,6,5)
d <- data.frame(x, y)
ggplot(d, aes(x,y)) + geom_line() + geom_point(size=3) + theme_tufte(base_size = 15) +
  theme(axis.title=element_blank()) + geom_hline(yintercept = c(5,6), lty=2) + 
  scale_y_continuous(breaks=seq(1, 6, 1), label=sprintf("$%s",seq(300,400,20))) + 
  scale_x_continuous(breaks=x,label=x) +
  annotate("text", x = c(1977,1977.2), y = c(2,5.5), adj=1,  family="serif",
           label = c("Per capita\nbudget expandures\nin constant dollars", "5%"))
