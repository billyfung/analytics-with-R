library(ggthemes)
library(ggplot2)
jobapps <- read.csv("~/Dropbox/jobapps.csv")
sort(table(jobapps$Location))
#sort location & Replied
jobapps <- within(jobapps, 
                  Location <- factor(Location, 
                                    levels=names(sort(table(Location), 
                                                      decreasing=TRUE))))
m <- ggplot(jobapps, aes(x=Location))
m + geom_bar(aes(y = ..count.., fill = Replied))+
  theme_tufte(base_size=14, ticks=F)+
    theme(axis.ticks = element_blank(), 
    axis.text.x = element_text(angle = 90, hjust = 1),
    axis.title.x = element_blank(),
    panel.background = element_blank())+
#   geom_hline(yintercept=seq(1, 5), col="white", lwd=1) +
#   ggtitle("Applications per location") + 
  scale_y_continuous(breaks=seq(0, 30, 5)) +
  annotate("text", x = 13.5, y = 22, adj=1,  family="serif",
           label = c("Applications per location\n21 Replies 91 Total"))

mean(jobapps$Days.taken.to.reply, na.rm=TRUE)

a <-  ggplot(jobapps[!is.na(jobapps$Days.taken.to.reply), ], aes(x=Location, y=Days.taken.to.reply)) 
a + geom_bar(stat="identity") +
  theme_tufte(base_size=14, ticks=F) +
  theme(axis.ticks = element_blank(), 
       # axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(breaks=seq(0, 70, 5)) +
  ylab("Days")+
  annotate("text", x = 7, y = 50, adj=1,  family="serif", size = 8,
           label = c("Days taken to reply"))

