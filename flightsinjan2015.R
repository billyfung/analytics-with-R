#let's look at some flight data from jan2015 in the usa
jan2015flights <- read.csv("~/Downloads/jan2015flights.csv")
summary(jan2015flights)
#max departure delay is 1988 minutes, average 9.8 minutes
#top 5 airports of origin are: ATL, ORD, DFW, LAX, DEN out of 312 airports
#top 5 destinations are: ATL, ORD, DFW, LAX, DEN
#least busy airports are: ADK, PPG, UST, OTH, ILG
#Adak Island (alaska), pago pago, Northeast Florida Regional, southwest oregon, new castle(philly)
table(jan2015flights$CANCELLED)
#percentage cancelled 
pcancel = 11982 / (11982 + 457986)
#let's look at only the cancelled flights
cancelled <- subset(jan2015flights, CANCELLED == 1)
sort(table(cancelled$ORIGIN))
#we can see that most cancelled flight originated from ORD, LGA, EWR, BOS, DFW
m <- ggplot(jan2015flights, aes(x=ORIGIN))
m + geom_histogram(aes(y = ..count..))+theme(axis.ticks = element_blank(),axis.text.x = element_blank())+
  ggtitle("Number of flights per airport") + scale_y_continuous(breaks=seq(0, 30000, 5000)) 

library(dplyr)
#i'd like to limit the data to the top 10 airports 
d2 <- jan2015flights %>%
  count(ORIGIN) %>%
  top_n(10) %>%
  arrange(n, ORIGIN) 

d <- ggplot(aes(x = ORIGIN) +
  geom_bar()
  
  
qplot(ORIGIN, data=jan2015flights, geom="histogram")
jan2015flights <- within(jan2015flights, 
       ORIGIN <- factor(ORIGIN, 
                          levels=names(sort(table(ORIGIN), 
                                            decreasing=TRUE))))
