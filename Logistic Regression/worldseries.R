baseball <- read.csv("~/Documents/analytics-with-R/Logistic Regression/baseball.csv")
length(table(baseball$Year)) #count number of years 
#we only care about playoff years
baseball <- subset(baseball, Playoffs == 1)
#NumCompetitors: the number of total teams making the playoffs in the year of a particular team/year pair
PlayoffTable = table(baseball$Year)
baseball$NumCompetitors = PlayoffTable[as.character(baseball$Year)] 
table(baseball$NumCompetitors)
#add variable to check if won world series
baseball$WorldSeries = as.numeric(baseball$RankPlayoffs == 1)
table(baseball$WorldSeries)
#check significant vars by creating bivariate models 
bivarYear = glm(WorldSeries~Year, data=baseball, family="binomial")
bivarRA = glm(WorldSeries~RA, data=baseball, family="binomial")
bivarRank = glm(WorldSeries~RankSeason, data=baseball, family="binomial")
bivarNum= glm(WorldSeries~NumCompetitors, data=baseball, family="binomial")
#the significant vars = Year, RA, RankSeason, NumCompetitors
#create multivariate model
mod1=glm(WorldSeries~Year+RA+RankSeason+NumCompetitors, data=baseball, family="binomial")
summary(mod1)
#looks like none of the variables are significant now, look at correlation
cor(baseball[c("Year", "RA", "RankSeason", "NumCompetitors")])
Model1 = glm(WorldSeries ~ Year + RA, data=baseball, family=binomial)

Model2 = glm(WorldSeries ~ Year + RankSeason, data=baseball, family=binomial)

Model3 = glm(WorldSeries ~ Year + NumCompetitors, data=baseball, family=binomial)

Model4 = glm(WorldSeries ~ RA + RankSeason, data=baseball, family=binomial)

Model5 = glm(WorldSeries ~ RA + NumCompetitors, data=baseball, family=binomial)

Model6 = glm(WorldSeries ~ RankSeason + NumCompetitors, data=baseball, family=binomial)
#seems like nothing is significant 