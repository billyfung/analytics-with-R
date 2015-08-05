songs <- read.csv("~/Documents/analytics-with-R/songs.csv")
str(songs)
table(songs$artistname == "Michael Jackson") #find # of songs by MJ
MJ = subset(songs, artistname == "Michael Jackson")
MJ[c("songtitle", "Top10")] #MJ songs in the top10
table(songs$timesignature)
songs$songtitle[which.max(songs$tempo)]
#create training and testing set
songsTrain = subset(songs, year<="2009")
songsTest = subset(songs, year=="2010")
#let's create a prediction model, logistic regression since output is binary
nonvars = c("year", "songtitle", "artistname", "songID", "artistID") #remove variables that are indep
songsTrain = songsTrain[,!(names(songsTrain)%in% nonvars)]
songsTest = songsTest[,!(names(songsTest)%in% nonvars)]
songsLog1 = glm(Top10~., data = songsTrain, family = binomial)
summary(songsLog1)
#check for colinearity 
cor(songsTrain[c("loudness", "energy")])
#new model without var "loudness"
songsLog2 = glm(Top10~ . -loudness, data = songsTrain, family=binomial)
summary(songsLog2)
songsLog3 = glm(Top10~ . -energy, data = songsTrain, family=binomial)
#let's use model3 to make predictions
predict1 = predict(songsLog3, type = "response", newdata = songsTest)
table(songsTest$Top10, predict1 >= 0.45)
#compare to baseline model
table(songsTest$Top10)
