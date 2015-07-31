#Election forecasting from polling data pollingdata.csv
#notice the NAs in rasmussen and surveyusa
#multiple imputation to fill in missing values based on non-missing value observation (MICE package)
library(mice)
#create a new dataframe that limits the variables used for mice, to variables used in polling 
simple = PollingData[c("Rasmussen", "SurveyUSA", "PropR", "DiffCount")]
summary(simple)
set.seed(144)
#run imputation to fill in missing values
imputed = complete(mice(simple))
PollingData$Rasmussen = imputed$Rasmussen
PollingData$SurveyUSA = imputed$SurveyUSA
#split into train/test
train = subset(PollingData, Year == 2004 | Year == 2008)
test = subset(PollingData, Year == 2012)
#let's look at baseline before creating model
table(train$Republican) 
#53% republican outcome, not a very good baseline model 
#use a smart baseline to predict outcome based on polling by checking +/- 
table(sign(train$Rasmussen))
#table below shows us the predictions of smart baseline 
table(train$Republican, sign(train$Rasmussen))

#before begin to build model, check for correlation
cor(train[c("Rasmussen", "SurveyUSA", "PropR", "DiffCount", "Republican")])
#propR most correlated to repub, try as 1st variable
mod1 = glm(Republican~PropR, data = train, family = "binomial")
summary(mod1) #AIC 19.772 
pred1 = predict(mod1, type = "response")
table(train$Republican, pred1>=0.5) #4 mistakes, same as baseline

#lets add another variable to model, search correlation matrix to find least correlated pair
mod2 = glm(Republican~SurveyUSA+DiffCount, data = train, family = "binomial")
pred2 = predict(mod2, type = "response")
table(train$Republican, pred2>=0.5) #3 mistakes, slightly better

#now to work with the testing set
#first check with baseline
table(test$Republican, sign(test$Rasmussen))
testprediction = predict(mod2, newdata = test, type = "response")
table(test$Republican, testprediction>=0.5)
#look at where the 1 mistake came from
subset(test, testprediction>=0.5 & Republican == 0)
