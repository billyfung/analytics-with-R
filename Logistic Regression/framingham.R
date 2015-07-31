#framingham heart study
str(framingham)
library(caTools)
#split data into training and testing set 
set.seed(1000)
set = sample.split(framingham$TenYearCHD, SplitRatio = 0.65)
train = subset(framingham, set == TRUE)
test = subset(framingham, set == FALSE)
#build logistic regression model, dependent variable tenyearchd, all other variables as indep
fLog = glm(TenYearCHD ~ ., data = train, family = binomial)
summary(fLog)
#we can see the significant variables with positive coefficients, which means higher probability of dependent variable
#use model on test set, type = response for probability
predictTest = predict(fLog, type = "response", newdata = test)
table(test$TenYearCHD, predictTest>0.5)
#this makes a confusion matrix with threshold of 0.5
#accuracy 
(1069+11)/(187+11+6+1069)
library(ROCR)
#look at AUC
ROCRpred = prediction(predictTest, test$TenYearCHD)
as.numeric(performance(ROCRpred, "auc")@y.values)
