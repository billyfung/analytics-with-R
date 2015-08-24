#predictin supreme court outcomes
#dep = vote to reverse, 0 = affirm, 1 = reverse
stevens <- read.csv("~/Documents/analytics-with-R/Trees/stevens.csv")
library(caTools)
set.seed(100)
spl = sample.split(stevens$Reverse, 0.7)
train = subset(stevens, spl==TRUE)
test = subset(stevens, spl==FALSE)
#CART stuff
library(rpart)
library(rpart.plot)
#lets create the classification tree
stevensTree=rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train,
                  method = "class", minbucket = 25)
prp(stevensTree)
predictCART = predict(stevensTree, newdata=test, type="class")
#accuracy of the model checked via confusion matrix
table(test$Reverse, predictCART)
accurary = (41 + 71) /(41 + 71 + 36+ 22)
library(ROCR)
predictROC = predict(stevensTree, newdata=test)
#predictROC shows the probability of outcome for each observation
pred = prediction(predictROC[,2], test$Reverse)
perf = performance(pred, "tpr", "fpr") #false positive vs true positive 
plot(perf)
#check the AUC
as.numeric(performance(pred, "auc")@y.values)

library(randomForest)
stevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train,
                             nodesize=25, entry=100)
#if classification, make sure outcome is a factor, otherwise will do regression
train$Reverse = as.factor(train$Reverse)
test$Reverse = as.factor(test$Reverse)
predictForest = predict(stevensForest, newdata=test)
table(test$Reverse, predictForest)
accuracyforest = (41+74)/(41+36+19+74)
