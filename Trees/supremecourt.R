#predictin supreme court outcomes
#dep = vote to reverse, 0 = affirm, 1 = reverse
stevens <- read.csv("~/Documents/analytics-with-R/Trees/stevens.csv")
library(caTools)
set.seed(200)
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
accuracyforest = (39+73)/(42+35+16+77)

#while using trees, how do we determine the optiminal minbucket value?
#k fold cross validation
library(caret)
library(e1071)
numFolds = trainControl(method = "cv", number =10)
#numbers from 0.01 to 0.5 in increments of 0.01
cpGrid = expand.grid(.cp = seq(0.01,0.5,0.01))
train(Reverse~Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train,
      method="rpart", trControl=numFolds, tuneGrid = cpGrid)
#The final value used for the model was cp = 0.21
stevensTreeCV=rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train,
                  method = "class", cp = 0.18)
predictCV = predict(stevensTreeCV, newdata=test, type ="class")
table(test$Reverse, predictCV)
prp(stevensTreeCV)
