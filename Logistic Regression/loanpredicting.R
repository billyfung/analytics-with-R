#predicting loan repayment
loans <- read.csv("~/Documents/analytics-with-R/Logistic Regression/loans.csv")
#note the variables with missing data, will need to impute
library(mice)
set.seed(144)
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed
#split data into training and testing, indep var is not.fully.paid
library(caTools)
Y = sample.split(loans$not.fully.paid, 0.7)
loanTrain = subset(loans, Y == TRUE)
loanTest = subset(loans, Y == FALSE)
loanMod = glm(not.fully.paid~., data = loanTrain, family = "binomial" )
#use model to predict
predicted.risk = predict(loanMod, newdata =loanTest, type="response")
loanTest$predicted.risk = predicted.risk
table(loanTest$not.fully.paid, loanTest$predicted.risk > 0.5 )
#look at confusion matrix for accuracy
accuracy = 2403/(2403+13+457)
#baseline would be 2413 / 2873
#computing the test set AUC
library(ROCR)
ROCRpred = prediction(predicted.risk, loanTest$not.fully.paid)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize = TRUE)
as.numeric(performance(ROCRpred, "auc")@y.values)
#we can see that this value isn't that great, low performance at 0.5 
#let's build a smart baseline
#bivarate model, we can see that correlation affects 
loanMod2 = glm(not.fully.paid~int.rate, data = loanTrain, family = "binomial")
pred2 = predict(loanMod2, newdata = loanTest, type="response")
summary(pred2)
#looking at the summary maximum predicted probability of the loan not being paid back is .42660
ROCRpred2 = prediction(pred2, loanTest$not.fully.paid)
as.numeric(performance(ROCRpred2, "auc")@y.values)
#how to predict if an investment will be profitable or not, compounded interest
loanTest$profit = exp(loanTest$int.rate*3)-1
loanTest$profit[loanTest$not.fully.paid==1]=-1
#looking for high interest rate and low risk of not being paid back
highInterest=subset(loanTest, int.rate>=0.15)
cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]
selectedLoans = subset(highInterest, predicted.risk<=cutoff)
summary(selectedLoans)
sum(selectedLoans$profit)
