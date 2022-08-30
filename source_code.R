setwd("C:/Users/Kuah Jia Chen/Documents/Monash_Resources/Sem 1 2022/FIT3152/Assignment 2")

# import necessary libraries
library(tree)
library(e1071) 
library(ROCR)
library(rpart)
library(randomForest)
library(adabag)
detach("package:neuralnet", unload = TRUE)

# Create the data set 
rm(list = ls())
WAUS <- read.csv("WarmerTomorrow2022.csv")
L <- as.data.frame(c(1:49))
set.seed(32286988) # Your Student ID is the random seed
L <- L[sample(nrow(L), 10, replace = FALSE),] # sample 10 locations
WAUS <- WAUS[(WAUS$Location %in% L),]
WAUS <- WAUS[sample(nrow(WAUS), 2000, replace = FALSE),] # sample 2000 rows

options(digits=3) # set the number of significant digits to a sensible value, e.g 3

# question 1

# check the number of rows with a NA value in the WarmerTomorrow column
nrow(WAUS[is.na(WAUS$WarmerTomorrow),])

# the proportion of days when it is warmer than the previous day and it is not a NA
nrow(WAUS[WAUS$WarmerTomorrow==1 & !is.na(WAUS$WarmerTomorrow),])/nrow(WAUS) * 100

# the proportion of days when it is cooler than the previous day and it is not a NA
nrow(WAUS[WAUS$WarmerTomorrow==0 & !is.na(WAUS$WarmerTomorrow),])/nrow(WAUS) * 100

# the descriptions of the predictor for real-valued attributes 
# (excluding target, which is WarmerTomorrow)
summary(WAUS[c(5:9,11,14:19,22:23)]) # use summary function only for those columns that are real-valued

# calculate the standard deviation 
selected_columns = c(5:9,11,14:19,22:23) # no need include target
standard_deviation_dataframe = data.frame(matrix(ncol = 2, nrow = 0))
colnames(standard_deviation_dataframe) <- c("Variables","Standard Deviation")
for (i in 1:ncol(WAUS)){
  if (i %in% selected_columns){
    current_column_name = colnames(WAUS[i])
    sd_value = sd(WAUS[,i] ,na.rm=TRUE) 
    standard_deviation_dataframe[nrow(standard_deviation_dataframe) + 1,] <- c(current_column_name,signif(sd_value,3))
    }
}
standard_deviation_dataframe

# question 2

# omit the first three variables (i.e., Day, Month and Year)
new.WAUS = WAUS[4:24]

# change the data type of categorical data to factor
# The reason I did not use a for loop to do the following is 
# because if I use a for loop, I will change the datatype of 
# a particular column using the index, however I am afraid I 
# might accidentally use the wrong index, therefore I manually 
# change the data type of each column. By doing so, the marker 
# also can visualise the data type of which columns has changed to factor 
# easily
new.WAUS$Location=factor(new.WAUS$Location)
new.WAUS$WindGustDir=factor(new.WAUS$WindGustDir)
new.WAUS$WindDir9am=factor(new.WAUS$WindDir9am)
new.WAUS$WindDir3pm=factor(new.WAUS$WindDir3pm)
new.WAUS$Cloud9am=factor(new.WAUS$Cloud9am)
new.WAUS$Cloud3pm=factor(new.WAUS$Cloud3pm)
new.WAUS$WarmerTomorrow=factor(new.WAUS$WarmerTomorrow)

# remove rows with NAs 
new.WAUS = new.WAUS[complete.cases(new.WAUS),]
# new.WAUS = new.WAUS[!is.na(new.WAUS$WarmerTomorrow),]

# question 3
set.seed(32286988)
new.WAUS.train.row = sample(1:nrow(new.WAUS), 0.7*nrow(new.WAUS))
new.WAUS.train = new.WAUS[new.WAUS.train.row,]
new.WAUS.test = new.WAUS[-new.WAUS.train.row,]

# question 4

# Hi, since all the ensemble classifiers use some randomisation for sample selection,
# if I accidentally run the model fitting code twice, my confusion matrix will be
# changed. Hence, the output will not be the same as I stated in Part 5. However,
# when this happen, please rerun everything again from the beginning (including
# the code to clear the environment), then the confusion matrix will be the 
# same again as I stated in Part 5. Thank you

# Decision Tree
# fit decision tree model to predict WarmerTomorrow
Decision.Tree.new.WAUS.fit = tree(WarmerTomorrow ~ ., data = new.WAUS.train)

# Naive Bayes 
# fit naive bayes model to predict WarmerTomorrow
Naive.Bayes.new.WAUS.fit = naiveBayes(WarmerTomorrow~., data = new.WAUS.train)

# Bagging
# fit bagging model to predict WarmerTomorrow
Bagging.new.WAUS.fit = bagging(WarmerTomorrow~., data = new.WAUS.train, mfinal=10)

# Boosting
# fit boosting model to predict WarmerTomorrow
Boosting.new.WAUS.fit = boosting(WarmerTomorrow~., data = new.WAUS.train, mfinal=5)

# Random Forest
# fit random forest model to predict WarmerTomorrow
Random.Forest.new.WAUS.fit = randomForest(WarmerTomorrow ~., data = new.WAUS.train)



# question 5

# Decision tree
# create confusion matrix using prediction and check accuracy
D.predict = predict(Decision.Tree.new.WAUS.fit, new.WAUS.test, type = "class")
table(predicted = D.predict ,actual = new.WAUS.test$WarmerTomorrow)

# Accuracy
(53+43)/(53+43+26+45)


# Navie Bayes
# create confusion matrix using prediction and check accuracy
Naive.Bayes.predict = predict(Naive.Bayes.new.WAUS.fit, new.WAUS.test)
table(predicted = new.WAUS.test$WarmerTomorrow, predicted = Naive.Bayes.predict)

# Accuracy
(57+51)/(57+51+37+22)


# Bagging
# check the accuracy using the confusion matrix
Bag.predict = predict.bagging(Bagging.new.WAUS.fit,new.WAUS.test)
Bag.predict$confusion

# Accuracy
(39+68) / (39+40+20+68)


# Boosting
# check the accuracy using the confusion matrix
Boost.predict = predict.boosting(Boosting.new.WAUS.fit, newdata = new.WAUS.test)
Boost.predict$confusion

# Accuracy 
(44+58)/(44+30+35+58)


# Random Forest
# check the accuracy using the confusion matrix
Random.Forest.predict = predict(Random.Forest.new.WAUS.fit, new.WAUS.test)
table(predicted = Random.Forest.predict ,actual = new.WAUS.test$WarmerTomorrow)

# Accuracy 
(40+63)/(40+25+39+63)


# question 6

# Decision tree
# do predictions as probabilities to get the confidence and draw ROC
Decision.Tree.predict.prob = predict(Decision.Tree.new.WAUS.fit, new.WAUS.test, type = "vector")
# computing a simple ROC curve (x-axis: fpr, y-axis: tpr)
# labels are actual values, predictors are probability of class low
decision.tree.pred = prediction(Decision.Tree.predict.prob[,2], new.WAUS.test$WarmerTomorrow)
dt.perf = performance(decision.tree.pred,"tpr","fpr")
plot(dt.perf, col = "steelblue", main="ROC curve for five classification models")
abline(0,1) 

# Naive Bayes
# do predictions as probabilities to get the confidence and draw ROC
Naive.Bayes.predict.prob = predict(Naive.Bayes.new.WAUS.fit, new.WAUS.test, type = "raw")
# computing a simple ROC curve (x-axis: fpr, y-axis: tpr)
# labels are actual values, predictors are probability of class low
naive.bayes.pred = prediction(Naive.Bayes.predict.prob[,2], new.WAUS.test$WarmerTomorrow)
nb.perf = performance(naive.bayes.pred,"tpr","fpr")
plot(nb.perf, add=TRUE, col = "blueviolet")

# Bagging
# do predictions as probabilities to get the confidence and draw ROC
Bagging.predict.pred = prediction(Bag.predict$prob[,2],new.WAUS.test$WarmerTomorrow)
# computing a simple ROC curve (x-axis: fpr, y-axis: tpr)
# labels are actual values, predictors are probability of class low
bag.perf = performance(Bagging.predict.pred,"tpr","fpr")
plot(bag.perf, add=TRUE, col = "darkred")

# Boosting
# do predictions as probabilities to get the confidence and draw ROC
Boosting.predict.pred = prediction(Boost.predict$prob[,2],new.WAUS.test$WarmerTomorrow)
# computing a simple ROC curve (x-axis: fpr, y-axis: tpr)
# labels are actual values, predictors are probability of class low
boost.perf = performance(Boosting.predict.pred,"tpr","fpr")
plot(boost.perf, add=TRUE, col = "chartreuse")

# Random Forest
# do predictions as probabilities to get the confidence and draw ROC
Random.Forest.predict.prob = predict(Random.Forest.new.WAUS.fit, new.WAUS.test, type="prob")
# computing a simple ROC curve (x-axis: fpr, y-axis: tpr)
# labels are actual values, predictors are probability of class low
random.forest.pred = prediction(Random.Forest.predict.prob[,2],new.WAUS.test$WarmerTomorrow)
random.forest.perf = performance(random.forest.pred,"tpr","fpr")
plot(random.forest.perf, add=TRUE, col = "darkgoldenrod1")

# AUC for Decision Tree model
decision.tree.auc = performance(decision.tree.pred, "auc")
print(as.numeric(decision.tree.auc@y.values)) # 0.602

# AUC for Naive Bayes model
naive.bayes.auc = performance(naive.bayes.pred, "auc")
print(as.numeric(naive.bayes.auc@y.values)) # 0.676

# AUC for Bagging model
bagging.auc = performance(Bagging.predict.pred, "auc")
print(as.numeric(bagging.auc@y.values)) # 0.642

# AUC for Boosting model
boosting.auc = performance(Boosting.predict.pred, "auc")
print(as.numeric(boosting.auc@y.values)) # 0.605

# AUC for Random Forest model
random.forest.auc = performance(random.forest.pred, "auc")
print(as.numeric(random.forest.auc@y.values)) # 0.68

legend(0.55, 0.5, legend=c("Decision Tree", "Naive Bayes","Bagging","Boosting","Random Forest"),
       col=c("steelblue", "blueviolet","darkred","chartreuse","darkgoldenrod1"),lty=1,cex=0.5,
       pt.cex = 1.5,text.font = 4,lwd=2,bty="n")


# question 8

#Attribute importance

cat("\n#Decision Tree Attribute Importance\n")
print(summary(Decision.Tree.new.WAUS.fit))

# plot the decision tree model to determine which variables are the
# most important (i.e., most important variables will appear at the
# top branches)
plot(Decision.Tree.new.WAUS.fit)
text(Decision.Tree.new.WAUS.fit, pretty = 0)

cat("\n#Baging Attribute Importance\n")
print(Bagging.new.WAUS.fit$importance)

cat("\n#Boosting Attribute Importance\n")
print(Boosting.new.WAUS.fit$importance)

cat("\n#Random Forest Attribute Importance\n")
print(Random.Forest.new.WAUS.fit$importance) 


# question 9

# extract only the important attributes and convert the categorical data to factor
Question.9.WAUS = WAUS[,c(10,12,13,24)]
Question.9.WAUS$WindGustDir = factor(Question.9.WAUS$WindGustDir)
Question.9.WAUS$WindDir9am = factor(Question.9.WAUS$WindDir9am)
Question.9.WAUS$WindDir3pm = factor(Question.9.WAUS$WindDir3pm)
Question.9.WAUS$WarmerTomorrow = factor(Question.9.WAUS$WarmerTomorrow)

# remove rows with NAs 
Question.9.WAUS = Question.9.WAUS[complete.cases(Question.9.WAUS),]

# get the training and testing data set
set.seed(32286988)
Q9.WAUS.train.row = sample(1:nrow(Question.9.WAUS), 0.7*nrow(Question.9.WAUS))
Q9.WAUS.train = Question.9.WAUS[Q9.WAUS.train.row,]
Q9.WAUS.test = Question.9.WAUS[-Q9.WAUS.train.row,]

# fit the initial decision tree
Decision.Tree.Q9.WAUS.fit = tree(WarmerTomorrow ~ ., data = Q9.WAUS.train)
Q9.D.predict = predict(Decision.Tree.Q9.WAUS.fit, Q9.WAUS.test, type = "class")
table(predicted = Q9.D.predict ,actual = Q9.WAUS.test$WarmerTomorrow)
summary(Decision.Tree.Q9.WAUS.fit)

# Accuracy
(112+142)/(112+110+101+142) # 0.546

# plot the original tree
# however, this plot is not included in the report
# as I only included the most simplest one (i.e., pruned tree)
plot(Decision.Tree.Q9.WAUS.fit)
text(Decision.Tree.Q9.WAUS.fit, pretty = 0)


# perform the cross validation test
cvtest = cv.tree(Decision.Tree.Q9.WAUS.fit, FUN = prune.misclass)
cvtest 

#prune using size 3 considering lowest misclassification rate 
# and lowest cost complexity and show the performance of the simplest tree
pruned.Dfit = prune.misclass(Decision.Tree.Q9.WAUS.fit, best = 3)
summary(pruned.Dfit)
# check accuracy using the pruned tree
PD.predict = predict(pruned.Dfit, Q9.WAUS.test, type = "class")
table(predicted = PD.predict ,actual = Q9.WAUS.test$WarmerTomorrow)

# Accuracy for pruned tree
(112+142)/(112+110+101+142) # 0.546

# do predictions as probabilities to get the confidence and draw ROC
Q9.Decision.Tree.predict.prob = predict(Decision.Tree.Q9.WAUS.fit, Q9.WAUS.test, type = "vector")
# computing a simple ROC curve (x-axis: fpr, y-axis: tpr)
# labels are actual values, predictors are probability of class low
Q9.decision.tree.pred = prediction(Q9.Decision.Tree.predict.prob[,2], Q9.WAUS.test$WarmerTomorrow)

# AUC for Question 9 Pruned Decision Tree model
Q9.decision.tree.auc = performance(Q9.decision.tree.pred, "auc")
print(as.numeric(Q9.decision.tree.auc@y.values)) # 0.547

# plot the simplest tree
plot(pruned.Dfit)
text(pruned.Dfit, pretty = 0)

# question 10

# extract only the attributes that are needed and convert the categorical data to factor
Question.10.WAUS = WAUS[,c(6,9,10,12,13,16,20,21,22,24)]
Question.10.WAUS$WindGustDir = factor(Question.10.WAUS$WindGustDir)
Question.10.WAUS$WindDir9am = factor(Question.10.WAUS$WindDir9am)
Question.10.WAUS$WindDir3pm = factor(Question.10.WAUS$WindDir3pm)
Question.10.WAUS$WarmerTomorrow = factor(Question.10.WAUS$WarmerTomorrow)
Question.10.WAUS$Cloud9am=factor(Question.10.WAUS$Cloud9am)
Question.10.WAUS$Cloud3pm=factor(Question.10.WAUS$Cloud3pm)

# remove rows with NAs 
Question.10.WAUS = Question.10.WAUS[complete.cases(Question.10.WAUS),]

# get the training and testing data set
set.seed(32286988)
Q10.WAUS.train.row = sample(1:nrow(Question.10.WAUS), 0.7*nrow(Question.10.WAUS))
Q10.WAUS.train = Question.10.WAUS[Q10.WAUS.train.row,]
Q10.WAUS.test = Question.10.WAUS[-Q10.WAUS.train.row,]

# fit the Bagging model
Bagging.Q10.WAUS.fit = bagging(WarmerTomorrow~., data = Q10.WAUS.train, mfinal=29)
# check the accuracy using the confusion matrix
Bag.Q10.predict = predict.bagging(Bagging.Q10.WAUS.fit,Q10.WAUS.test)
Bag.Q10.predict$confusion # 0.672 mfinal = 29

# Accuracy
(62+65)/(62+65+36+26)

# do predictions as probabilities to get the confidence and draw ROC
Bagging.predict.Q10.pred = prediction(Bag.Q10.predict$prob[,2],Q10.WAUS.test$WarmerTomorrow)
# computing a simple ROC curve (x-axis: fpr, y-axis: tpr)
# labels are actual values, predictors are probability of class low
bag.Q10.perf = performance(Bagging.predict.Q10.pred,"tpr","fpr")
plot(bag.Q10.perf, col = "steelblue",main="ROC curve for the best model using Bagging classifier")
abline(0,1)

# AUC for Bagging model
bagging.Q10.auc = performance(Bagging.predict.Q10.pred, "auc")
print(as.numeric(bagging.Q10.auc@y.values)) # 0.698

# question 11
# clean up the environment before starting
library(neuralnet)
# select the same attributes as question 10
Question.11.WAUS = WAUS[,c(6,9,10,12,13,16,20,21,22,24)] 

# remove rows with NAs and convert Cloud9am and Cloud3pm to factor as they are initially integer
Question.11.WAUS = Question.11.WAUS[complete.cases(Question.11.WAUS),]
Question.11.WAUS$Cloud9am=factor(Question.11.WAUS$Cloud9am)
Question.11.WAUS$Cloud3pm=factor(Question.11.WAUS$Cloud3pm)

# create the indicator columns for each categorical attribute using model.matrix
Q11.WAUS.mm = model.matrix(~WindGustDir+WindDir9am+WindDir3pm+Cloud3pm+Cloud9am, data=Question.11.WAUS)
# merge the output of model.matrix with "Question.11.WAUS"
Question.11.WAUS = cbind(Question.11.WAUS,Q11.WAUS.mm)
# tidy up the merged data frame such that it only contains necessary columns
Question.11.WAUS = Question.11.WAUS[,c(1,2,6,9,12:72,10)]

# get the training and testing data set
set.seed(32286988)
Q11.WAUS.train.row = sample(1:nrow(Question.11.WAUS), 0.7*nrow(Question.11.WAUS))
Q11.WAUS.train = Question.11.WAUS[Q11.WAUS.train.row,]
Q11.WAUS.test = Question.11.WAUS[-Q11.WAUS.train.row,]

# Fit the neural network
Q11.WAUS.nn = neuralnet(WarmerTomorrow~., Q11.WAUS.train, hidden = 2)

Q11.WAUS.nn.pred = compute(Q11.WAUS.nn, Q11.WAUS.test)

# Binomial classification: predict the probability of belonging to class 1
# and if the probability is less than 0.5 consider it predicted as class 0 
Q11.WAUS.nn.predr = round(Q11.WAUS.nn.pred$net.result,0)

# construct the confusion matrix
table(observed = Q11.WAUS.test$WarmerTomorrow, predicted = Q11.WAUS.nn.predr)

# Accuracy
(15+96)/(15+73+5+96)
