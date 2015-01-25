##Second try; simple decision tree model following Trevor Stephen's tutorial

# Set working directory and import datafiles
setwd("~/Desktop/Kaggle/Predicting Titanic Survivors")
test <- read.csv("~/Desktop/Kaggle/Predicting Titanic Survivors/data/test.csv")
train <- read.csv("~/Desktop/Kaggle/Predicting Titanic Survivors/data/train.csv")

#Decision tree
library(rpart)
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class")
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(fit)
Prediction <- predict(fit, test, type = "class")

#Create file for submission
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "simpledtree.csv", row.names = FALSE)

