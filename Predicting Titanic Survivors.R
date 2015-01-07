##use later for scoring models
library(pROC)
score <- auc(y_test_resp, y_test_pred)


##Download the data
setwd("~/Desktop/Kaggle/Predicting Titanic Survivors")
fileUrl_train <- "https://kaggle2.blob.core.windows.net/competitions-data/kaggle/3136/train.csv?sv=2012-02-12&se=2015-01-08T02%3A54%3A57Z&sr=b&sp=r&sig=8H11bbsaj9DQDMQ0R5WIQq9NIrLirih29FC7eW6Viqs%3D"
fileUrl_test<-"https://kaggle2.blob.core.windows.net/competitions-data/kaggle/3136/test.csv?sv=2012-02-12&se=2015-01-08T02%3A48%3A46Z&sr=b&sp=r&sig=VYoBoaeKYINoZTv45IF9lglMCBCloxTM5hjpFo%2FQmtA%3D"
download.file(fileUrl_train, destfile = "./data/train.csv", method = "curl")
download.file(fileUrl_test, destfile = "./data/test.csv", method = "curl")
list.files("./data")
train <- read.csv("./data/train.csv")
test <- read.csv("./data/test.csv")
set.seed(1965)

#wrangle the data
train$Pclass<-as.factor(train$Pclass)
test$Pclass<-as.factor(test$Pclass)
train$Ticket<-as.numeric(train$Pclass)
test$Ticket<-as.numeric(test$Pclass)
train$Survived<-as.factor(train$Survived)
train<-train[,-c(4,11)]
test<-test[,-c(3,10)]

##impute missing values--haven't tried this yet
library("caret", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")
train.imputed <- rfImpute(Survived ~ ., train)
test.imputed <- rfImpute(Pclass ~ ., test)

##Partition the data
inTrain<-createDataPartition(train.imputed$Survived, p=0.75, list=FALSE)
final_train<-train.imputed[inTrain,]
final_validation<-train.imputed[-inTrain,]

##Create the Model
modelFit<-train(Survived~.,data=final_train, method="rf")
pred_final_validation<-predict(modelFit,final_validation)
table(pred_final_validation,final_validation$Survived)
pred_final_test<-predict(modelFit,test.imputed)

##create file in correct format for submission
test$Survived<-pred_final_test
test<-test[,c(1,12)]
write.csv(test, file="titanic predictions01.csv")

