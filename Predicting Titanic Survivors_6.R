##Sixth try; adding additinal engineered features in an attempt to beat Trevor Stephen's benchmark

# Set working directory and import datafiles
test <- read.csv("~/Desktop/Kaggle/Predicting Titanic Survivors/data/test.csv")
train <- read.csv("~/Desktop/Kaggle/Predicting Titanic Survivors/data/train.csv")

#exploratory data analysis
library("vcd", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")
mosaicplot(train$Pclass ~ train$Survived, main="Passenger Fate by Traveling Class", shade=FALSE, color=TRUE, xlab="Pclass", ylab="Survived")
mosaicplot(train$Sex ~ train$Survived, main="Passenger Fate by Gender", shade=FALSE, color=TRUE, xlab="Sex", ylab="Survived")
boxplot(train$Age ~ train$Survived, main="Passenger Fate by Age", xlab="Survived", ylab="Age")
mosaicplot(train$Embarked ~ train$Survived,main="Passenger Fate by Port of Embarkation",shade=FALSE, color=TRUE, xlab="Embarked", ylab="Survived")

#Combine data sets for manipulation
test$Survived <- NA
combi <- rbind(train, test)

#Feature engineering--same as previous submissions
combi$Name <- as.character(combi$Name)
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir','Jonkheer')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess')] <- 'Lady'
combi$Title <- factor(combi$Title)
combi$FamilySize <- combi$SibSp + combi$Parch + 1
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)

#Feature engineering--new for this submission
require(plyr)
require(stringr)
isDigit <- function(x) x %in% c("0","1","2","3","4","5","6","7","8","9") 
combi$Ticket <- as.character(combi$Ticket)
combi$ticketprefix <- sapply(combi$Ticket, FUN=function(x) {strsplit(x, split=' ')[[1]][1]})
combi$ticketprefix.first.digit<-str_sub(combi$ticketprefix, start=1, end=1)
combi$ticketprefix[which(isDigit(combi$ticketprefix.first.digit))] <- "none"
combi$ticketprefix[combi$ticketprefix %in% c('A./5.', 'A.5', 'A.5.', 'A/5', 'A/5.', 'A/S')] <- 'A/5'
combi$ticketprefix[combi$ticketprefix %in% c('A/4', 'A/4.', 'A4.')] <- 'A/4'
combi$ticketprefix[combi$ticketprefix %in% c('C.A', 'CA', 'CA.','C.A.')] <- 'CA'
combi$ticketprefix[combi$ticketprefix %in% c('W./C.', 'W/C')] <- 'W/C'
combi$ticketprefix[combi$ticketprefix %in% c('W.E.P', 'W.E.P.', 'WE/P')] <- 'W.E.P'
combi$ticketprefix[combi$ticketprefix %in% c('C.A./SOTON', 'SOTON/O.Q.', 'SOTON/O2','SOTON/OQ','STON/O','STON/OQ.','STON/O2.')] <- 'SOTON'
combi$ticketprefix[combi$ticketprefix %in% c('SC/Paris', 'SC/PARIS', 'S.C./PARIS')] <- 'SC/PARIS'
combi$ticketprefix<-as.factor(combi$ticketprefix)
combi$ticketprefix.first.digit <- NULL

combi$Deck <- substring(combi$Cabin, 1, 1)
combi$Deck<-as.character(combi$Deck)
combi$Deck[ which( combi$Deck =="")] <- "UNK"
combi$Deck <- as.factor(combi$Deck)

isEven <- function(x) x %in% c("0","2","4","6","8") 
isOdd <- function(x) x %in% c("1","3","5","7","9") 
combi$Cabin<-as.character(combi$Cabin)
combi$cabin.last.digit <- str_sub(combi$Cabin, -1)
combi$Side <- "UNK"
combi$Side[which(isEven(combi$cabin.last.digit))] <- "port"
combi$Side[which(isOdd(combi$cabin.last.digit))] <- "starboard"
combi$Side <- as.factor(combi$Side)
combi$cabin.last.digit <- NULL

#extrapolate deck and cabin number to all members of a group
pmax<-length(combi$PassengerId)
for (i in 1:pmax){
        v<-which(combi$Ticket %in% combi$Ticket[i])
        vmax<-length(v)
        for (j in 1:vmax) {
                if (!(combi$Deck[v[j]]=="UNK")) {combi$Deck[i]<-combi$Deck[v[j]]}  
                }
                        }

for (i in 1:pmax){
        v<-which(combi$Ticket %in% combi$Ticket[i])
        vmax<-length(v)
        for (j in 1:vmax) {
                if (!(combi$Side[v[j]]=="UNK")) {combi$Side[i]<-combi$Side[v[j]]}  
        }
}

for (i in 1:pmax){
        v<-which(combi$Ticket %in% combi$Ticket[i])
        vmax<-length(v)
        for (j in 1:vmax) {
                if (combi$FamilyID[i]=="small" && !(combi$FamilyID[v[j]]=="small"))         
                        {combi$FamilyID[i]<-combi$FamilyID[v[j]]}
                          }
                }

#Fill in missing datapoints
library("rpart", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize, data=combi[!is.na(combi$Age),], method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])
which(combi$Embarked == '')
combi$Embarked[c(62,830)] = "S"
combi$Embarked <- factor(combi$Embarked)

#Split data sets back out
train <- combi[1:891,]
test <- combi[892:1309,]

#Conditional Forest
install.packages('party')
library(party)
set.seed(415)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + Embarked + Title + FamilySize + FamilyID + ticketprefix + Deck + Side, data = train, controls=cforest_unbiased(ntree=2000, mtry=3))
Prediction <- predict(fit, test, OOB=TRUE, type = "response")

#Create file for submission
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "cforest_enhanced.csv", row.names = FALSE)
