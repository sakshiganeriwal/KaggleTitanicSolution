#set working directory and import data set
setwd("C:/Users/sakshi/Desktop/titanic")
train <- read.csv("C:/Users/sakshi/Desktop/titanic/train.csv")
View(train)
test <- read.csv("C:/Users/sakshi/Desktop/titanic/test.csv")
View(test)
str(train)#structure of training set
# if the dataset has a lot of text that we know we will want to work with, we could have imported the file with

train <- read.csv("train.csv", stringsAsFactors=FALSE)
# isolate a single column of the dataframe
train$Survived
#table runs through the vector you gave it and simply counts the occurrence of each value in it
table(train$Survived)
#proportion
prop.table(table(train$Survived))
#‘rep’ that simply repeats something by the number of times 
test$Survived <- rep(0,418)
#submit1 
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "theyallperish.csv", row.names = FALSE)
#summary of Sex
summary(train$Sex)
#expand the proportion table command
prop.table(table(train$Sex,train$Survived))
# row-wise proportion
#proportions in the 1st dimension which stands for the rows (using ‘2’ instead would give you column proportions
prop.table(table(train$Sex,train$Survived),1)
#change submission
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
#submit 2
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "submit2.csv", row.names = FALSE)
summary(train$Age)
#on the basis of age divide as below 18 or above
train$Child <- 0
train$Child[train$Age < 18] <-1
#table with both gender and age to see the survival proportions for different subsets
#number of survivors for the different subsets
aggregate(Survived ~ Child + Sex,data=train, FUN=sum)
#total no. of ppl
aggregate(Survived ~ Child + Sex, data=train, FUN=length)
aggregate(Survived ~ Child + Sex, data=train, FUN=function(x){sum(x)/length(x)})
#class and fare taken into consideration
train$Fare2 <- '30+'
train$Fare2[train$Fare <30 & train$Fare >=20] <- '20-30'
train$Fare2[train$Fare <20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'
aggregate(Survived ~ Fare2 + Pclass + Sex,data=train, FUN=function(x){sum(x)/length(x)} )
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
test$Survived[test$Sex == 'female' &test$Pclass == 3 & test$Fare >= 20] <- 0
#submit3
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "submit3.csv", row.names = FALSE)
#machine learning to build decision trees(glass-box model) to do the heavy lifting for us.
#rpart for 'Recursive Partitioning and Regression Trees' 
#import rpart
library(rpart)
#rpart same as aggregate ,equation, headed up by the variable of interest and followed by the variables used for prediction. 
# to predict a continuous variable, such as age, you may use method="anova"
#since here a one or a zero, so method="class" is appropriate
fit <- rpart(Survived ~ Pclass + Sex + Age +SibSp + Parch + Fare + Embarked, data=train,method="class")
plot(fit)
text(fit)
install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(fit)
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
#submit4
write.csv(submit,file ="myfirstdtree.csv",row.names = FALSE)
#part package automatically caps the depth that the tree grows by using a metric called complexity which stops the resulting model from getting too out of hand
#default limits by typing ?rpart.control. The first one we want to unleash is the cp parameter, this is the metric that stops splits that aren't deemed important enough. The other one we want to open up is minsplit which governs how many passengers must sit in a bucket before even looking for a split. Let's max both out and reduce cp to zero and minsplit to 2 
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train,
             method="class", control=rpart.control(minsplit=2, cp=0))
fancyRpartPlot(fit)
#submit5
write.csv(submit,file ="uncontrolledDT.csv",row.names = FALSE)
#manually trim tree
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train,
             method="class", control=rpart.control( minsplit=8, cp=0 ))
new.fit <- prp(fit,snip=TRUE)$obj
fancyRpartPlot(new.fit)
#Feature Engineering
train$Name[1]
test$Survived <- NA
combi<- rbind(train,test)
combi$Name <- as.character(combi$Name)
combi$Name[1]
strsplit(combi$Name[1],split='[,.]')[[1]]
strsplit(combi$Name[1], split='[,.]')[[1]][2]
# apply this transformation to every row of the combined train/test dataframe
combi$Title <- sapply(combi$Name, FUN=function(x){strsplit(x,split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '',combi$Title)
table(combi$Title)
# %in% operator checks to see if a value is part of the vector we're comparing it to
combi$Title[combi$Title %in% c('Mme','Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
#decision tree biased to favour factors with many levels.
combi$Title <- factor(combi$Title)
combi$FamilySize <- combi$SibSp + combi$Parch + 1
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
table(combi$FamilyID)
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)
train <- combi[1:891,]
test <- combi[892:1309,]
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,data=train, method="class")
fancyRpartPlot(fit)
#submit6
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit,file ="FeatureEngineering.csv",row.names = FALSE)
# ensemble models work, they grow a lot of different models, and let their outcomes be averaged or voted across the group
#Bagging takes a randomized sample of the rows in your training set, with replacement
sample(1:10, replace = TRUE)
summary(combi$Age)
# method="anova" version of our decision tree, as we are not trying to predict a category any more, but a continuous variable
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=combi[!is.na(combi$Age),], method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])
summary(combi)
# reduce the number of trees, at least for initial exploration, or restrict the complexity of each tree using nodesize as well as reduce the number of rows sampled with sampsize. You can also override the default number of variables to choose from with mtry, but the default is the square root of the total number available and that should work just fine. 
which(combi$Embarked == '')
combi$Embarked[c(62,830)] = "S"
combi$Embarked <- factor(combi$Embarked)
summary(combi$Fare)
which(is.na(combi$Fare))
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)
combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)
install.packages('randomForest')
library(randomForest)
set.seed(415)
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize +
                      FamilyID2, data=train, importance=TRUE, ntree=2000)
varImpPlot(fit)
Prediction <- predict(fit, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "firstforest.csv", row.names = FALSE)
# forest of conditional inference trees. They make their decisions in slightly different ways, using a statistical test rather than a purity measure
install.packages('party')
library(party)
set.seed(415)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
               data = train, controls=cforest_unbiased(ntree=2000, mtry=3))
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "secondforest.csv", row.names = FALSE)
