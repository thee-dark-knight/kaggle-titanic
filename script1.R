#Setting directory
setwd("D:/Learning/Kaggle/Titanic")
#Importing datasets
train <- read.csv("D:/Learning/Kaggle/Titanic/train.csv")
test <- read.csv("D:/Learning/Kaggle/Titanic/test.csv")


#1st attempt at decision tree
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class")

#better readable decision tree
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(fit)

#Prediction
Prediction <- predict(fit, test, type = "class")

#Making Submission File
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)

#write the csv firstdtree
write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)






#Pruning not much effective..see the ?rpart.control for more
#2nd attempt at decision tree
fit2 <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class")
pruned_fit2 <- prune(fit2,cp=.03,minsplit=40,minbucket=10)

#Prediction
Prediction2 <- predict(pruned_fit2, test, type = "class")

#Making Submission File
submit2 <- data.frame(PassengerId = test$PassengerId, Survived = Prediction2)

#write the csv firstdtree
write.csv(submit2, file = "myseconddtree.csv", row.names = FALSE)








#3rd attempt at decision tree
new.fit <- prp(fit,snip=TRUE)$obj
fancyRpartPlot(new.fit)

#Prediction
Prediction3 <- predict(new.fit, test, type = "class")

#Making Submission File
submit3 <- data.frame(PassengerId = test$PassengerId, Survived = Prediction3)

#write the csv firstdtree
write.csv(submit3, file = "mythirddtree.csv", row.names = FALSE)



#4th attempt at decision tree..doing real machine learning..feature engineering
#in order to create new variable..u need to do it for both the test and train datasets

test$Survived<-NA

#FEATURE ENGINEERING..MAKING NEW COLUMNS
comb<-rbind(train,test)
comb$Name<-as.character(comb$Name)

#sapply used as simple lapply..on a dataframe/list a specified function

comb$Title <- sapply (comb$Name, FUN=function(x){strsplit(x, split='[,.]')[[1]][2] })

#To strip spaces
#sub and gsub perform replacement of the first and all matches respectively.
#sub(pattern,replacement,x)

comb$Title <- sub(' ', '', comb$Title)


#The %in% operator checks to see if a value is part of the vector we're comparing it to

comb$Title[comb$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
comb$Title[comb$Title %in% c('Capt', 'Don', 'Major', 'Sir','Rev', 'Jonkheer')] <- 'Sir'
comb$Title[comb$Title %in% c('Dona', 'Lady', 'the Countess')] <- 'Lady'
comb$Title[comb$Title %in% c('Miss', 'Ms')] <- 'Miss'

#Making factor of all available levels..ignoring levels with no nodes
#1st feature
comb$Title <- factor(comb$Title)

#2nd feature
comb$FamilySize <- comb$SibSp + comb$Parch + 1

#3rd feature
comb$Surname <- sapply(comb$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})


comb$FamilyID <- paste(as.character(comb$FamilySize), comb$Surname, sep="")

comb$FamilyID<- 'Large'
comb$FamilyID[comb$FamilySize<=2]<- 'Small'
comb$FamilyID[comb$FamilySize>=3 & comb$FamilySize<=5]<- 'Medium'
comb$FamilyID[comb$FamilySize>=8]<- 'Very Large'


#Now separation and modelling
train4<-comb[1:891,]
test4<-comb[892:1309,]


#Decision tree 4
fit4 <- rpart(Survived ~ Pclass + Sex + Age + Fare + Embarked + Title + FamilySize + FamilyID,
             data=train4, method="class")

#Prediction4
Prediction4 <- predict(fit4, test4, type = "class")

#Making Submission File
submit4 <- data.frame(PassengerId = test$PassengerId, Survived = Prediction4)

#write the csv firstdtree
write.csv(submit4, file = "myfourthdtree.csv", row.names = FALSE)






#Using Random Forest Directly, remove the NA objects
library(randomForest)
set.seed(100)

#Age Fit using method=anova
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
               data=comb[!is.na(comb$Age),], method="anova")
comb$Age[is.na(comb$Age)] <- predict(Agefit, comb[is.na(comb$Age),])


#which(combi$Embarked == '') to find blanks... which to find things

comb$Embarked[c(62,830)] = "S"
comb$Embarked <- factor(comb$Embarked)
comb$Fare[1044] <- median(comb$Fare, na.rm=TRUE)

train5<-comb[1:891,]
test5<-comb[892:1309,]

#In addition: Warning message:
#In data.matrix(x) : NAs introduced by coercion
#This error if we try to use char columns in random Forest..all should be factors

train5$FamilyID <- as.factor(train5$FamilyID)

fit5 <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + Fare + Embarked + Title + FamilySize + FamilyID,data=train5,importance=TRUE,ntrees=2000)

# check importance by varImpPlot(fit)

#meaning of mean decrease gini ?? 
library(party)
set.seed(100)

#controls=cforest_unbiased(ntree=2000, mtry=3) important rest argument for random forest
fit6 <- cforest(as.factor(Survived) ~ Pclass + Sex + SibSp + Parch + Age + Fare + Embarked + Title + FamilySize + FamilyID,
               data = train5, controls=cforest_unbiased(ntree=2000, mtry=3))

#new prediction :p




test5$Survived <- as.factor(test5$Survived)
test5$FamilyID <- as.factor(test5$FamilyID)
Prediction6 <- predict(fit6, test5, OOB=TRUE, type = "response")








#Making Submission File
submit6 <- data.frame(PassengerId = test$PassengerId, Survived = Prediction6)

#write the csv firstdtree
write.csv(submit6, file = "myrandomForest.csv", row.names = FALSE)



