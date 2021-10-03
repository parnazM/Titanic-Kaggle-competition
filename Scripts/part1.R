#set direcrtory
setwd('E:/Kaggle competitions/Titanic')

#import dataset
train.model = read.csv(file = 'InputData/train.csv', stringsAsFactors = FALSE, header = TRUE)
test.model = read.csv(file = 'InputData/test.csv', stringsAsFactors = FALSE, header = TRUE)

#combine dataset into total
train.model$IsTrainSet = TRUE
test.model$IsTrainSet = FALSE
test.model$Survived = NA

total = rbind(train.model,test.model)

#clean missing values of Embarked , Age and Fare
table(total$Embarked)
total[total$Embarked=="","Embarked"] = 'S'

table(is.na(total$Age))
total[is.na(total$Age), "Age"] = median(total$Age, na.rm = T)

table(is.na(total$Fare))
total[is.na(total$Fare), "Fare"] = median(total$Fare , na.rm = T)

#categorical casting
total$Pclass = as.factor(total$Pclass)
total$Sex = as.factor(total$Sex)
total$Embarked = as.factor(total$Embarked)

#split dataset backout into train and test
train.model = total[total$IsTrainSet == TRUE,]
test.model = total[total$IsTrainSet == FALSE,]

train.model$Survived = as.factor(train.model$Survived)

#random Forest algorithm
Survived.equation = "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
Survived.formula = as.formula(Survived.equation)

library(randomForest)
titanic.model = randomForest(formula = Survived.formula, data = train.model, ntree =500,
                             mtry = 3, nodesize = 0.01*nrow(test.model))
features.equation = "Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
Survived = predict(titanic.model, newdata = test.model)
PassengerID = test.model$PassengerId

#make a 2-column table (outputdata) for kaggle submission
df = as.data.frame(PassengerID)
df$Survived = Survived

write.csv(df, file = 'outputData/part1.CSV', row.names = F)
