# load libraries of interest\
library(randomForest)
library(rpart.plot)

# The train and test data is stored in the ../input directory
train <- read.csv("../input/train.csv");
test  <- read.csv("../input/test.csv");

# prep and merge the two datasets, for convenience
test$Survived <- NA
combo <- rbind(train, test)

combo$Name <- as.character(combo$Name)

combo$Title <- sapply(combo$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combo$Title <- sub(' ', '', combo$Title)

combo$Title[combo$Title %in% c('Mme', 'Mlle')] <- 'Mlle'

combo$Title[combo$Title %in% c('Capt', 'Don', 'Major', 'Sir', 'Jonkheer')] <- 'Sir'
combo$Title[combo$Title %in% c('Dona', 'Lady', 'the Countess')] <- 'Lady'

combo$Title <- factor(combo$Title)

#table(combo$Title)

combo$FamilySize <- combo$SibSp + combo$Parch + 1

train <- combo[1:891,]
test <- combo[892:1309,]

colnames(train)

fit <- rpart(Survived ~ Pclass + Sex + Age + Fare + Embarked + Cabin + Title + FamilySize, data=train, method="class")

prediction <- predict(fit, test, type="class")
submission <- data.frame(PassengerId = test$PassengerId, Survived = prediction)
write.csv(submission, file="titanic_solution.csv", row.names = FALSE)