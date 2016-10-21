# load decision tree package
library(rpart.plot)

# train and test data is stored in the ../input directory
train <- read.csv("../input/train.csv");
test  <- read.csv("../input/test.csv");

# prep and merge the two datasets, for convenience
test$Survived <- NA
combo <- rbind(train, test)

## enginneer new feature for passengers' titles
# process strings
combo$Name <- as.character(combo$Name)
combo$Title <- sapply(combo$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combo$Title <- sub(' ', '', combo$Title)

# combine titles into categories
combo$Title[combo$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combo$Title[combo$Title %in% c('Capt', 'Don', 'Major', 'Sir', 'Jonkheer')] <- 'Sir'
combo$Title[combo$Title %in% c('Dona', 'Lady', 'the Countess')] <- 'Lady'

# recode titles as factors
combo$Title <- factor(combo$Title)

## engineer new feature for family size
# family size = individual + siblings + parents/children
combo$FamilySize <- combo$SibSp + combo$Parch + 1

# split merged file back into test and train sets
train <- combo[1:891,]
test <- combo[892:1309,]

# let's use a simple decision tree for our model
fit <- rpart(Survived ~ Pclass + Sex + Age + Fare + Embarked + Cabin + Title + FamilySize, data=train, method="class")
prediction <- predict(fit, test, type="class")

# format and output file for kaggle submission
submission <- data.frame(PassengerId = test$PassengerId, Survived = prediction)
write.csv(submission, file="titanic_solution.csv", row.names = FALSE)
