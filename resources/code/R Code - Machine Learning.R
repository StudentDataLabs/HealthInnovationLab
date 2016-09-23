# Machine Learning

# load data from flat files (both data and labels) - you can find these in data > open-data
pad_data <- read.csv("pima-indians-diabetes.data", header = FALSE)
names(pad_data) <- c("no_preg","glucose","bloodpress","skinthick","insulin","BMI","dpf","age","hasdiab")

install.packages("e1071")
library(e1071)

# set the classifier column to a factor - required for naive bayes methods to work 
pad_data$hasdiab <- as.factor(pad_data$hasdiab)

install.packages("caret")
library(caret)

inTrain <- createDataPartition(pad_data$hasdiab, p = 0.7, list = F)
training_set <- pad_data[inTrain, ]

excluded_set <- pad_data[-inTrain, ]
inTest <- createDataPartition(excluded_set$hasdiab, p = 0.5, list = F)
test_set <- excluded_set[inTest, ] 
cv_set <- excluded_set[-inTest, ]

install.packages("ggplot2")
library(ggplot2)

g1 <- ggplot(data = training_set, aes(x = hasdiab, y = glucose, fill = hasdiab))
g1 + geom_boxplot()
with(training_set, t.test(glucose[hasdiab == 1], glucose[hasdiab == 0]))

logit_model <- glm(hasdiab ~ age,family=binomial(link="logit"),training_set)
test_set$hasdiab_pred <- predict(logit_model, test_set)
test_set$hasdiab_pred <- ifelse(test_set$hasdiab_pred > 0.5,1,0)
confusionMatrix(test_set$hasdiab_pred, test_set$hasdiab)

install.packages("rpart")
library(rpart)

ct <- rpart(hasdiab ~ ., data = training_set)

install.packages("rattle")
library(rattle)

install.packages("rpart.plot")
library(rpart.plot)

fancyRpartPlot(ct)


install.packages("randomForest")
library(randomForest)

rf <- randomForest(hasdiab ~ ., data = training_set)
