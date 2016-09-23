library("e1071")

# load data from flat files (both data and labels) - you can find these in data > open-data
pad_data <- read.csv("pima-indians-diabetes.data", header = FALSE)
names(pad_data) <- c("no_preg","glucose","bloodpress","skinthick","insulin","BMI","dpf","age","hasdiab")

# set the classifier column to a factor - required for naive bayes methods to work 
pad_data$hasdiab <- as.factor(pad_data$hasdiab)

training_set <- pad_data[1:500,] # Take first 500 rows for training - data isn't ordered so this is OK
test_set <- pad_data[501:768,] # Leave remaining 268 for testing

# train the model
logit_model <- glm(hasdiab~no_preg + glucose + bloodpress + skinthick + insulin + BMI + dpf + age,family=binomial(link="logit"),training_set)

# training set - add a new column for predicted values -----------------------
training_set$hasdiab_pred <- predict(logit_model,training_set)
training_set$hasdiab_pred <- ifelse(training_set$hasdiab_pred > 0.5,1,0) #converts outputs to 0 to 1

# training set - calculated true / false positives / negatives
tp <- nrow(training_set[(training_set$hasdiab == 1 & training_set$hasdiab_pred == 1),])
tn <- nrow(training_set[(training_set$hasdiab == 0 & training_set$hasdiab_pred == 0),])
fp <- nrow(training_set[(training_set$hasdiab == 0 & training_set$hasdiab_pred == 1),])
fn <- nrow(training_set[(training_set$hasdiab == 1 & training_set$hasdiab_pred == 0),])
nrows <- nrow(training_set)

# training set - calculate accuracy
training_acc <- (tp+tn)/nrows

# test set - add a new column for predicted values -------------------
test_set$hasdiab_pred <- predict(logit_model,test_set)
test_set$hasdiab_pred <- ifelse(test_set$hasdiab_pred > 0.5,1,0) #converts outputs to 0 to 1

# test set - calculated true / false positives / negatives
tp <- nrow(test_set[(test_set$hasdiab == 1 & test_set$hasdiab_pred == 1),])
tn <- nrow(test_set[(test_set$hasdiab == 0 & test_set$hasdiab_pred == 0),])
fp <- nrow(test_set[(test_set$hasdiab == 0 & test_set$hasdiab_pred == 1),])
fn <- nrow(test_set[(test_set$hasdiab == 1 & test_set$hasdiab_pred == 0),])
nrows <- nrow(test_set)

# test set - calculate accuracy
test_acc <- (tp+tn)/nrows

# plot output ------------------------------------------------------------
bars = c(fn,fp,tn,tp)/n
barplot(bars,names.arg =c("false negatives","false positives","true negatives","true positives"))
title(paste("Logistic Regression Classifier Accuracy - ",round(test_acc,3)*100,"%"))

# plot coefficients ------------------------------------------------------
coeffs <- logit_model$coefficients[2:9] # Remove intercept
coeffs <- coeffs[order(coeffs,decreasing = TRUE)]
barplot(coeffs)
