library("e1071")

# load data from flat files (both data and labels)
pad_data <- read.csv("pima-indians-diabetes.data", header = FALSE)
names(pad_data) <- c("no_preg","glucose","bloodpress","skinthick","insulin","BMI","dpf","age","hasdiab")

# set the classifier column to a factor - required for naive bayes methods to work 
pad_data$hasdiab <- as.factor(pad_data$hasdiab)

training_set <- pad_data[1:500,] # Take first 500 rows for training - data isn't ordered so this is OK
test_set <- pad_data[501:768,] # Leave remaining 268 for testing

# train the model
nb_model <- naiveBayes(hasdiab~no_preg + glucose + bloodpress + skinthick + insulin + BMI + dpf + age,training_set)

# add a new column for predicted values
test_set$hasdiab_pred <- predict(nb_model,test_set)

# calculated true / false positives / negatives
tp <- nrow(test_set[(test_set$hasdiab == 1 & test_set$hasdiab_pred == 1),])
tn <- nrow(test_set[(test_set$hasdiab == 0 & test_set$hasdiab_pred == 0),])
fp <- nrow(test_set[(test_set$hasdiab == 0 & test_set$hasdiab_pred == 1),])
fn <- nrow(test_set[(test_set$hasdiab == 1 & test_set$hasdiab_pred == 0),])

# calculate total number of samples
tot <- nrow(test_set)

# calculate accuracy
acc <- (tp+tn)/tot

# plot output
bars = c(fn,fp,tn,tp)/tot
barplot(bars,names.arg =c("false negatives","false positives","true negatives","true positives"))
title(paste("Test Set Naive Bayes Classifier Accuracy - ",round(acc,3)*100,"%"))