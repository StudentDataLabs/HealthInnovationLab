# Install the necessary packages
install.packages("ggplot2")
library(ggplot2)

# Read in the UCI Pima Indians dataset
pima <- read.csv("UCI Diabetes Pima Indians.csv", header = TRUE, stringsAsFactors = FALSE)

# Inspect the first few rows
head(pima)

# Visualise with a scatter plot by age and BMI
ggplot(pima, aes(x=Age, y=BMI)) + geom_point()

# Create your first model exploring the relationship between class and BMI
pima_1 = lm(Class ~ BMI, data=pima)
print(summary(pima_1))

# Create your second model exploring class and age
pima_2 = lm(Class ~ Age, data=pima)
print(summary(pima_2))

# Create your third model
pima_3 = lm(Class ~ Age + Pregnant + Plasma.Glucose + Blood.Pressure + BMI + Tricep.Skin.Thickness + Diabetes.Pedigree + Serum.Insulin, data=pima)
print(summary(pima_3))

# Find out how the models performed
AIC_model_1 = step(pima_1, direction='both')
summary(AIC_model_1)

AIC_model_2 = step(pima_2, direction='both')
summary(AIC_model_2)

AIC_model_3 = step(pima_3, direction='both')
summary(AIC_model_3)

# Create a subset for those diagnosed with Diabetes
subset <- subset(pima, Class == 1)

# Create a couple of scatter plots
ggplot(subset, aes(x=Age, y=BMI)) + geom_point() + geom_smooth()

ggplot(subset, aes(x=BMI, y=Plasma.Glucose)) + geom_point() + geom_smooth()
