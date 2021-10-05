# Title     : R Use Case - Random Forest exercise – ADS_5
# Objective : Predicting customers of a bank
# Created by: Pascal
# Created on: 04-11-20
getwd()
# "C:/Users/User/Workspace/RprogProjects/AppliedDataScience_5"

# Read input bank file
# ref. https://informatique-mia.inrae.fr/r4ciam/sites/default/files/download/tutoriels/readTable.pdf
bank_data <- read.csv2("bank.csv", header = TRUE, as.is = FALSE)  # i.e. sep = ";" + convert to factors
str(bank_data)
summary(bank_data)
head(bank_data)
head(bank_data[,c(8:16,17)])
# no cleansing required

# Correlation between predictors?
library(psych)
pairs.panels(bank_data[,c(1:8,17)])
pairs.panels(bank_data[,c(9:16,17)])

# Keep predictors with coreff. above 0.1 (> 10%)
newbank_data <- bank_data[c(1:4,7:9,12,14,15,17)]
pairs.panels(newbank_data)

# Do som etransforamtions on the data
# ref. cut - https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/cut
newbank_data$age <- cut(newbank_data$age, c(1,20,40,60,100))

newbank_data$is_divorced <- ifelse(newbank_data$marital == "divorced" , 1, 0)
newbank_data$is_single   <- ifelse(newbank_data$marital == "single" , 1, 0)
newbank_data$is_married  <- ifelse(newbank_data$marital == "married", 1, 0)
newbank_data$marital     <- NULL  # to delete
str(newbank_data)

# Exploratory Data Analysis
# par - ref. https://www.rdocumentation.org/packages/graphics/versions/3.6.2/topics/par
par(mfrow=c(3,2), las=0)
plot(newbank_data$housing, newbank_data$y, xlab="Housing" , ylab="Become cusotmer?" , col=c("darkgreen","red"))
plot(newbank_data$contact, newbank_data$y, xlab="Contact type" , ylab="Become customer?" , col=c("darkgreen","red"))
plot(newbank_data$education, newbank_data$y, xlab="Education type" , ylab="Become customer?" , col=c("darkgreen","red"))
plot(newbank_data$age, newbank_data$y, xlab="Age" , ylab="Become customer?" , col=c("darkgreen","red"))
boxplot(duration ~ y, data = newbank_data, col="orange" )
boxplot(pdays ~ y, data = newbank_data, col="blue" )

par(mfrow=c(1,1), las=0)
plot(newbank_data$job, newbank_data$y, xlab="Job" , ylab="Become customer?" , col=c("darkgreen","red"))
newbank_data$duration_bins <- cut(newbank_data$duration, c(1,20,50,100,300,500,1000,3000,5000))
plot(newbank_data$duration_bins, newbank_data$y, xlab="Duration" , ylab="Become customer?" , col=c("darkgreen","red"))


# Model building
library(caret)
inTrain <- createDataPartition(y=newbank_data$y, p=0.7, list=FALSE)
training_data <- newbank_data[inTrain,]
testing_data <- newbank_data[-inTrain,]
dim(training_data); dim(testing_data)
table(training_data$y);table(testing_data$y);

# Random forest
# ref. https://www.rdocumentation.org/packages/randomForest/versions/4.6-14/topics/randomForest
#  see example with 'iris' data set
library(randomForest)
model <- randomForest( y ~ . , data=training_data)
print(model)
str(model)
# ref. https://www.rdocumentation.org/packages/randomForest/versions/4.6-14/topics/importance
# Extract Variable Importance Measure - extractor function for variable importance measures as
# produced by randomForest.
importance(model)
round(importance(model),2)
# testing and predict
predicted <- predict(model, testing_data)
str(predicted)
table(predicted)
cm <- confusionMatrix(predicted, testing_data$y)
print(cm)
cm$byClass
cm$overall
cm$positive
cm$table

# test accurary with differnet number of tree(s)
# seq - href. ttps://www.rdocumentation.org/packages/base/versions/3.6.2/topics/seq
accuracies=c() # vector - https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/c
for (i in seq(500)) {
  modelForest <- randomForest( y ~ . , data=training_data, ntree=i)
  predictForest <- predict(modelForest, testing_data) # , type="class")
  accuracies <- c(accuracies, confusionMatrix(predictForest, testing_data$y)$overall[1] )
}
table(accuracies)
object.size(accuracies)
print(accuracies)

par(mfrow=c(1,1), las=0)
plot(x=1:500, y=accuracies, main="Effect of increasing tree size", xlab="Nbr of Tress" , ylab="Accuracy" , col=c("black"), type="l")