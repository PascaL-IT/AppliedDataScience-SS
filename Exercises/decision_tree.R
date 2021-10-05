# Title     : R Use Case - Decision Tree exercise – ADS_5
# Objective : Predicting flower types
# Created by: Pascal
# Created on: 21-10-20

# 4 predictors : sepal length & width, petal length & width
# Ref. https://rpubs.com/moeransm/intro-iris
library(datasets)
data(iris)
iris_data <- iris

class(iris)
class(iris_data)

str(iris)
str(iris_data)

summary(iris)
aggregate(x = iris$Species, by = list(specy = iris$Species), FUN = length)

column_names <- tolower(names(iris))
column_names

virginica_flowers <- filter(iris, grepl("virginica", iris$species, fixed = TRUE))
virginica_flowers

# Ref. https://cran.r-project.org/web/packages/C50/vignettes/C5.0.html
plot(iris)

#Ref. https://www.rdocumentation.org/packages/sna/versions/2.5/topics/gplot
library(ggplot2)
ggplot(data = iris_data) +
 geom_point(aes(x = Petal.Length, y = Petal.Width, color = Species, size = Sepal.Length, alpha = Sepal.Width))

#Box plots
par(mfrow=c(2, 2))  # divide graph area in 2 columns
boxplot(data = iris_data, Sepal.Length ~ Species, col = "blue")
boxplot(data = iris_data, Sepal.Width ~ Species, col = "red")
boxplot(data = iris_data, Petal.Length ~ Species, col = "green")
boxplot(data = iris_data, Petal.Width ~ Species, col = "orange")

# Correlation
library(psych)
pairs.panels(iris_data)


# Modelling & Predictions
library(caret)
# creation of a vector with the training data set (70%)
inTrain <- createDataPartition(y=iris_data$Species, p=0.7, list=FALSE)
class(inTrain); str(inTrain)
# training: 105 obs vs. testing : 45 obs
training <- iris_data[inTrain,]
class(training); str(training); dim(training)
table(training$Species)

testing <- iris_data[-inTrain,]
class(testing); str(testing); dim(testing)
table(testing$Species)


# Model building
#Ref. https://cran.r-project.org/web/packages/C50/index.html
library(C50)

# build a model based on training datasert (target is Species)
model <- C5.0(training[-5], training$Species)
summary(model)

# Predicting from the model
predicted <- predict(model, testing)
class(predicted)
str(predicted)
table(predicted)
confusionMatrix(predicted, testing$Species)

# Modelling with a sub-set of data (just Sepal & Species)
sub_data <- iris_data[c(1,2,5)]
str(sub_data)
sub_inTrain <- createDataPartition(y=sub_data$Species, p=0.7, list=FALSE)
sub_training <- sub_data[sub_inTrain,]
sub_testing <- sub_data[-sub_inTrain,]
model2 <- C5.0(sub_training[-3], sub_training$Species)
summary(model2)
sub_predicted <- predict(model2, sub_testing)
confusionMatrix(sub_predicted, sub_testing$Species)