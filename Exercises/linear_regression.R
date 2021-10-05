# Title     : R Use Case - Linear Regression exercise – ADS_5
# Objective : Predicting miles per gallon (i.e. MPG)
# Created by: Pascal
# Created on: 16-10-20

getwd()
# "C:/Users/User/Workspace/RprogProjects/AppliedDataScience_5"
# auto-miles-per-gallon.csv
auto_data <- read.csv("auto-miles-per-gallon.csv", stringsAsFactors = TRUE)
auto_data <- read.csv("auto-miles-per-gallon.csv", stringsAsFactors = FALSE)
# inspect the data
str(auto_data)
# inspect the quartiles of this data set/frame to be sure data is good/ok
summary(auto_data)
# 398 rows
nrow(auto_data)
# 8 columns : MPG CYLINDERS DISPLACEMENT HORSEPOWER WEIGHT ACCELERATION MODELYEAR NAME
ncol(auto_data)
length(auto_data)
# dimension of this data frame => [1] 398   8
dim(auto_data)
# view head and tail of this data set
head(auto_data)
tail(auto_data)
# data is good/ok => no outliers
# except in column HORSEPOWER where we found some '?' (33,127,331,337,355,375)

# Cleansing
auto_data[auto_data$HORSEPOWER == '?',]
# convert ? to 1 via as.numeric !!!
auto_data$HORSEPOWER <- as.numeric(auto_data$HORSEPOWER)
auto_data[auto_data$HORSEPOWER == 82,]
print(auto_data$HORSEPOWER) # NA values are displayed
for (i in c(33,127,331,337,355,375)) {
  print(auto_data[i,]) # HORSEPOWER is NA
}
# now, we will replace NA value with the mean of all the HP !
hp_mean <- mean(auto_data$HORSEPOWER,na.rm="TRUE")
hp_mean  # i.e. 104.4694 ok - Mean:104.5 computed by summary
auto_data$HORSEPOWER[is.na(auto_data$HORSEPOWER)] <- hp_mean
for (i in c(33,127,331,337,355,375)) {
  print(auto_data[i,])   # HORSEPOWER is now 104.4694
}
summary(auto_data)

# Exploratory Data Analysis
library(ggplot2)
ggplot(auto_data, aes(factor(CYLINDERS),MPG)) +
  geom_boxplot(aes(fill=factor(CYLINDERS)))

# Correlation
library(psych)
pairs.panels(auto_data)
# High correlation between CYLINDERS, WEIGHT and DISPLACEMENT
#... just keep one (WEIGTH)
auto_data$CYLINDERS <- NULL
auto_data$DISPLACEMENT <- NULL
summary(auto_data)
# Scatterplot - visualize any linear relationships between
#  the dependent (response) variable and independent (predictor) variables.
scatter.smooth(x=auto_data$WEIGHT, y=auto_data$HORSEPOWER, main="Weight ~ HP")
scatter.smooth(x=auto_data$WEIGHT, y=auto_data$MGP, main="Weight ~ MGP")
scatter.smooth(x=auto_data$WEIGHT, y=auto_data$MGP, main="Weight ~ MGP")
# Density plot – Check if the response variable is close to normality
library(e1071)
par(mfrow=c(2, 2))  # divide graph area in 2 columns
plot(density(auto_data$WEIGHT), main="Density Plot: Weigth", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(auto_data$WEIGHT), 2)))  # density plot for 'WEIGHT'
polygon(density(auto_data$WEIGHT), col="red")
plot(density(auto_data$MPG), main="Density Plot: MGP", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(auto_data$MPG), 2)))  # density plot for 'MGP'
polygon(density(auto_data$MPG), col="blue")
plot(density(auto_data$HORSEPOWER), main="Density Plot: HORSEPOWER", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(auto_data$HORSEPOWER), 2)))  # density plot for 'HORSEPOWER'
polygon(density(auto_data$HORSEPOWER), col="green")
plot(density(auto_data$ACCELERATION), main="Density Plot: ACCELERATION", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(auto_data$ACCELERATION), 2)))  # density plot for 'ACCELERATION'
polygon(density(auto_data$ACCELERATION), col="orange")

# Modelling & Prediction
head(auto_data[,-6])

# linear model
lm_model <- lm(MPG ~ . ,auto_data[1:5])
summary.lm(lm_model) #, TRUE, TRUE, TRUE)

# prediction based on the model and actual data
predicted <- predict.lm(lm_model, auto_data)
summary(predicted)
summary(auto_data[1])
par(mfrow=c(1, 1))
plot(auto_data$MPG, predicted, col = "red") # , type = "s"

# correlation => [1] 0.8990293
cor(auto_data$MPG, predicted)