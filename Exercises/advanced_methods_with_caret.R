# Title     : R Use Case - Advanced methods with Caret – ADS_5
# Objective : Predicting breast cancer
# Created by: Pascal
# Created on: 08-11-20

# Max Kuhn and Kjell Johnson, Applied Predictive Modeling (book, 2016)
# Ref. http://appliedpredictivemodeling.com/
# Ref. http://topepo.github.io/caret/index.html
getwd()
# "C:/Users/User/Workspace/RprogProjects/AppliedDataScience_5"
cancer_data <- read.csv("breast_cancer.csv")
cancer_data[1:5,]   # breast cancer data frame
dim(cancer_data)    # 569 rows, 32 columns

# Exploratory Data Analysis
str(cancer_data)
head(cancer_data)
summary(cancer_data)

# Correlation to 'diagnosis'
# ref. https://www.rdocumentation.org/packages/psych/versions/2.0.9/topics/pairs.panels
library(psych)
pairs.panels(cancer_data[,c(2,3:10)])  # scatter plot of matrices (SPLOM)
pairs.panels(cancer_data[,c(2,10:20)])
pairs.panels(cancer_data[,c(2,21:32)])

# Principal Components Analysis (PCA)
# ref. https://www.datacamp.com/community/tutorials/pca-analysis-r
#      http://www.sthda.com/french/articles/38-methodes-des-composantes-principales-dans-r-guide-pratique/79-acp-dans-r-prcomp-vs-princomp/
#      https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/prcomp
scale_data <- scale(cancer_data[,3:32])
pca_data <- prcomp(scale_data)
str(pca_data); summary(pca_data)
# plotting this PCA
plot(pca_data, xlab="PCn variables")
# use the first three (3) PC1, PC2 and PC3
set_pca_data <- pca_data$x[,1:3]
head(set_pca_data)
#convert pca to data frame (569 rows, 3 columns)
final_data <- data.frame(set_pca_data)
# add the 'diagnostics' column (569 rows)
final_data$diagnosis <- cancer_data$diagnosis
head(final_data)
# scatter plot of matrices (SPLOM)
pairs.panels(final_data)

# Modelling and predicting
# ref. https://www.rdocumentation.org/packages/caret/versions/6.0-86/topics/createDataPartition
#
library(caret)
inTrain <- createDataPartition(y=final_data$diagnosis, p=0.7, list=FALSE)
training_data<- final_data[inTrain,]
test_data<- final_data[-inTrain,]
dim(training_data); dim(test_data)
table(training_data$diagnosis); table(test_data$diagnosis)
training_data
test_data

# Model building and test
# with 4 algorithms : bagging, boosting, neural networks, support vector machines
algoList <- c("bagFDA","LogitBoost","nnet","svmRadialCost")
results <- data.frame(Algorithm=character(), Duration=numeric(), Accuracy=numeric(), stringsAsFactors = FALSE )

for (i in 1:length(algoList)) {
  algo <- algoList[i]
  print(paste("Alogrithm:", algo))

  startTime <- as.integer(Sys.time())
  # Build model with 'train' function - ref. https://www.rdocumentation.org/packages/caret/versions/4.47/topics/train
  model <- train( diagnosis ~ . , data = training_data , method = algo)
  model

  # Predict against test data
  predicted <- predict(model, test_data)
  length(predicted); length(test_data$diagnosis)

  # Compare diagnosis prediction with test data
  comp <- confusionMatrix(predicted, as.factor(test_data$diagnosis))

  # Store the result
  stopTime <- as.integer(Sys.time())
  result <- c( as.character(algo) , stopTime - startTime , as.numeric(comp$overall[1]) )
  print(result)
  results[i,1] <- as.character(algo)
  results[i,2] <- ( stopTime - startTime )
  results[i,3] <- round(as.numeric(comp$overall[1]) * 100, 2)
}

#Print comparison results
results
