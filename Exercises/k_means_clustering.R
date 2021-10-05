# Title     : R Use Case - K-means clustering exercise – ADS_5
# Objective : Grouping cars into 4 clusters
# Created by: Pascal
# Created on: 05-11-20
getwd()
# "C:/Users/User/Workspace/RprogProjects/AppliedDataScience_5"

# Read input bank file
car_data <- read.csv("auto-data.csv", as.is=FALSE)
str(car_data)
head(car_data)
summary(car_data)
dim(car_data)  # 197  12

# Exploratory Data Analysis
par(mfrow=c(1,5))
boxplot(x = car_data$HP, col = "red", main = "HP")
boxplot(x = car_data$RPM, col = "blue", main = "RPM")
boxplot(x = car_data$MPG.CITY, col = "green", main = "MPG.CITY")
boxplot(x = car_data$MPG.HWY, col = "maroon", main = "MPG.HWY")
boxplot(x = car_data$PRICE, col = "cyan", main = "PRICE")
# OpenIntro -  https://www.openintro.org/
# ref. http://rfunction.com/archives/2575#more-2575
library(openintro)
edaPlot(car_data)


# Modeling & prediction
library(class)        # classification - https://cran.r-project.org/web/packages/class/class.pdf
set.seed(11111)  # to generate the same random (here with ID=11111)
car_subset <- car_data[1:100, c(8,12)]  # 100 rows , 2 dim. { #HP ~ PRICE }
car_subset

car_clusters <- kmeans(car_subset, 4)  # 4 clusters
car_clusters
car_clusters$centers
car_clusters$cluster

# plot - https://www.datamentor.io/r-programming/plot-function/
par(mfrow=c(1,1), cex=1, mai=c(1,1,1,1))
plot(car_subset$HP, car_subset$PRICE,
     col=car_clusters$cluster, pch=20, cex=3,
     xlab="HP", ylab="PRICE", main="Four clusters")
points(car_clusters$centers, col="purple", pch=17, cex=3 )


# Clustering for all the data
# Data transformation (from factors to numeric/ids)
car_data2 <- car_data
for (columns in 1:8) {
  car_data2[,columns] <- as.numeric(car_data[,columns])
}
car_data[1:3,]
car_data2[1:3,]
dim(car_data2)  # 197  12

# to generate the same random (here with ID=11111)
set.seed(11111)
car_clusters2 <- kmeans(car_data2, 4)  # 4 clusters
car_clusters2$centers
car_clusters2$centers[,c(8,12)]
# Center of clusters
car_clusters2$centers
# Cluster assignments on data
car_clusters2$cluster
# Between SS
car_clusters2$betweenss
# No fault (0)
car_clusters2$ifault
# Iteration => 3 (convergence)
car_clusters2$iter
# Size of the 4 clusters
car_clusters2$size
# Total within SS
car_clusters2$tot.withinss
# Total SS
car_clusters2$totss
# Within SS
car_clusters2$withinss

par(mfrow=c(1,1), cex=1, mai=c(1,1,1,1))
plot(car_data2$HP, car_data2$PRICE,
     col=car_clusters2$cluster, pch=20, cex=3,
     xlab="HP", ylab="PRICE", main="Four clusters on all data")
#points - https://www.math.ucla.edu/~anderson/rw1001/library/base/html/points.html
points(car_clusters2$centers[,c(8,12)] , col="blue" , pch=17 , cex=3 )

library(cluster)
clusplot(car_data2, car_clusters2$cluster,
         main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE, labels=2, lines=0)

# Finding optimum number of clusters
# ref. https://rstudio-pubs-static.s3.amazonaws.com/411716_509d28e10fa244019267cd1c59c1fb3a.html
set.seed(11111)
within_ss <- c(); between_ss <- c();
nbr_clusters <- 1:15
for (i in nbr_cluster_range) {
  cluster_i <- kmeans(car_data2, i)
  within_ss[i] <- cluster_i$withinss
  between_ss[i] <- cluster_i$betweenss
}
within_ss; between_ss
plot(x=nbr_clusters,y=within_ss, col="black", type="b", pch=20, cex=3)
points(x=nbr_clusters,y=between_ss, col="red", pch=20, cex=3, type="b")
# => optimun is 3
