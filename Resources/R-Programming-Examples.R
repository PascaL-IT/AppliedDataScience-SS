#
#-----------------------------------------------------------------------------
#
#                         R Programming Examples
#
#                         Copyright : V2 Maestros @2015
#
#-----------------------------------------------------------------------------

# This file contains sample code for demonstrating the capabilities of R. Any
# data files required for execution of this code is present with the package.
# Please place all the files in the same folder and set that folder as the current
# working directory.

# It is expected that you have prior programming experience with another language
# Basics of programming and language contructs are not explained in this
# course.

#------------------------------------------------------------------------------
#                         Set working Directory
#------------------------------------------------------------------------------
setwd("C:/Personal/V2Maestros/Modules/R Programming")

#------------------------------------------------------------------------------
#                         Variables
#------------------------------------------------------------------------------

#a Character/String variable
aCharVar <- "Programming"
aCharVar

#a Numeric variable
aNumVar <- 1992.12
aNumVar

#Using Scientific Notation
aNumVar <- 2.3e-4
aNumVar

#Boolean variable
aBoolVar <- TRUE
aBoolVar

#Changing data types
as.character(aNumVar)
as.numeric(aBoolVar)

#Invalid conversion results in NA
as.numeric(aCharVar)

# See all variables in the workspace
ls()

#------------------------------------------------------------------------------
#                         Arithmetic
#------------------------------------------------------------------------------

3 + 5

var1 <- 5
var2 <- 3
var3 <- var1 + var2

#------------------------------------------------------------------------------
#                         Strings
#------------------------------------------------------------------------------

string2 <- c("I am saying ", "hello")
string2

paste("I can also use", "paste")

paste0("To concat", "without spaces")

cat('testing \t tabs')


#------------------------------------------------------------------------------
#                         Date and Time
#------------------------------------------------------------------------------

#POSIX date time
Sys.time()
unclass(Sys.time())

as.Date('2015-01-16')
as.Date('1/16/2015',format='%m/%d/%Y')

#------------------------------------------------------------------------------
#                         Vectors
#------------------------------------------------------------------------------

myVector <- c(1,2,3)
myVector
class(myVector)

myVector <- c(1:100)
myVector

myVector[4]
myVector[5:8]
myVector[-3]

myVector > 40

length(myVector)
sum(myVector)

vec1 <- c(3,4,5)
vec2 <- c(10,11,12)
vec1+vec2

vec3 <- c(1)
vec1+vec3

vegetables <- c(5,7,3)
names(vegetables) <- c("carrot","beans","broccoli")
vegetables
vegetables["beans"]

#------------------------------------------------------------------------------
#                         Lists
#------------------------------------------------------------------------------

employee <- list(1001,"Joe Smith", FALSE)
employee
employee[2]
employee <- list(id=1001, name="Joe Smith", manager=FALSE)
employee["id"]
employee$id

#------------------------------------------------------------------------------
#                         Data Frames
#------------------------------------------------------------------------------

#Create a data frame from Vectors
empId <- c(1,2,3)
empName <- c("John", "Mike", "Randy")
isManager <- c(FALSE, TRUE, FALSE )
salary <- c(1242.11, 3490.20, 2201.87)

emp_df <- data.frame(empId, 
                     empName,
                     isManager, 
                     salary, 
                     stringsAsFactors = FALSE)
emp_df

class(emp_df)
nrow(emp_df)
ncol(emp_df)
str(emp_df)
summary(emp_df)
names(emp_df)
head(emp_df)

emp_df[1,3]
emp_df[1:2,1:3]
emp_df$salary
emp_df$empName[2]

sum(emp_df$salary)

emp_df$salary > 2000
emp_df [ emp_df$salary > 2000, 2]

emp_df$dept_id <- c(1,1,2)
emp_df

#builtin data frame
iris

df1 <- data.frame(
  x = 1:3,
  y = c("a", "b", "c"),
  stringsAsFactors = FALSE)

df2 <- data.frame(
  x = 7:9,
  y = c("f", "g", "h"),
  stringsAsFactors = FALSE)

df3 <- data.frame(
  x2 = 7:9,
  y2 = c("f", "g", "h"),
  stringsAsFactors = FALSE)

df1
df2
df3

rbind( df1, df2)

cbind(df1, df3)

#------------------------------------------------------------------------------
#                         Matrices
#------------------------------------------------------------------------------
demo_matrix <- matrix(c(2,4,6,3,5,8),
                      nrow=3,ncol=2)
demo_matrix

t(demo_matrix)

#------------------------------------------------------------------------------
#                         Factors
#------------------------------------------------------------------------------

vFactor <- as.factor("Apple")
vFactor

as.factor(emp_df$empName)
table(as.factor(emp_df$isManager))

#------------------------------------------------------------------------------
#                         Sorting
#------------------------------------------------------------------------------

aVector <- c (6,3,4,11,2,9,5)
sort(aVector)

emp_df
emp_df[ order(salary),]
#------------------------------------------------------------------------------
#                         Merging
#------------------------------------------------------------------------------

dept_df <- data.frame(
  dept_id = c(1,2),
  dept_name = c("Sales","Operations"),
  stringsAsFactors = FALSE)
dept_df

merge(emp_df, dept_df)

#------------------------------------------------------------------------------
#                         Binning
#------------------------------------------------------------------------------

emp_df$sal_range <- cut( emp_df$salary, 
                          c(1,2000.0,5000.0 ))
emp_df

aggregate(emp_df$salary, 
          by=list(emp_df$sal_range), FUN=sum)


#------------------------------------------------------------------------------
#                         Input/Output Operations
#------------------------------------------------------------------------------

read_data <- scan()
read_data
print(read_data)
print( c("we read ", read_data))

dir()
list.files()

# File Operations
emp_df <- read.csv("employee.csv")
emp_df
emp_df$sal_range <- cut( emp_df$salary, 
                         c(1,2000.0,5000.0 ))
emp_df

write.csv(emp_df,"employee_added.csv")


#------------------------------------------------------------------------------
#                         Control Structures
#------------------------------------------------------------------------------

#Loop through every row in the data frame
for (i in 1:nrow(iris)) {
  
  if ( iris[i,"Species"] == "setosa"){
    print("This is setosa")
  }
  else {
    print("This is NOT setosa")
  }
}


#------------------------------------------------------------------------------
#                         Functions
#------------------------------------------------------------------------------

computeSum <- function(x,y) {
  print( paste("Received ", x, y))
  x+y
}
computeSum(4,6)

#------------------------------------------------------------------------------
#                         Packages
#------------------------------------------------------------------------------

install.packages("RCurl")
library(RCurl)

#------------------------------------------------------------------------------
#                         Apply Functions
#------------------------------------------------------------------------------
mat <- matrix(c(1:20), nrow = 10, ncol = 2)
apply(mat, 1, mean)
apply(mat, 2, mean)
#------------------------------------------------------------------------------
#                         Statistics in R
#------------------------------------------------------------------------------

iris_df <- iris
summary(iris_df)

mean(iris_df$Sepal.Length)
range(iris_df$Petal.Width)

cor(iris_df[,1:4])

library(psych)
pairs.panels(iris_df)

iris_df$Species <- as.numeric(iris_df$Species)
iris_model <- lm(Species ~ . , iris_df)
summary(iris_model)

#------------------------------------------------------------------------------
#                         Base Plotting System
#------------------------------------------------------------------------------

par(mfrow=c(1,1))
stripchart(iris$Sepal.Width)

hist(iris$Sepal.Width)
hist(iris$Sepal.Width, col="cyan",
     main="Sepal Width", xlab="Dist of width", ylab="Sample Count")

boxplot(iris$Sepal.Width)
boxplot(iris$Sepal.Width ~ iris$Species, col="green")

plot(iris$Sepal.Length, iris$Sepal.Width)
plot(iris$Sepal.Length, iris$Sepal.Width, col=iris$Species)

plot(iris$Sepal.Length,  type="l")
plot(sort(iris$Sepal.Length), type="l")

irisagg <- aggregate(iris[,1:4], by=list(iris$Species), FUN="mean" )
irisagg

barplot(irisagg$Sepal.Length, names.arg=irisagg$Group.1,
        legend.text=irisagg$Group.1, col=irisagg$Group.1)

#------------------------------------------------------------------------------
#                         ggplot
#------------------------------------------------------------------------------

data(mtcars)
summary(mtcars)
head(mtcars)

library(ggplot2)

#line plot
ggplot( data=mtcars, aes( x=mpg, y=wt ) ) + 
  geom_line()


#histogram
ggplot(mtcars, aes(x=cyl)) + 
  geom_histogram(binwidth=1, colour="green") +
  theme_bw()

#density
ggplot(mtcars, aes(x=cyl)) + 
  geom_density()

#box plot
ggplot(mtcars, aes(x=factor(cyl), y=mpg, colour=factor(cyl))) + 
  geom_boxplot() + 
  labs(title="Boxplot", x="# of cylinders", 
       y="Miles per gallon")


# scatter plot
ggplot( data=mtcars, aes( x=mpg, y=wt, 
                          colour=as.factor(cyl), shape=as.factor(gear),
                          label=as.character(rownames(mtcars)) ) ) + 
  geom_point(size=6) +
  geom_text(size=3, colour="black")

#pie chart
ggplot(mtcars, aes(x=factor(1), fill=factor(cyl))) + 
  geom_bar(width=1) +  
  coord_polar(theta="y") 

# Faceting 

ggplot(mtcars, aes(x=hp, y=mpg, colour=factor(am))) + 
  geom_point() + 
  facet_grid( cyl ~ gear ) +
  theme_bw() 


# Heat Maps

library(graphics); 
library(grDevices)

heatmap(as.matrix(mtcars), Rowv = NA, Colv = NA, scale = "column",
        main = "heatmap")

#Time Series

timeseries <- read.csv("timeseries.csv")
head(timeseries)

timeseries$Date2 <- as.Date(timeseries$Date, 
                            format="%m/%d/%Y")

ggplot(timeseries, aes(Date2, 
                       y=Value, colour="blue")) + 
  geom_line()

ggplot(timeseries, aes(format(Date2,'%m'),
                       y=Value, colour=format(Date2,'%b'))) +
  geom_boxplot()

#Plotting maps

library(ggmap)
indiamap <- qmap("India", zoom=5, legend="bottom")
indiamap

poi <- read.csv("capoi.csv")
summary(poi)

calimap <-qmap("California", zoom=7, 
               legend="bottom", maptype="satellite")
calimap

calimap <-qmap("California", zoom=7, 
               legend="bottom", maptype="toner")
calimap +  geom_point(aes(x=Longitude, y=Latitude,
                          color=PointOfInterest), 
                      shape=20, size=3,
                      data = poi[ poi$PointOfInterest %in% 
                                    c("airport", "dam", "reservoir", "tunnel"), ]) 
