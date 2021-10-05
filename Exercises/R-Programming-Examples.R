#
#-----------------------------------------------------------------------------
#
#                         R Programming Examples
#
#                         Copyright : V2 Maestros @2015 + Pascal @202010
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
#setwd("C:/Personal/V2Maestros/Modules/R Programming")
getwd()

#------------------------------------------------------------------------------
#                         Variables
#------------------------------------------------------------------------------

#a Character/String variable
aCharVar <- "Programming"
aCharVar
aCharVar <- "A Different Value"
aCharVar

#a Numeric variable
aNumVar <- 1992.12
aNumVar

#Using Scientific Notation
aNumVar <- 2.3e-4
aNumVar

#Boolean variable
aBoolVar <- TRUE
bBoolVar <- FALSE
aBoolVar
bBoolVar

#Changing data types
as.character(aNumVar)
as.numeric(aBoolVar)  # True = 1
as.numeric(bBoolVar)  # False = 0

#Invalid conversion results in NA (not applicable)
as.numeric(aCharVar)

# See all variables in the workspace
ls()
lsresult <- ls()
lsresult[3]

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
string2[3] # => NA : ok
string2[2]

s1 <- paste("I can also use", "paste")
s2 <- paste0("To concat", "without spaces")
s3 <- cat('testing \t tabs')  #!!! cat return NULL !!!


#------------------------------------------------------------------------------
#                         Date and Time
#------------------------------------------------------------------------------

#POSIX date time
Sys.time()
?Sys.time()  #? to view the help ...

unclass(Sys.time()) # Unix time format !

as.Date('2015-01-16')
as.Date('1/16/2015',format='%m/%d/%Y')

#------------------------------------------------------------------------------
#                         Vectors
#------------------------------------------------------------------------------

myVector <- c(1,2,3)
myVector
class(myVector)  # pour voir le type ex. numeric, integer

myVector <- c(1:100)
myVector
class(myVector)

myVector[4]
myVector[5:8]
myVector[-3]   # enlève le 3e élément de façon logique

myVector > 40   # Test sur les valeurs du vecteur

length(myVector)
sum(myVector)

vec1 <- c(3,4,5)
vec2 <- c(10,11,12)
vec1+vec2   # sum sur les colonnes

vec3 <- c(1)
vec1+vec3   # sum sur tous les éléments

vegetables <- c(5,7,3)
names(vegetables) <- c("carrot","beans","broccoli")
vegetables   # nous avons créé une HashMap<K,V>
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
empId <- c(1,2,3,4)
empName <- c("John", "Mike", "Randy","Pascal")
isManager <- c(FALSE, TRUE, FALSE , FALSE)
salary <- c(1242.11, 3490.20, 2201.87, 3600)

emp_df <- data.frame(empId,
                     empName,
                     isManager, 
                     salary, 
                    # stringsAsFactors = TRUE)
                      stringsAsFactors = FALSE)
emp_df

class(emp_df)
nrow(emp_df)
ncol(emp_df)
str(emp_df)      # display the internal structure of an R object
summary(emp_df)  # summary of data set
names(emp_df)    # names of columns
head(emp_df, 2)
tail(emp_df, 2)

emp_df[1,3]      # access row 1 col 3
emp_df[1:2,1:3]  # access range
emp_df$salary    # access full salary column
emp_df$empName[2]  # access 2nd element in a column

sum(emp_df$salary) # sum on salary column

emp_df$salary > 2000
emp_df [ emp_df$salary < 2500, 1] # filter on salary

emp_df$dept_id <- c(1,1,2)  #add a new column dept_id
emp_df
emp_df[ emp_df$dept_id == 2 , (1:5)] # filter on dept_id

# builtin data frame
# ref. https://www.rdocumentation.org/packages/datasets/versions/3.6.2/topics/iris
#  contains three plant species (setosa, virginica, versicolor) ...
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

rbind( df1, df2)   #row binding

cbind(df1, df3)    #col binding

#------------------------------------------------------------------------------
#                         Matrices
#------------------------------------------------------------------------------
demo_matrix <- matrix(c(2,4,6,3,5,8), ncol=3,nrow=2)
demo_matrix2 <- matrix(c(2,4,6,3,5,8), nrow=2,ncol=3)

demo_matrix
demo_matrix2

t(demo_matrix)

#------------------------------------------------------------------------------
#                         Factors
#------------------------------------------------------------------------------

vFactor <- as.factor(c("Apple","Orange"))
vFactor

as.factor(emp_df$empName)
table(as.factor(emp_df$isManager))

vPascal <- c(4,"Pascal", FALSE, 3600.0, 3)
vPascal
emp_df <- rbind(emp_df,vPascal)

table(as.factor(emp_df$salary))

#------------------------------------------------------------------------------
#                         Sorting
#------------------------------------------------------------------------------
aVector <- c (6,3,4,11,2,9,5)
sort(aVector)

emp_df_sort <- emp_df[ order(salary), ]
emp_df_sort

emp_df_sort_empName <- emp_df[ order(empName), ]
emp_df_sort_empName



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
str(iris)
iris[1,"Species"]
#Loop through every row in the data frame
for (i in 1:nrow(iris)) {
  if ( iris[i,"Species"] == "setosa") {
    print(paste(i, "is a setosa"))
  } else if ( iris[i,"Petal.Width"] > 1.4 ) {
    print(paste(i, "is a specy with petal widther than  1.4"))
  } else {
    print(paste(i, "is NOT setosa, but ", iris[i,"Species"]))
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

# ref. https://statistics.berkeley.edu/computing/r-reading-webpages
#      https://www.programmingr.com/content/webscraping-using-readlines-and-rcurl/
#      http://www.endmemo.com/r/gsub.php  (regex includes)
readURL <- function(url) {
  print(url)
  webPage <- readLines(url)
  return(webPage)
}

page <- readURL("http://www.jefflemat.fr/autres/")
index_lines <- grep('desja(.*)pdf', page)

for (i in 1:length(index_lines)) {
  # print(paste("line:",page[indexes[i]]))
  livre <- gsub("(.*)href=.","", page[indexes[i]], perl = FALSE)
  livre2 <- gsub("\">(.*)$","", livre, perl = FALSE)
  print(livre2)
}

# r read webpage
web_page <- readLines("http://www.jefflemat.fr/autres/")
class(web_page)
typeof(web_page)
length(web_page)
attributes(web_page)
str(web_page)
# names(web_page) <- c("Autres") ???


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

aggregate(iris_df, by=list(iris_df$Species), FUN=mean)
aggregate(iris_df$Petal.Width, by=list(iris_df$Species), FUN=range)

cor(iris_df[,1:4])

install.packages("psych")
library(psych)
pairs.panels(iris_df)

iris_df$Species <- as.numeric(iris_df$Species)  # add Species numeric column
iris_df
iris_model <- lm(Species ~ . , iris_df)
summary(iris_model)

#------------------------------------------------------------------------------
#                         Base Plotting System
#------------------------------------------------------------------------------
par(mfrow=c(2,2))
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
plot(iris$Species, sort(iris$Sepal.Length), type="l")

irisagg <- aggregate(iris[,1:4], by=list(iris$Species), FUN="mean" )
irisagg

par(mfrow=c(1,1))
barplot(irisagg$Sepal.Length, names.arg=irisagg$Group.1,
        legend.text=irisagg$Group.1, col=irisagg$Group.1)

#------------------------------------------------------------------------------
#                         ggplot
#------------------------------------------------------------------------------
data(mtcars)
summary(mtcars)
head(mtcars)

install.packages("ggplot2")
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
head(timeseries)

ggplot(timeseries, aes(Date2,
                       y=Value, colour="orange")) +
  geom_line()

ggplot(timeseries, aes(format(Date2,'%m'),
                       y=Value, colour=format(Date2,'%b'))) +
  geom_boxplot()

#Plotting maps - Google Map & Open Street Map
api <- "AIzaSyCUgNlOjGrxS7FWP05uJS-yzCC196HGzzo" # Text file with the API key
# AIzaSyCUgNlOjGrxS7FWP05uJS-yzCC196HGzzo
register_google(key = api)
getOption("ggmap")

locations <- c("Hoensbroek", "Johannesburg", "Barrow-in-Furness",
               "Hong Kong", "Singapore", "Tangail", "Maastricht", "Bendigo") %>%
     geocode()

library(ggplot2)
library(mapproj)

world <- map_data("world")
ggplot() +
     geom_polygon(data = world,  aes(long, lat, group = group),
                  fill = "grey") +
     geom_point(data = locations, aes(lon, lat),
                colour = "red", size = 5) +
     coord_map("ortho", orientation = c(30, 80, 0)) +
     theme_void()


# Ex. 2 - library(ggmap)
Amap <- qmap("Belgium", zoom=5, legend="bottom")
Amap

# https://cran.r-project.org/web/packages/OpenStreetMap/index.html
library(tmaptools)
ggmap(get_stamenmap(rbind(as.numeric(paste(geocode_OSM("Arlon")$bbox))), zoom = 14))

# Ex. 3 - capoi.csv
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
