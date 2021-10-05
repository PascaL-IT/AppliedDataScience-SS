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
setwd("C:/Personal/V2Maestros/Modules/Data Engineering")

#------------------------------------------------------------------------------
#                         Data Acquisition
#------------------------------------------------------------------------------

# DB Data Acquisition

library(RMySQL)

con <- dbConnect(MySQL(), user="root", password="", 
                 dbname="demo", host="localhost" )
rs <- dbSendQuery(con, "select name from demotable limit 10;")

data <- fetch(rs, n=10)
huh <- dbHasCompleted(rs)
dbClearResult(rs)
dbDisconnect(con)

data

#downloading files from web
web_url <- "https://sourceforge.net/p/openflights/code/HEAD/tree/openflights/data/airlines.dat?format=raw"
local_file <- "downfile.csv"
download.file(web_url,local_file )
airlines_data <- read.csv(local_file)
str(airlines_data)
head(airlines_data)

#scraping web pages
library(RCurl)
down_page <- getURL("http://openflights.org/data.html")
cat(down_page)

#------------------------------------------------------------------------------
#                         Accessing REST 
#---------------------------------------------------------------------------

library(httr)
library(jsonlite)
library(httpuv)

#Register an application with github to get the key and secret.
gitapi <- oauth_app("github", "cd235362e0f195748d3e",
                   secret="b982628aa7eb37e650150764a99b51e55e4337bd")

git_token <- oauth2.0_token(oauth_endpoints("github"), gitapi)

sample_request <- GET("https://api.github.com/users/kumaranpm",
                      config(token=git_token))
sample_request

content(sample_request)$blog

#------------------------------------------------------------------------------
#                         Data Cleansing 
#---------------------------------------------------------------------------

#finding outliers
students_age <- c(-1, 3,4,12,6,8,4,5,7)
quantile(students_age)
boxplot(students_age)

students_age[students_age < 0]

#------------------------------------------------------------------------------
#                         Data Transformations 
#---------------------------------------------------------------------------

car_data <- mtcars
str(car_data)
summary(car_data)
# factor conversion
car_data$fact_cyl <- as.factor( car_data$cyl)

# binning
quantile(car_data$hp)
car_data$bin_hp <- cut(car_data$hp, c(0,100, 200,300,400))

# indicator variables
car_data$is_4cyl <- ifelse( car_data$cyl == 4, 1,0)
car_data$is_6cyl <- ifelse( car_data$cyl == 6, 1,0)

# centering and scaling
car_data$scaled_mpg <- scale(car_data$mpg)[,1]


