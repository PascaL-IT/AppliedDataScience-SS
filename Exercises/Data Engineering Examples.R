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
# setwd("C:/Personal/V2Maestros/Modules/Data Engineering")
# => "C:/Users/User/Workspace/RprogProjects/AppliedDataScience_3"
getwd()

#------------------------------------------------------------------------------
#                         Data Acquisition
#------------------------------------------------------------------------------

# DB Data Acquisition (MySQL vs. PostgreSQL)
# Ref. https://www.oracle.com/fr/database/postgresql-versus-mysql.html
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
web_url <- "http://www.jefflemat.fr/autres/"
docfile <- "148_a_desjardins_-_les_chemins_de_la_sagesse.pdf"
download.file(paste0(web_url,docfile), docfile, mode="wb")
# lines_data <- read.csv(docfile)
library(tm)
getReaders()
library(pdftools)
# Text Mining
# 1) doc <- readPDF(control = list(text = "-layout"))(elem = list(uri = docfile), language = "fr", id = "pdf1")
aReader <- readPDF(engine="pdftools")
doc <- aReader(elem = list(uri = docfile), language = "fr", id = "pdf1" )
class(doc)
str(doc)
class(doc$meta)
class(doc$content)
str(doc$meta)
str(doc$content)
cat(content(doc)[2])
head(inspect(doc))
length(doc)
dir()

# ref. https://www.rdocumentation.org/packages/tm/versions/0.7-7/topics/Corpus
corpus = VCorpus(VectorSource(doc$content))
class(corpus)
str(corpus)
doc2 <- tm_map(corpus, stripWhitespace)
doc2 <- tm_map(doc2, content_transformer(tolower))
doc2 <- tm_map(doc2, removeNumbers)
doc2 <- tm_map(doc2, removePunctuation)
doc2 <- tm_map(doc2, removeWords, stopwords(kind = "french"))
doc2 <- tm_map(doc2, removeWords, c("les","que","est","qui","des","une","pas","dans","nous","plus","pour","par"))
#doc2 <- tm_map(doc2, stemDocument, language = "french")
inspect(doc2)

# Creating the Bag of Words model
dtm = DocumentTermMatrix(doc2)
dtm = removeSparseTerms(dtm, 0.999)
dataset = as.data.frame(as.matrix(dtm))
class(dataset)
dataset

termFreq1 <- colSums(as.matrix(dtm))
tf1 <- data.frame(term = names(termFreq1), freq = termFreq1)
tf1 <- tf1[order(-tf1[,2]),]
head(tf1, 100)


#wordCloud
library(wordcloud)
datasetCloud = as.matrix(dtm)
v = sort(colSums(datasetCloud),decreasing=TRUE)
myNames = names(v)
d = data.frame(word=myNames,freq=v)
wordcloud(d$word, colors=c(3,4),random.color=FALSE, d$freq, min.freq=80)


# Web scraping with R
# Ref. http://edutechwiki.unige.ch/fr/Web_scraping_avec_R , https://thinkr.fr/rvest/
library(RCurl)
# down_page <- getURL("https://openflights.org/data.html")
down_page <- getURL("https://www.evolute.fr/connaissance-soi/test-culpabilite")
cat(down_page)

text_base <- readLines("https://www.evolute.fr/connaissance-soi/test-culpabilite")
cat(text_base)

#------------------------------------------------------------------------------
#                         Accessing REST 
#------------------------------------------------------------------------------
library(httr)
library(jsonlite)
library(httpuv)

#Register an application with github to get the key and secret.
gitapi <- oauth_app("github", "cd235362e0f195748d3e",
                   secret="b982628aa7eb37e650150764a99b51e55e4337bd")
git_token <- oauth2.0_token(oauth_endpoints("github"), gitapi)

# Also on LinkedIn : https://www.linkedin.com/learning/instructors/kumaran-ponnambalam
# Test with GitHub REST API v3 : https://developer.github.com/v3/#current-version
# Test connection with Curl : https://curl.haxx.se/windows/
# Ref. https://developer.github.com/v3/
sample_request <- GET("https://api.github.com/users/kumaranpm", config(token=git_token))
sample_request
content(sample_request)$blog

library(curl)
req <- curl_fetch_memory("https://api.github.com/")
str(req)
class(req)
parse_headers(req$headers)
json_content <- jsonlite::prettify(rawToChar(req$content))
class(json_content)
content <- jsonlite::parse_json(json_content)
class(content)
names(content)
content[32]$user_search_url


# CONVID19 - Sciensano
# Dataset : https://epistat.wiv-isp.be/covid/


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
car_data$fact_cyl <- as.factor(car_data$cyl)
str(car_data)

# binning
quantile(car_data$hp)
car_data$bin_hp <- cut(car_data$hp, c(0,100, 200,300,400))
str(car_data)
car_data
# indicator variables
car_data$is_4cyl <- ifelse( car_data$cyl == 4, 1,0)
car_data$is_6cyl <- ifelse( car_data$cyl == 6, 1,0)
str(car_data)

# centering and scaling
car_data$scaled_mpg <- scale(car_data$mpg)[,1]
str(car_data)

