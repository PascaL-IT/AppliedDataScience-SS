# Title     : R Use Case - Naïve Bayes exercise – ADS_5
# Objective : Detecting betwenn ham and spam emails
# Created by: Pascal
# Created on: 27-10-20
getwd()
# "C:/Users/User/Workspace/RprogProjects/AppliedDataScience_5"

# 1) Data set - data.frame with 500 obs. of  2 variables
sms_data <- read.csv("sms_spam_short.csv", stringsAsFactors = FALSE)
class(sms_data); dim(sms_data); str(sms_data)
# type and text
head(sms_data[1]); head(sms_data[2])
# convert type column as factor
sms_type_factors <- as.factor(sms_data$type)
class(sms_type_factors)
str(sms_type_factors)
sms_data$type <- sms_type_factors
str(sms_data) # type are now seen as factor (2 levels)
summary(sms_data)  # 437 ham / 63 spam


# 2) Data cleansing on email messages via Corpus
library(tm)
email_msg_corpus <- Corpus(VectorSource(sms_data$text))
class(email_msg_corpus)
inspect(email_msg_corpus) # [1:3])  # inspect first 3 on 500 documents
# convert to lowercase
refined_msg_corpus <- tm_map(email_msg_corpus, content_transformer(tolower))
# removing punctuation, white space, numbers, stop and specific words
refined_msg_corpus <- tm_map(refined_msg_corpus, removePunctuation)
refined_msg_corpus <- tm_map(refined_msg_corpus, removeNumbers)
refined_msg_corpus <- tm_map(refined_msg_corpus, stripWhitespace)
refined_msg_corpus <- tm_map(refined_msg_corpus, removeWords, stopwords())
refined_msg_corpus <- tm_map(refined_msg_corpus, removeWords, c("else","the","are","for"))
refined_msg_corpus <- tm_map(refined_msg_corpus, removeWords, c("as","they","has","a","his"))
refined_msg_corpus <- tm_map(refined_msg_corpus, removeWords, c("on","when","is","in","already"))
# replacing “/”, “@” et “|” with a space char
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
refined_msg_corpus <- tm_map(refined_msg_corpus, toSpace, "/")
refined_msg_corpus <- tm_map(refined_msg_corpus, toSpace, "@")
refined_msg_corpus <- tm_map(refined_msg_corpus, toSpace, "\\|")
refined_msg_corpus <- tm_map(refined_msg_corpus, stripWhitespace)
# messages now look more cleaner ...
inspect(refined_msg_corpus)

# 3) Creation of a refined/filtered document-term matrix (dtm)
dtm <- DocumentTermMatrix(refined_msg_corpus)
class(dtm)  # "DocumentTermMatrix"    "simple_triplet_matrix"
str(dtm)    # Docs , Terms
# they are too many columns (about ~2000)
dim(dtm)    # 500    1967
# reduce to terms that are occuring at least 10 times in this refined corpus
filtered_dtm <- DocumentTermMatrix(refined_msg_corpus, list(dictionary=findFreqTerms(dtm,10)))
filtered_dtm
inspect(filtered_dtm)
dim(filtered_dtm)   # 500 docs and 59 terms
t(inspect(filtered_dtm)[1:5,])


# 4) Exploratory Data Analysis
# Build a matrix 'm' from 'filtered_dtm'
# rem. in a matrix all the elements are the same type of data
# ref. http://www.sthda.com/french/wiki/text-mining-et-nuage-de-mots-avec-le-logiciel-r-5-etapes-simples-a-savoir
#      https://www.datamentor.io/r-programming/matrix/
m <- as.matrix(filtered_dtm)
class(m); attributes(m) # $dim 500 59, $dimnames$Docs , $dimnames$Terms
head(colnames(m)) # Terms
head(rownames(m)) # Docs ids
print(head(m))

# Build a vector that is ordered descrease with sum of each term occurence
v <- sort(colSums(m),decreasing=TRUE)
head(v); class(v[1]); str(v); names(v)
print(v[1]); print(names(v[1]));

# Convert 'v' to a data frame <term,ferq>
# rem. in a data frame the columns contain different types of data
d <- data.frame(term = names(v), freq = v)
head(d, 5) # top five term occurences

# Bar plot of 'd'
# Ref. https://www.statmethods.net/graphs/bar.html
barplot(d$freq, names.arg = d$term, col = d$freq,
        #main ="Most frequent terms",
        xlab ="Term name", ylab = "Frequency",
        horiz = FALSE)

# findFreqTerms(x, lowfreq = 0, highfreq = Inf)
# Ref. https://www.rdocumentation.org/packages/tm/versions/0.7-7/topics/findFreqTerms
findFreqTerms(dtm, lowfreq = 40, highfreq = Inf)
findAssocs(dtm, terms = "call", corlimit = 0.3)

# world cloud
# Refs. https://www.rdocumentation.org/packages/wordcloud/versions/2.6/topics/wordcloud
#       https://www.rdocumentation.org/packages/RColorBrewer/versions/1.1-2/topics/RColorBrewer
library(wordcloud)
pal_colors <- brewer.pal(9, "Dark2")
display.brewer.pal(9, "Dark2")
wordcloud(refined_msg_corpus, min.freq = 5, colors = pal_colors, random.order=FALSE)
wordcloud(refined_msg_corpus[sms_data$type == "ham"], min.freq = 5, colors = pal_colors, random.order=FALSE)
wordcloud(refined_msg_corpus[sms_data$type == "spam"], min.freq = 5, colors = pal_colors, random.order=FALSE)


# 5) Modeling
# Ref. http://topepo.github.io/caret/index.html (lattice, ggplot2)
# Classification And REgression Training
library(caret)
# ref. https://www.rdocumentation.org/packages/caret/versions/6.0-86/topics/createDataPartition
inTrain <- createDataPartition(y=sms_data$type, p=0.7, list=FALSE) #matrix
dim(inTrain); inTrain[1:8,1]
# splitting the raw data between training and testing
train_rawdata <- sms_data[inTrain,]
test_rawdata <- sms_data[-inTrain,]
dim(raw_data_train);dim(raw_data_test)
str(raw_data_train);str(raw_data_test)
raw_data_train[1:3,1:2]
# splitting the corpus between training and testing
train_corpus <- refined_msg_corpus[inTrain]
test_corpus <- refined_msg_corpus[-inTrain]
#splitting the refined/filtered dtm between training and testing
train_dtm <- filtered_dtm[inTrain,]
test_dtm <- filtered_dtm[-inTrain,]
#converting numeric data to factors (nbr. of occurences vs. Yes/No found in a doc)
# via 'conv_counts' function
conv_counts <- function(x) {
  y <- ifelse(x > 0, 1, 0)
  x <- factor(y, levels = c(0,1), labels = c("No", "Yes") )
}
#applying this function on our 2 dtm (on all columns of a matrix)
# ref. https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/apply
train <- apply(train_dtm, MARGIN = 2, FUN = conv_counts)
test <- apply(test_dtm, MARGIN = 2, FUN = conv_counts)
# converting to data frames
df_train <- as.data.frame(train)
df_test <- as.data.frame(test)
df_train[1:5,1:6]
str(df_train)  # 351 obs of 59 var
str(df_test)   # 149 obs of 59 var
# add 'type' column
df_train$hamspam_type <- train_rawdata$type
df_test$hamspam_type <-  test_rawdata$type
str(df_train)  # 351 obs of 60 var
tail(df_test)

# model building
# Ref. https://www.rdocumentation.org/packages/e1071/versions/1.7-4
library(e1071)
nb_model <- naiveBayes(df_train[,-60],df_train$hamspam_type)
filename <- "my_naive_bayes_log.txt"
# ref. http://www.cookbook-r.com/Data_input_and_output/Writing_text_and_output_from_analyses_to_a_file/
sink(filename) # prepare to write into output file
nb_model
sink() # return to R Console

#Prediction with model against test samples
predicted <- predict(nb_model, df_test)
predicted
confusionMatrix(predicted, df_test$hamspam_type)
sink(filename, append=TRUE) # prepare to write into output file
confusionMatrix(predicted, df_test$hamspam_type)
sink() # return to R Console

# also explained here : https://rpubs.com/Seun/455974
#                       http://www.dbenson.co.uk/Rparts/subpages/spamR/
#                       https://www.edureka.co/blog/naive-bayes-in-r/
#
# + https://www.rdocumentation.org/packages/gmodels/versions/2.18.1/topics/CrossTable
library(gmodels)
CrossTable(predicted, df_test$hamspam_type,
           prop.chisq = FALSE, prop.t = FALSE,
           dnn = c('predicted', 'actual'))

# prediction based on the model on an real email message ??? [TODO, TBC...]
message <- "Call free"
names(message) <- "Yes Yes"
message

msg <- as.data.frame(message)
summary(msg)

predicted2 <- predict(nb_model, msg)
predicted2

summary(predicted2)
summary(sms_data$type)

