# Title     : R Use Case - Association Rules Mining  – ADS_5
# Objective : Predicting which accident conditions frequently occur together
# Created by: Pascal
# Created on: 07-11-20
getwd()
# "C:/Users/User/Workspace/RprogProjects/AppliedDataScience_5"
accident_data <- read.csv("accidents.csv")
str(accident_data); dim(accident_data) # 1000 obs. of  16 variables
accident_data[1:5,]
summary(accident_data)

# data transformation - building a basket
colnames <- names(accident_data)
str(colnames); length(colnames)
# In a basket, every row represents a transaction (Id,<K=V>, ...)
# paste - ref. https://www.math.ucla.edu/~anderson/rw1001/library/base/html/paste.html
# char - ref. https://stat.ethz.ch/R-manual/R-devel/library/base/html/nchar.html
basket <- list()
for (n in 1:nrow(accident_data)) {
  kv <- paste0(n, SEP=",")
  for (c in 2:ncol(accident_data)) {  # without column n°2
    transaction <- paste0(colnames[c], sep = "=", accident_data[n,c], collapse = NULL)
    kv <- paste0(kv, transaction, SEP=",")
  }
  basket[n] <- substr(kv, 1, nchar(kv) - 1)
}
write(as.character(basket),"accidents_basket.csv")

# Exploratory Daa Analysis
# arules - ref. https://www.rdocumentation.org/packages/arules/versions/1.6-6
#               https://www.rdocumentation.org/packages/arules/versions/1.6-6/topics/read.transactions
library(arules)  # load "Mining Association Rules and Frequent Itemsets"
accidents <- read.transactions("accidents_basket.csv", format = "basket", sep=",")
summary(accidents)
inspect(accidents)

# Items Frequency Plot - https://www.rdocumentation.org/packages/arules/versions/1.5-5/topics/itemFrequencyPlot
itemFrequencyPlot(accidents, type = "absolute", topN = 10, popCol = "green", popLwd = 1,
                  lift = FALSE, horiz = TRUE, names = TRUE, cex.names=graphics::par("cex.axis"),
                  main = "Accident conditions")

# Modelling and predictions
# apriori - ref. https://www.rdocumentation.org/packages/arules/versions/1.6-6/topics/apriori
# The default behavior is to mine rules with minimum support of 0.1, minimum confidence of 0.8,
#  maximum of 10 items (maxlen), and a maximal time for subset checking of 5 seconds (maxtime).
rules <- apriori(accidents, parameter=list(supp=0.1, conf = 0.3))
class(rules)  # arules
length(rules) # set of 21772 rules

# Example also here : http://r-statistics.co/Association-Mining-With-R.html
# Rules with confidence of 1 (see rules_conf above) imply that, whenever the XXX item was purchased,
# the YYY item was also purchased 100% of the time.
rules_conf <- sort (rules, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf))

# A rule with a lift of 18 (see rules_lift above) imply that, the items in XXX and YYY
#  are N times more likely to be purchased together compared to the purchases when they
#   are assumed to be unrelated.
rules_lift <- sort (rules, by="lift", decreasing=TRUE) # 'high-lift' rules.
inspect(head(rules_lift)) # show the support, lift and confidence for all rules



