# sys 6018 twitter sentiment analysis #

library(tidyverse)
library(XML)
library(tm)

#Read in the Data

train = read.csv('train.csv', stringsAsFactors = FALSE)
test = read.csv('test.csv', stringsAsFactors = FALSE)

# create a function to clean the data

tweet.cleaner = function(X) {
  
  get.tweets = function(X) { 
    tweets = data.frame(X[2], stringsAsFactors = FALSE)
    tweets = VCorpus(DataframeSource(tweets))
  }
  
  tweets.clean = function(X) {
    tweets.clean = tm_map(get.tweets(X), stripWhitespace)                   # remove extra whitespace
    tweets.clean = tm_map(tweets.clean, removeNumbers)                      # remove numbers
    tweets.clean = tm_map(tweets.clean, removePunctuation)                  # remove punctuation
    tweets.clean = tm_map(tweets.clean, content_transformer(tolower))       # ignore case
    tweets.clean = tm_map(tweets.clean, removeWords, stopwords("english"))  # remove stop words
    tweets.clean = tm_map(tweets.clean, stemDocument)
  }
  
  return(tweets.clean(X))
}

# function to unpack a vcorpus's cleaned content into a list
tweet.unpacker = function(N) {
  lapply(X = 1:length(N), function(X) {
    N[[X]]$content})
}

# create test and train VCorpus
train.tweets.clean = tweet.cleaner(train)
test.tweets.clean = tweet.cleaner(test)

# create dataframes of test and train with cleaned tweets
train.cleaned = data.frame(sentiment = train$sentiment , text = unlist(tweet.unpacker(train.tweets.clean)))                 
test.cleaned = data.frame(id = test$id , text = unlist(tweet.unpacker(test.tweets.clean))) 

########################
### knn from scratch ###
########################
knn_pred <- function(response, train, test, k){
  n = nrow(test)
  pred <- rep(NA, n) # create a list to store the predictions
  y <- train[,response] # get the response variable from training set
  train[[response]]=NULL # remove the response variable from training set
  #test[[response]]=NULL 
  for(i in 1:n){
    dist <- apply(train,1, function(x) sum((x-test[i,])^2)) 
    # calculate the euclidean distance for each instance in training set
    ordered <- order(dist)[1:k] # sort the list of distances
    freq <- table(y[ordered]) # compute the frequencies of each class
    #print(freq)
    # print(names(freq))
    most.frequent.classes = names(freq)[freq == max(freq)]
    print(most.frequent.classes)
    pred[i] = sample(most.frequent.classes, 1)
  }
  factor(pred, levels=levels(y))
}
knn_pred('sentiment',train.cleaned,test.cleaned,k=3)