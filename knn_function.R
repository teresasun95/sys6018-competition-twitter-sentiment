library(tidyverse)
library(XML)
library(tm)

# Read in the Data

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

data = data.frame(sentimentID = c(train$sentiment,test$id), text = c(train$text,test$text))

data.clean = tweet.cleaner(data)
data.tfidf = DocumentTermMatrix(data.clean, control = list(weighting = weightTfIdf))
data.b = removeSparseTerms(data.tfidf, .97) #if you want to play with the sparceness do so here, to ensure the variables remain equal
data.b.df = data.frame(as.matrix(data.b))

train.clean = data.frame(sentiment = train$sentiment, data.b.df[1:981,])
test.clean = data.frame(id = test$id, data.b.df[982:1960,])

test.clean <- test.clean[,-1]

##################
####### knn ######
##################

# knn function from scratch
# response: the response variable in training data
# train: training data with all the variables and response variable
# test: testing data
# k

knn_pred <- function(response, train, test, k) {
  n = nrow(test)
  pred <- c() # create a list to store the predictions
  y <- train[,response] # get the response variable from training set
  train[[response]]=NULL # remove the response variable from training set
  for(i in 1:n){
    dist <- apply(train,1, function(x) sum((x-test[i,])^2)) 
    # calculate the euclidean distance for each instance in training set
    ordered <- order(dist)[1:k] # sort the list of distances
    freq <- table(y[ordered]) # compute the frequencies of each class
    most.frequent.classes = names(freq)[freq == max(freq)]
    pred<-c(pred,sample(most.frequent.classes,1))
  }
  return(pred)
}

pred <- knn_pred('sentiment',train.clean, test.clean,k = 3)
pred2 <- knn_pred('sentiment',train.clean, test.clean,k = 5)

## The KNN function we created took a long time (>20 minutes) to run. 
## Though the function gave no erros, it didn't run as we expected.
## So we decided to break the function into pieces, which worked smoothly. 
## The only thing needs to change is the k value. 

# for k = 3
n = nrow(test.clean)
pred <- list() # create a list to store the predictions
y <- train.clean[,'sentiment'] # get the response variable from training set
train.clean[['sentiment']]=NULL

for(i in 1:n){
  dist <- apply(train.clean,1, function(x) sum((x-test.clean[i,])^2)) 
  # calculate the euclidean distance for each instance in training set
  ordered <- order(dist)[1:3] # sort the list of distances
  freq <- table(y[ordered]) # compute the frequencies of each class
  most.frequent.classes = names(freq)[freq == max(freq)]
  pred[i] = sample(most.frequent.classes, 1)
}
print(pred)

# for k = 5

n = nrow(test.clean)
pred <- list() # create a list to store the predictions
y <- train.clean[,'sentiment'] # get the response variable from training set
train.clean[['sentiment']]=NULL

for(i in 1:n){
  dist <- apply(train.clean,1, function(x) sum((x-test.clean[i,])^2)) 
  # calculate the euclidean distance for each instance in training set
  ordered <- order(dist)[1:5] # sort the list of distances
  freq <- table(y[ordered]) # compute the frequencies of each class
  most.frequent.classes = names(freq)[freq == max(freq)]
  pred[i] = sample(most.frequent.classes, 1)
}
print(pred)


## we compared the results with k=1,k=3,k=5
## k=5 gives us the best result


# write the csv file for submission on Kaggle
# our final submission on kaggle with KNN
finalpred5<- data.frame(cbind(test$id,pred))
colnames(finalpred5)<- c("id", "sentiment")
df5 = as.matrix(finalpred5)
write.csv(df5, "sys6018-twitter-sentiment-KNN-k5-97.csv", row.names = FALSE)





