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
data.b = removeSparseTerms(data.tfidf, .99) #if you want to play with the sparceness do so here, to ensure the variables remain equal
data.b.df = data.frame(as.matrix(data.b))

train.clean = data.frame(sentiment = train$sentiment, data.b.df[1:981,])
test.clean = data.frame(id = test$id, data.b.df[982:1960,])

test.clean <- test.clean[,-1]




####### separate data into training and validation sets

sub<-sample(1:nrow(train.clean), size = 0.8*nrow(data.train))
train<-data.train[sub, ]
valid<- data.train[-sub, ]

####################
#### parametric ####
####################
train.clean$sentiment <- as.factor(train.clean$sentiment)
levels(train.clean$sentiment)
sum(is.na(train.clean$sentiment))

model <- lm(sentiment~., data=train.clean)
summary(model)

model1 <- lm(sentiment~ cant + dont + googl + hope+insur+less+like+much+need+one+thing+use+vehicl+wait+want+yes, data=data.clean)

library(DAAG)

cv.err <- cv.lm(train.clean,model)
sum(cv.err$Predicted==cv.err$cvpred)
cv.err$





##################
####### knn ######
##################
# train.clean[['sentiment']] = NULL
# y <- train[,'sentiment']
y
Eudist <- function(x1,x2){
  d=sum((x1-x2)^2)
  return (sqrt(d))
}
n = nrow(test.clean)
pred <- list() # create a list to store the predictions
y <- train.clean[,'sentiment'] # get the response variable from training set
train.clean[['sentiment']]=NULL

for(i in 1:n){
  dist <- apply(train.clean,1, function(x) sum((x-test.clean[i,])^2)) 
  #print(dist)
  # calculate the euclidean distance for each instance in training set
  ordered <- order(dist)[1:5] # sort the list of distances
  freq <- table(y[ordered]) # compute the frequencies of each class
  #print(freq)
  #print(names(freq))
  most.frequent.classes = names(freq)[freq == max(freq)]
  #print(most.frequent.classes)
  pred[i] = sample(most.frequent.classes, 1)
}
print(pred)

finalpred5<- data.frame(cbind(test$id,pred))
colnames(finalpred5)<- c("id", "sentiment")
df5 = as.matrix(finalpred5)
write.csv(df5, "sys6018-twitter-sentiment-KNN-k5-97.csv", row.names = FALSE)


finalpred<- data.frame(cbind(test$id,pred))
colnames(finalpred)<- c("id", "sentiment")
df = as.matrix(finalpred)
write.csv(df, "sys6018-twitter-sentiment-KNN.csv", row.names = FALSE)




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
    #print(freq)
    #print(names(freq))
    most.frequent.classes = names(freq)[freq == max(freq)]
    #print(most.frequent.classes)
    pred<-c(pred,sample(most.frequent.classes,1))
  }
  # factor(pred, levels=levels(y))
  # print(pred)
  return(pred)
}

knn_pred('sentiment',train.clean,test.clean,k=5)
pred
finalpred2<- data.frame(cbind(test$id,pred))
colnames(finalpred2)<- c("id", "sentiment")
df2 = as.matrix(finalpred2)
write.csv(df2, "sys6018-twitter-sentiment-KNN.csv", row.names = FALSE)


