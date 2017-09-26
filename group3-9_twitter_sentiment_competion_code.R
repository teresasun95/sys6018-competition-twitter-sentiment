
### Twitter Sentiment Analysis ###
# Gregory Wert : gaw8pa | Teresa Sun : js6sk | James Xie : jx4cc

library(tidyverse)
library(tm)
library(DAAG)

#---------------------------------------------------------------------------------------------------------------------
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

# Combine the train and test so they have commmon variables in tfidf later
data = data.frame(sentimentID = c(train$sentiment,test$id), text = c(train$text,test$text))

#clean the combined data
data.clean = tweet.cleaner(data)

# convert data into tfidf and clean for sparcity
data.tfidf = DocumentTermMatrix(data.clean, control = list(weighting = weightTfIdf))
data.b = removeSparseTerms(data.tfidf, .99) #99% brings the variable count down while maintaining a good amount at 113 words
data.b.df = data.frame(as.matrix(data.b)) 

#seperate out the training and test sets 
train.clean = data.frame(sentiment = train$sentiment, data.b.df[1:981,])
test.clean = data.frame(id = test$id, data.b.df[982:1960,])

#---------------------------------------------------------------------------------------------------
##################
### parametric ###
##################

# create a model with every word as a variable
model0 = lm(sentiment~., data=train.clean)
sum0 = summary(model0)

# remove terms that had less than 50% confidence
model1 = lm(sentiment~accid+around+audi+auto+autonom+buy+cant+car+citi+come+cool+dont+drive+futur+get+googl+googleãoââ.ãã.â.ãâ+happen+hit+hope+industri+innov+insur+interest+just+less+look+make+much+need+now+one+peopl+public+realli+road+robot+saw+see+selfdriv+show+soon+state+still+taxi+thing+think+thought+today+use+vehicl+wait+want+wheel+will+work+yes, data = train.clean)

# examine the changes in r-squared versus adj r squared
sum1 = summary(model1)
(sum1$adj.r.squared - sum0$adj.r.squared) > abs(sum1$r.squared - sum0$r.squared) # decrease in r2 is smaller than change in adj r2

# repeat
model2 = lm(sentiment~accid+around+audi+auto+cant+car+come+cool+dont+drive+futur+get+googl+googleãoââ.ãã.â.ãâ+happen+hit+hope+industri+innov+insur+just+less+look+make+much+need+now+one+peopl+public+realli+road+robot+saw+see+selfdriv+show+soon+state+still+taxi+thing+think+thought+today+use+vehicl+wait+want+wheel+will+work+yes, data = train.clean)
sum2 = summary(model2)
(sum2$adj.r.squared - sum1$adj.r.squared) > abs(sum2$r.squared - sum1$r.squared) # model still a net increase, past this point no adj r benefit from dropping weak variables

#run leave one out cross validation on models
#start with all variables model
cv.err.a = cv.lm(data = train.clean, form.lm = model0, m = length(train.clean$sentiment), seed = 1)

#check for the accuracy rate
sum(as.integer(cv.err.a$Predicted)==as.integer(cv.err.a$cvpred))/length(cv.err.a$Predicted) #.931

#repeat for model2 
cv.err.b = cv.lm(data = train.clean, form.lm = model2, m = length(train.clean$sentiment), seed = 1)
sum(as.integer(cv.err.b$Predicted)==as.integer(cv.err.b$cvpred))/length(cv.err.b$Predicted) #.973

# use model2 to generate predictions
set.seed(1)
pred = as.integer(predict.lm(model2, test.clean))

# create dataframe of predictions then output
lm.preds = data.frame(id = test$id, sentiment = pred)
write.csv(lm.preds, 'lmpred.csv', row.names = FALSE, quote = FALSE)


#---------------------------------------------------------------------------------------------------------------------
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

#-----------------------------------------------------
##################
###### open ######
##################

# find the mean sentiment
mean(train$sentiment)

# see the distribution
hist(train$sentiment) # appears that 3 is very popular

# see what percentage of tweets have a sentiment of 3
t = train[train$sentiment ==3,]
length(t$sentiment)/length(train$sentiment)

# create a model of only 3s
mode_pred = data.frame(id=test$id, sentiment=rep(3, length(test$id)))
write.csv(mode_pred, 'pred3.csv', row.names = FALSE, quote = FALSE)