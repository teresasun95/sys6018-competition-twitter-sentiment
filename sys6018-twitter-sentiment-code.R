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






## get the euclidean distance
euclideanDist <- function(instance1, instance2){
  distance = 0
  for(i in 1:(length(instance1)-1)){
    distance = distance + (instance1[i]-instance2[i])^2
  }
  d = sqrt(distance)
  return(d)
}

distance = list()
## collect K neighbors
getNeighbors <- function(train_data, test_data, k){
  distance = list()
  
  for(i in 1:length(train_data)){
    dist = euclideanDist(test_data[i,],train_data)
    #print(dist)
    distance = c(distance, dist)
  }
  order(distance)
  print(distance)
  neighbors = list()
  for(i in 1:k){
    neighbors = c(neighbors,distance[i][1])
  }
  return(neighbors)
}


## get response
getResponse <- function(neighbors, response){
  classVotes = c()
  df = data.frame(classVotes)
  for(i in 1:nrow(neighbors)){
    if(response[i] %in% classVotes) {
      df$classVotes[df$response == response[i]] = df$classVotes[df$response == response[i]] +1
    }
    else{
      df$classVotes[df$response == response[i]] = 1
    }
  }
  sortedVotes = df[-order(df$classVotes),]
  return(sortedVotes$response[1])
}

train <- rbind(iris3[1:25,,1], iris3[1:25,,2], iris3[1:25,,3])
test <- rbind(iris3[26:50,,1], iris3[26:50,,2], iris3[26:50,,3])
test
cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))
train <- cbind(train,cl)
class(train)
pred = rep(NA_character_, n)

getResponse(getNeighbors(train,test,1),cl)

## accuracy
getAccuracy <- function(test_data, response, predictions){
  correct = 0
  for(i in 1:nrow(test_data)){
    if(response[i] == predictions[i]){
      correct = correct + 1
    }
  }
  return(correct/nrow(test_data))
}



knn_predict <- function(test_data, train_data,cl, k_value){
  pred <- c()  #empty pred vector 
  #LOOP-1
  for(i in c(1:nrow(test_data))){   #looping over each record of test data
    eu_dist =c()          #eu_dist & eu_char empty  vector
    eu_char = c()
    good = 0              #good & bad variable initialization with 0 value
    bad = 0
    
    #LOOP-2-looping over train data 
    for(j in c(1:nrow(train_data))){
      
      #adding euclidean distance b/w test data point and train data to eu_dist vector
      eu_dist <- c(eu_dist, euclideanDist(train_data[i,], test_data[j,]))
      
      #adding class variable of training data in eu_char
     # eu_char <- c(eu_char, as.character(train_data[j,][[6]]))
    }
    
    eu <- data.frame(cl, eu_dist) #eu dataframe created with eu_char & eu_dist columns
    
    eu <- eu[order(eu$eu_dist),]       #sorting eu dataframe to gettop K neighbors
    eu <- eu[1:k_value,]               #eu dataframe with top K neighbors
    
    #Loop 3: loops over eu and counts classes of neibhors.
    
    classVotes = c()
    df = data.frame(classVotes)
    for(i in 1:nrow(eu)){
      if(cl[i] %in% classVotes) {
        df$classVotes[df$class == cl[i]] = df$classVotes[df$class == cl[i]] +1
      }
      else{
        df$classVotes[df$class == cl[i]] = 1
      }
    }
    sortedVotes = df[-order(df$classVotes),]
    return(sortedVotes)
  }
  }  
  #   
  #   for(k in c(1:nrow(eu))){
  #     if(as.character(eu[k,"eu_char"]) == "g"){
  #       good = good + 1
  #     }
  #     else
  #       bad = bad + 1
  #   }
  #   
  #   # Compares the no. of neighbors with class label good or bad
  #   if(good > bad){          #if majority of neighbors are good then put "g" in pred vector
  #     
  #     pred <- c(pred, "g")
  #   }
  #   else if(good < bad){
  #     #if majority of neighbors are bad then put "b" in pred vector
  #     pred <- c(pred, "b")
  #   }
  #   
  # }
  # return(pred) #return pred vector

knn_predict(test, train,1)



