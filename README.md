# sys6018-competition-twitter-sentiment
Group Competition 3-9

## Roles
Teresa has been tasked with construction and implementation of the KNN model

James and Gregory have been tasked with the data clearing and exploration as well as the implementation of parametric models

## Nonparametric approach
KNN is a nonparametic approach.
The advantage of KNN approach is that the model is robust to noisy training data. It will be very effective if the training data is large.

The disadvantage of KNN approach includes: determination of k value, high computational cost since we need to calculate the distance of each query instance to all training samples.

The knn model from scratch can be found in the file knn_function.R.

First, we read in and cleaned the train and test data using sparsity = 0.97. KNN function takes in four values: 
1. response is the response variable in train data. In our case, response = 'sentiment'.
2. train is the cleaned train data set with all variables (including response variable).
3. test is the cleaned test data set.
4. k is our selected k value.

Second, we created a vector called pred to store all the predictions. Then extract the response variable from train data. 

Third, compute the Euclidean distance between each test point to all training samples and sort the distance list. Compute the frequencies of each class and store the mode of the class in pred. 

Our highest score with knn approach is 0.52147.

## Parametric approach

