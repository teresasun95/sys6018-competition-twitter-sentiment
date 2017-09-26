# sys6018-competition-twitter-sentiment
Group Competition 3-9

all code can be found in "group3-9_twitter_sentiment_competion_code.R"

## Roles
Teresa has been tasked with construction and implementation of the KNN model

James and Gregory have been tasked with the data cleaning and exploration as well as the implementation of parametric models

Teresa and Gregory worked on cleaning the code, compiling it, and writing up the report

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
The parametric approach worked by running regressions on the tfidf of the data.

The initial model was a mulitivariable model run on all words on the 113 variables
The model was stripped of weaker variables leaving it with 53 variables

The models were then cross validated to and the stripped down model performed better than the full one.

From that predictions were created on the test set and output as a csv in "lmpred.csv"


## Written report
also available in "group3-9_writeup.docx"

	Text analysis is an incredibly important and applicable form of data science. Text, being one of the primary means of communication, can provide valuable amounts insight into people’s views. Text is thus an incredibly valuable form of data that can nonetheless prove somewhat more challenging to work with. The text analysis of twitter data we performed showcases the benefits and pitfalls of such analysis. It shows the various potential benefits in working with text analysis while showing the difficulty in finding means of powerfully analyzing it.
	The analysis of twitter sentiment showed how tweets can be used to estimate product interest and expectations. Accurately predicting this can provide valuable data and analysis to many interested parties. Firstly, it can tell companies how users feel about a certain product. Knowing this, a company can adjust in many ways. It can work on targeting people interested already, or it can use this to alter it course and interest new people: such as by adjusting its marketing. Similarly, they can forecast how much interest there will be in the product by seeing how people feel towards it.  Views towards electric cars being positive, for example, may indicate a willingness to immediately adopt it. Conversely, regulators may use the data to understand what concerns consumers may have with a product. For example, if they predict a lot of negative sentiments, then they may want to explore those low sentiment tweets and see what the concern is. Companies and regulators may then work to address the problem before it become an issue. The value of these tweet analyses and predictions is that they give insights into how people conceive of and how people view a product: understanding this may help allay concerns and entice desires. 
	This problem is challenging however because deciphering people’s full views from tweets is complicated. The tweets themselves were manually marked up, so the models may be fitting towards the markers conception of positive instead of the tweeters. There may be further bias in using sentiments in tweets as a proxy for overall sentiment since. People who use twitter for example may not be an accurate reflection of the populace; this is especially true when looking at this electric car example since people taking the time to tweet about them may be a special subpopulation. Even without these biases there is room for errors and issues. In our own model, we had limited success accurately predicting the sentiments. This was likely due to overfitting the model on the training set. Text presents a variety of possible relations and finding the accurate and predictive ones can be difficult. 
	This problem resembles various other problems in text mining. One such example is our team member—Greg’s—capstone. In that text is being analyzed in Arabic to see whether it is associated with violence in the hopes of creating a predictive model. Similarly, there have been attempts to predict crime hotspots with twitter that resemble this. They look for certain word clusters to see how they may be associated with crimes and how that can predict it.

