# importing churn data set from .csv file
data1 <-read.csv("C:/Users/REVANTH/Desktop/data/churn_data.csv",header= TRUE,sep=",")

#as the first three columns of the data arent important for the prediction we drop those columns from the data frame 
# drop the variables from the data set
data1<-data1[-c(1,2,3)]

#data exploration

#gives the dimensions of the data set
dim(data1)

#gives the names of the variables in the data set
names(data1)

#displays top 10 observations of the data set
head(data1)

#displays bottom top 10 observations of the data set
tail(data1)

#displays structure of the variables in the data set
str(data1)

#gives the summary of the variables of the data set 
summary(data1)

#library for the sample.split function
library(caTools)

#splitting the data into training and testing data 
# sample the input data with 70% for training and 30% for testing
sample <- sample.split(data1$churn,SplitRatio=0.70)

trainData <- subset(data1,sample==TRUE)

testData <- subset(data1,sample==FALSE)

#rpart --- recursive partitioning decision tree
library(rpart)

#TO BUILD DECISON TREE BY USING RPART PACKAGE(recursive partitioning decision tree)
churn_model<- rpart(churn ~ .,data=trainData)
churn_model

#to display it in diagram

library(rattle)

library(rpart.plot)

fancyRpartPlot(churn_model)

#predicting the model data based on the model built from training data set
pred<- predict(churn_model,testData,type = "class" )
pred

pred1<- data.frame(pred)
View(pred1)

#Confusion matrix is a technique for summarizing the performance of a classification algorithm 
table(testData$churn ,pred)#create confusion matrix to see how mnay cus are correctly predicted and incorrectly predicted


#####
#####RANDOM FOREST MODEL
#####


library(randomForest)

#to build the random forest model 
random_model <- randomForest(churn~.,data=trainData, importance = T)
random_model

#predicting the data based on the model developed
ran_pred <- predict(random_model,testData)
ran_pred

ran_pred1 <-data.frame(ran_pred)

table(testData$churn ,ran_pred) # confusion matrix

#accuracy rate 
(1276+ 173)/(1276+12+39+173) # 97 % of accuracy : higher than decision tree , where accuracy rate is 95 % 

#most commonly mean decrease gini is considered
importance(random_model)

# Variables that result in nodes with higher purity have a higher decrease in Gini coefficient.
varImpPlot(random_model)
#total day charges have higher decrease in gini index so its more significant than other

