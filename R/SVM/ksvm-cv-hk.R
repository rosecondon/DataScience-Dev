# Load required library
library("readr")
library("dplyr")
library("data.table")
library(tidyverse)
library(kernlab)

# install kernlab package for SVM ksvm function. Warnimg: package ‘kernlab’ was built under R version 3.5.2
if (!require("kernlab")) {
  install.packages("kernlab", dependencies = TRUE)
  library(kernlab)
}

# Load the data from text file
nonheaderData <- read_tsv("credit_card_data.txt")
withHeaderData <- read_tsv("credit_card_data-headers.txt")
# view data
# head(withHeaderData)
# view columns in data frame
# glimpse(withHeaderData)

# Transfer to matrix
dataMatrix<-data.matrix(withHeaderData)
# To call ksvm. Vanilladot is a simple linear kernel
model <- ksvm(dataMatrix[,1:10],dataMatrix[,11],type= "C-svc", kernel= "vanilladot",C=100,scaled=TRUE)

# calculate a1...am
a <- colSums(model@xmatrix[[1]] * model@coef[[1]])
a
#a <- colSums(dataMatrix[model@SVindex,1:10] * model@coef[[1]])

# Calculate a0
a0 <- -model@b
a0

# see what the model predicts
pred <- predict(model,dataMatrix[,1:10])
pred

# see what fraction of the model’s predictions match the actual classification
sum(pred == dataMatrix[,11]) / nrow(dataMatrix)

# Q2.2.2 try other (nonlinear) kernels
model2 <- ksvm(dataMatrix[,1:10],dataMatrix[,11],type= "C-svc", kernel= "anovadot",C=100,scaled=TRUE)
model2
a2 = colSums(model2@xmatrix[[1]] * model2@coef[[1]])
a02 <- -model2@b
pred2 <- predict(model2,dataMatrix[,1:10])
sum(pred2 == dataMatrix[,11]) / nrow(dataMatrix)

# kernel= "rbfdot" has the lowest training error
model3 <- ksvm(dataMatrix[,1:10],dataMatrix[,11],type= "C-svc", kernel= "rbfdot",C=100,scaled=TRUE)
model3
a3 = colSums(model3@xmatrix[[1]] * model3@coef[[1]])
a03 <- -model3@b
pred3 <- predict(model3,dataMatrix[,1:10])
sum(pred3 == dataMatrix[,11]) / nrow(dataMatrix)

# Install kknn package - Weighted K-nearest neighbors package
if (!require("kknn")) {
  install.packages("kknn", dependencies = TRUE)
}

# Q2.2.3 Loading kknn
library("kknn")
# Set seed for reproductivity
set.seed(1)
# Load credit card data with header from local text
creditCardData <- read_tsv("credit_card_data-headers.txt")
# view and look for preper formula
head(creditCardData, 10)

# function to check 
check_accuracy = function(X){
  # predictions: start with a vector of all zeros
  predicted <- rep(0,(nrow(creditCardData))) 
  
  # for each row, estimate its response based on the other rows
  for (i in 1:nrow(creditCardData)){
    
    # data[-i] means is all the data except for the ith data point, meaning we remove row i of the data when finding nearest neighbors
    model <- kknn(R1~.,creditCardData[-i,],creditCardData[i,],k=X,scale=TRUE) # Don’t forget to scale the data
    
    # split predictions and round up/down to either predition > 0.5 or < 0.5
    predicted[i] <- as.integer(fitted(model)+0.5) # round off to 0 or 1
  }
  
  # calculate fraction of correct predictions
  accuracy <- sum(predicted == creditCardData[,11]) / nrow(creditCardData)
  return(accuracy)
}

# replicates 0 for 40 times to setup a vector
accurracyResult=rep(0,40) 
for (X in 1:40){
  accurracyResult[X] = check_accuracy(X) # test knn with X neighbors
}

# view the accurracy result
# plot(accurracyResult, main ="K-Nearest-Neighbors", xlab="Index", ylab="Accurracy")
k <- which.max(accurracyResult)
k

# Q3.1 use the ksvm or kknn function to find a good classifier
# a) using cross-validation - K numbers is 5, meaning 5-fold cross validation
library(caret)
trControl <- trainControl(method="cv", number=5)
modelcv <- train(R1~., 
             method="knn", 
             tuneGrid=expand.grid(k = 1:10),
             trControl=trControl,
             data=creditCardData)
modelcv

# b) splitting the data into training,validation,and test datasets
d.rows = nrow(creditCardData) 
# Randomly selecting (1/3)rd rows among 654 rows => see solution code snipptes
d.sample = sample(1:d.rows, size = round(d.rows/3), replace = FALSE)
# Training data selected by excluding the 1/3rd sample
d.train = creditCardData[-d.sample,]
# Test data seleted the 2/3rd sample
d.test = creditCardData[d.sample,]
#Training of kknn method via leave-one-out (train.kknn) cross validation to find the optimal value of 'k'
xval=train.kknn(R1 ~ ., data = d.train, kmax = 100, kernel = c("optimal","rectangular", "inv", "gaussian", "triangular"), scale = TRUE) 
xval

# see what the model predicts
pred<-predict(xval, d.test)
pred_bin<-round(pred)
pred_accuracy<-table(pred_bin,d.test$R1)
pred_accuracy

sum(pred_bin==d.test$R1)/length(d.test$R1)

# Try splitting the data into training, validation dataset using same sample data
n <- dim(creditCardData[1]) # number of observation
n_tr <- floor(n * .7)
# randomly select n_tr numbers, without replacement, from 1...n
tr_indices <- sample(x=1:n, size=n_tr, replace=FALSE) #sample(1:d.rows, size = round(d.rows/3), replace = FALSE)
# Trainning : Validation 70/30
d2.train <- creditCardData[tr_indices, ]
d2.validation <- creditCardData[-tr_indices, ]

# KNN model from caret package
#xval2 <- knn3(R1 ~ ., data =d2.train, k = 5)
xval2=train.kknn(R1 ~ ., data=d2.train, kmax = 100, kernel = c("optimal","rectangular", "inv", "gaussian", "triangular"), scale = TRUE) 
xval2
# see what the model predicts
pred2<-predict(xval2, d2.validation)
pred_bin2<-round(pred2)
pred_accuracy2<-table(pred_bin2,d2.validation$R1)
pred_accuracy2

sum(pred_bin2==d2.validation$R1)/length(d2.validation$R1)
