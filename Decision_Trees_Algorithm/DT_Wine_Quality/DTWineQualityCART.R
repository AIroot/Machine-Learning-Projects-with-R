# Reference for data source (
# @misc{Lichman:2013 ,
# author = "M. Lichman",
# year = "2013",
# title = "{UCI} Machine Learning Repository",
# url = "http://archive.ics.uci.edu/ml",
# institution = "University of California, Irvine, School of Information and Computer Sciences" })

# Decision Trees
# Source of Data Set:- UCI Repository - Wine Quality Data(https://archive.ics.uci.edu/ml/datasets/wine+quality)

# required libraries
# # The rpart package can be installed via the install.packages("rpart") and 
# # loaded with the library(rpart) command.
library(rpart) #recursive and partitioning trees

# # The plotly package can be installed via the install.packages("plotly") and 
# # loaded with the library(plotly) command.
library(plotly) #data visualization

# # The rpart.plot package can be installed via the install.packages("rpart.plot") and 
# # loaded with the library(rpart.plot) command.
library(rpart.plot)

# # The rattle package can be installed via the install.packages("rattle") and 
# # loaded with the library(rattle) command.
library(rattle)

# # The RColorBrewer package can be installed via the install.packages("RColorBrewer") and 
# # loaded with the library(RColorBrewer) command.
library(RColorBrewer)


# # The RWeka package can be installed via the install.packages("RWeka") and 
# # loaded with the library(RWeka) command.
library(RWeka)


# Exploring and preparing the data
# Step 2: Exploring and preparing the data
# Read the csv file into a data frame titled WineData.
WineData <- read.table("winequality-red.csv", sep=";", header=TRUE)



WineData$quality <- ifelse(WineData$quality < 5, 'bad', ifelse(WineData$quality > 6,'good','normal'))
WineData$quality <- as.factor(WineData$quality)
str(WineData$quality)

# Data preparation - creating random training and test datasets
# Create random sample
# Divide the data into a training set and a test set randomly with ratio 80:20

set.seed(123)
train_sample <- sample(nrow(WineData), 0.8 * nrow(WineData))
WineData_train <- WineData[train_sample, ]
WineData_test <- WineData[-train_sample, ]
# Check whether data set fairly even split
prop.table(table(WineData_train$quality))

prop.table(table(WineData_test$quality))


# Train model - Regression Tree
# Build the model with recursive partitioning trees
WineData_model <- rpart(quality ~. , data = WineData_train)

summary(WineData_model)

# plot the cost complexity parameters
plotcp(WineData_model)


# Visualizing Decision Trees 
fancyRpartPlot(WineData_model)


# Visualize the classification tree
plot(WineData_model, uniform=TRUE, branch=0.6, margin=0.1)
text(WineData_model, all=TRUE, use.n = TRUE)

# Model Evaluation using test data
WineData_predict <- predict(WineData_model, WineData_test, type="class")

# Use the table function to generate a classification table for testing dataset
table(WineData_test$quality, WineData_predict)


# Accuracy : Measures of performance
library(caret)
confusionMatrix(table(WineData_predict, WineData_test$quality))

# Pruning a recursive partitioning tree
# Find minimum cross-validation error of the classification tree model
min(WineData_model$cptable[,"xerror"])
# Locate the record with the minimum cross-validation errors
which.min(WineData_model$cptable[,"xerror"])

# Get the cost complexity parameter of the record with the minimum cross-validation errors
WineData_model_CP = WineData_model$cptable[5, "CP"]
WineData_model_CP

# Prune the tree by setting the cp parameter to the CP value of the record with minimum cross-validation errors
prune_tree = prune(WineData_model, cp=WineData_model_CP)

# Visualize the classification tree by using the plot and text function
plot(prune_tree, margin=0.1)
text(prune_tree, all=TRUE, use.n=TRUE)

# Generate a classification table based on the pruned classification tree model
predictions = predict(prune_tree, WineData_test, type="class")
table(WineData_test$quality, predictions)


# Generate confusion matrix
confusionMatrix(table(predictions, WineData_test$quality))





















# # Model Improvement using M5P from RWeka 
# # Build Model 
# WineData_model_M5P <- M5P(quality ~. , data= WineData_train)

# # Model Evaluation using test data
# WineData_predict_M5P <- predict(WineData_model_M5P, WineData_test)

# WineData_model_M5P

# MAE(WineData_test$quality, WineData_predict_M5P)





