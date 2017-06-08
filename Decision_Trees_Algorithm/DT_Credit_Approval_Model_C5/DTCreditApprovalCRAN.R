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


# Step 01: Collecting data 
# Download data from UCI repository
CreditDataUrl <- "https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data"

# Read the url html file into a data frame titled CreditData.
CreditData <- read.table(CreditDataUrl)

# Assging attribute information
# The target function column name is class
colnames(CreditData) <- c("chk_status", "mth_duration", "credit_history", "purpose", "credit_amount", "saving", "employ_time", "pct_dpi", "status_gender", "other_debts", "residency_time", "property", "age", "other_installments", "housing", "existing_credits", "job", "dependents_num", "phone", "foreign", "class")

# Write a CSV file from CreditData
Credit_Data <- write.csv(CreditData, file = "CreditData.csv", row.names = FALSE)

# Exploring and preparing the data
# Step 2: Exploring and preparing the data
# Read the csv file into a data frame titled CreditData.
CreditData <- read.csv("CreditData.csv", header=TRUE)

# Class columns convert into facator
CreditData$class <- ifelse(CreditData$class==1, "good","bad")
CreditData$class = as.factor(CreditData$class)

# Displays description of each variable

head(CreditData)


# Data preparation - creating random training and test datasets
# Create random sample
# Divide the data into a training set and a test set randomly with ratio 80:20

set.seed(123)
train_sample <- sample(nrow(CreditData), 0.9 * nrow(CreditData))
CreditData_train <- CreditData[train_sample, ]
CreditData_test <- CreditData[-train_sample, ]

# Check whether data set fairly even split
prop.table(table(CreditData_train$class))

prop.table(table(CreditData_test$class))



# Train model - Regression Tree
# Build the model with recursive partitioning trees
CreditData_model <- rpart(class ~. , data = CreditData_train)

summary(CreditData_model)

# plot the cost complexity parameters
plotcp(CreditData_model)


# Visualizing Decision Trees 
fancyRpartPlot(CreditData_model)


# Visualize the classification tree
plot(CreditData_model, uniform=TRUE, branch=0.6, margin=0.1)
text(CreditData_model, all=TRUE, use.n = TRUE)

# Model Evaluation using test data
CreditData_predict <- predict(CreditData_model, CreditData_test, type="class")

# Use the table function to generate a classification table for testing dataset
table(CreditData_test$class, CreditData_predict)


# Accuracy : Measures of performance
library(caret)
confusionMatrix(table(CreditData_predict, CreditData_test$class))

# Pruning a recursive partitioning tree
# Find minimum cross-validation error of the classification tree model
min(CreditData_model$cptable[,"xerror"])
# Locate the record with the minimum cross-validation errors
value = which.min(CreditData_model$cptable[,"xerror"])

# Get the cost complexity parameter of the record with the minimum cross-validation errors
CreditData_model_CP = CreditData_model$cptable[value, "CP"]
CreditData_model_CP

# Prune the tree by setting the cp parameter to the CP value of the record with minimum cross-validation errors
prune_tree = prune(CreditData_model, cp=CreditData_model_CP)

# Visualize the classification tree by using the plot and text function
plot(prune_tree, margin=0.1)
text(prune_tree, all=TRUE, use.n=TRUE)

# Generate a classification table based on the pruned classification tree model
predictions = predict(prune_tree, CreditData_test, type="class")
table(CreditData_test$class, predictions)


# Generate confusion matrix
confusionMatrix(table(predictions, CreditData_test$class))





















# # Model Improvement using M5P from RWeka 
# # Build Model 
# WineData_model_M5P <- M5P(quality ~. , data= WineData_train)

# # Model Evaluation using test data
# WineData_predict_M5P <- predict(WineData_model_M5P, WineData_test)

# WineData_model_M5P

# MAE(WineData_test$quality, WineData_predict_M5P)





