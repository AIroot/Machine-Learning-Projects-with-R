# Reference for data source (
# @misc{Lichman:2013 ,
# author = "M. Lichman",
# year = "2013",
# title = "{UCI} Machine Learning Repository",
# url = "http://archive.ics.uci.edu/ml",
# institution = "University of California, Irvine, School of Information and Computer Sciences" })

# Decision Trees
# Source of Data Set:- UCI Repository - Wine Quality Data(https://archive.ics.uci.edu/ml/datasets/wine+quality)

# Exploring and preparing the data
# Step 2: Exploring and preparing the data
# Read the csv file into a data frame titled WineData.
WineData <- read.table("winequality-red.csv", sep=";", header=TRUE)

# Creating a categorical variable for wine quality
# WineData$quality <- ifelse(WineData$quality == 3, "Lev_Three", ifelse(WineData$quality == 4, "Lev_Four", ifelse(WineData$quality == 5, "Lev_Five", ifelse(WineData$quality == 6, "Lev_Six", ifelse(WineData$quality == 7, "Lev_Seven", ifelse(WineData$quality == 8, "Lev_Eight", "Lev_Nine"))) )))
# WineData$quality <- as.factor(WineData$quality)


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
## Training random forests
library(randomForest)
# set.seed(300)
rf_model <- randomForest(WineData_train[-12], WineData_train$quality)
rf_model

# Evaluating random forest performance
# Making predictions
rf_predict <- predict(rf_model, WineData_test)

# Various R Programming Tools for Model Fitting
library(gmodels)

# create a cross tabulation indicating the agreement between the two vectors.
# Specifying prop.chisq = FALSE will remove the unnecessary chi-square
# values from the output.
# Setting the prop.c and prop.r parameters to FALSE removes the column and row percentages
# from the table. The remaining percentage ( prop.t ) indicates the proportion of
# records in the cell out of the total number of records:
CrossTable(WineData_test$quality, rf_predict, prop.chisq = FALSE, prop.c= FALSE, prop.r = FALSE, dnn = c('Actual quality', 'Predicted quality'))

# Accuracy : Measures of performance
library(caret)
confusionMatrix(WineData_test$quality, rf_predict)


# Evaluating random forest performance
# use training control option and use repeated 10-fold cross-validation or 10-fold CV repeated 10 times.
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

# Set up the tuning grid for the random forest 
grid_rf <- expand.grid(.mtry = c(2, 4, 8, 16))

# Use kappa metric to select the best model
set.seed(300)
m_rf <- train(quality ~ ., data = WineData, method = "rf", metric = "Kappa", trControl= ctrl, tuneGrid = grid_rf)

# Compare the boosted tree using 10,20,30 and 40 iterations

grid_c50 <- expand.grid(.model = "tree", .trials = c(10, 20, 30, 40),.winnow = "FALSE")

set.seed(300)
m_c50 <- train(quality ~ ., data = WineData, method = "C5.0", metric = "Kappa", trControl= ctrl, tuneGrid = grid_c50)

# Compare the two approaches
m_rf

m_c50 
