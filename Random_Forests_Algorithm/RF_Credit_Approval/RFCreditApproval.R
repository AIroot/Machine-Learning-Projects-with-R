# Reference for data source (
# @misc{Lichman:2013 ,
# author = "M. Lichman",
# year = "2013",
# title = "{UCI} Machine Learning Repository",
# url = "http://archive.ics.uci.edu/ml",
# institution = "University of California, Irvine, School of Information and Computer Sciences" })

# Decision Trees
# Source of Data Set:- UCI Repository - German Credit Data(https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data)

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
# Divide the data into a training set and a test set randomly with ratio 90:1

set.seed(123)
train_sample <- sample(nrow(CreditData), 0.9 * nrow(CreditData))
CreditData_train <- CreditData[train_sample, ]
CreditData_test <- CreditData[-train_sample, ]

# Check whether data set fairly even split
prop.table(table(CreditData_train$class))

prop.table(table(CreditData_test$class))
# Training random forests
library(randomForest)
# set.seed(300)
rf_model <- randomForest(CreditData_train[-21], CreditData_train$class)
rf_model

# Evaluating random forest performance
# Making predictions
rf_predict <- predict(rf_model, CreditData_test)

# Various R Programming Tools for Model Fitting
library(gmodels)

# create a cross tabulation indicating the agreement between the two vectors.
# Specifying prop.chisq = FALSE will remove the unnecessary chi-square
# values from the output.
# Setting the prop.c and prop.r parameters to FALSE removes the column and row percentages
# from the table. The remaining percentage ( prop.t ) indicates the proportion of
# records in the cell out of the total number of records:
CrossTable(CreditData_test$class, rf_predict, prop.chisq = FALSE, prop.c= FALSE, prop.r = FALSE, dnn = c('Actual', 'Predicted'))


# Accuracy : Measures of performance
library(caret)
confusionMatrix(CreditData_test$class, rf_predict)


# Evaluating random forest performance
# use training control option and use repeated 10-fold cross-validation or 10-fold CV repeated 10 times.
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

# Set up the tuning grid for the random forest 
grid_rf <- expand.grid(.mtry = c(2, 4, 8, 16))

# Use kappa metric to select the best model
set.seed(300)
m_rf <- train(class ~ ., data = CreditData, method = "rf", metric = "Kappa", trControl= ctrl, tuneGrid = grid_rf)

# Compare the boosted tree using 10,20,30 and 40 iterations

grid_c50 <- expand.grid(.model = "tree", .trials = c(10, 20, 30, 40),.winnow = "FALSE")

set.seed(300)
m_c50 <- train(class ~ ., data = CreditData, method = "C5.0", metric = "Kappa", trControl= ctrl, tuneGrid = grid_c50)

# Compare the two approaches
m_rf

m_c50 
