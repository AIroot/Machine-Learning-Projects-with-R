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
WineData$quality <- ifelse(WineData$quality == 3, "Lev_Three", ifelse(WineData$quality == 4, "Lev_Four", ifelse(WineData$quality == 5, "Lev_Five", ifelse(WineData$quality == 6, "Lev_Six", ifelse(WineData$quality == 7, "Lev_Seven", ifelse(WineData$quality == 8, "Lev_Eight", "Lev_Nine"))) )))
WineData$quality <- as.factor(WineData$quality)
str(WineData)
head(WineData)
table(WineData$quality)

## Training random forests
library(randomForest)
set.seed(300)
rf <- randomForest(quality ~ ., data= WineData)
rf

# Evaluating random forest performance