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

#Data Visualization(Use RStudio or web-browser to view following plots)
# Histogram for wine quality level
histo <- plot_ly(data = WineData, x =~quality, type = "histogram")
histo

# Box plots
WineDataTemp <- WineData
WineDataTemp$qualityLevels <- ifelse(WineDataTemp$quality == 3, "Lev_Three", ifelse(WineDataTemp$quality == 4, "Lev_Four", ifelse(WineDataTemp$quality == 5, "Lev_Five", ifelse(WineDataTemp$quality == 6, "Lev_Six", ifelse(WineDataTemp$quality == 7, "Lev_Seven", ifelse(WineDataTemp$quality == 8, "Lev_Eight", "Lev_Nine"))) )))

# Alcohol Content
Box_plot <- plot_ly(data = WineDataTemp, x = ~qualityLevels, y = ~alcohol, color = ~qualityLevels, type = "box", colors = "Dark2")
Box_plot

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
# Build the model S