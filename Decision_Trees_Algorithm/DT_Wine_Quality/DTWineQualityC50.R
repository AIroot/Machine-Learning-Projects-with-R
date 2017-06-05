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

head(WineData)
table(WineData$quality)

# Creating a categorical variable for wine quality (bad,good,normal)
# Less than 6 quality level == bad
# 6 quality level == normal
# Greater than 6, quality level == good
WineData$quality <- ifelse(WineData$quality == 3, "Lev_Three", ifelse(WineData$quality == 4, "Lev_Four", ifelse(WineData$quality == 5, "Lev_Five", ifelse(WineData$quality == 6, "Lev_Six", ifelse(WineData$quality == 7, "Lev_Seven", ifelse(WineData$quality == 8, "Lev_Eight", "Lev_Nine"))) )))
WineData$quality <- as.factor(WineData$quality)
# WineData$taste <- ifelse(WineData$quality < 6, 'bad', 'good')
# WineData$taste[WineData$quality == 6] <- 'normal'
# WineData$taste <- as.factor(WineData$taste)
# table(WineData$taste)
str(WineData)

#Data Visualization(Use RStudio or web-browser to view following plots)
# Histogram for wine quality level
# histo <- plot_ly(data = WineData, x =~quality, type = "histogram")
# histo

# # Box plots
# WineDataTemp <- WineData
# # WineDataTemp$qualityLevels <- ifelse(WineDataTemp$quality == 3, "Lev_Three", ifelse(WineDataTemp$quality == 4, "Lev_Four", ifelse(WineDataTemp$quality == 5, "Lev_Five", ifelse(WineDataTemp$quality == 6, "Lev_Six", ifelse(WineDataTemp$quality == 7, "Lev_Seven", ifelse(WineDataTemp$quality == 8, "Lev_Eight", "Lev_Nine"))) )))

# # Alcohol Content
# Box_plot <- plot_ly(data = WineDataTemp, x = ~qualityLevels, y = ~alcohol, color = ~qualityLevels, type = "box", colors = "Dark2")
# Box_plot


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

# Train model
# Regression Trees

# # C5.0
# fit.c50 <- train(quality.level ~ alcohol + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + fixed.acidity, data=WineData_train, method="C5.0")
# confusionMatrix(fit.c50)
# # # Training a model on the data
# # # The C5.0 package can be installed via the install.packages("C50") and 
# # # loaded with the library(C50) command.
library(C50)



WineData_model <- C5.0(WineData_train[-12], WineData_train$quality)
WineData_model
# See the tree's decisions 
summary(WineData_model)



# Evaluating model performance
WineData_predict <- predict(WineData_model, WineData_test)

# Various R Programming Tools for Model Fitting
library(gmodels)

# create a cross tabulation indicating the agreement between the two vectors.
# Specifying prop.chisq = FALSE will remove the unnecessary chi-square
# values from the output.
# Setting the prop.c and prop.r parameters to FALSE removes the column and row percentages
# from the table. The remaining percentage ( prop.t ) indicates the proportion of
# records in the cell out of the total number of records:
CrossTable(WineData_test$quality, WineData_predict, prop.chisq = FALSE, prop.c= FALSE, prop.r = FALSE, dnn = c('Actual quality', 'Predicted quality'))

# Accuracy : Measures of performance
library(caret)
confusionMatrix(WineData_test$quality, WineData_predict,)

# Improving model performance
# Boosting the accuracy of decision trees
# Add additional trials parameter indicating the number of
# separate decision trees to use in the boosted team.
WineData_boost10 <- C5.0(WineData_train[-12], WineData_train$quality, trials = 10)
WineData_boost10

# See all 10 trees 
summary(WineData_boost10)

WineData_boost10_predict <- predict(WineData_boost10, WineData_test)
CrossTable(WineData_test$quality, WineData_boost10_predict, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('Actual Class', 'Predicted Class'))
confusionMatrix(WineData_test$quality, WineData_boost10_predict, positive = "good")


# # Making mistakes more costlier than others
# Matrix_dimensions <- list(c("bad", "good"), c("bad", "good"))
# names(Matrix_dimensions) <- c("Predicted", "Actual")

# Matrix_dimensions

# error_cost <- matrix(c(0,1,4,0), nrow = 2, dimnames = Matrix_dimensions)

# error_cost

# CreditData_cost <- C5.0(CreditData_train[-21], CreditData_train$class, costs = error_cost)
# CreditData_cost_predict <- predict(CreditData_cost, CreditData_test)
# CrossTable(CreditData_test$class, CreditData_cost_predict, prop.chisq=FALSE, prop.c = FALSE, prop.r=FALSE, dnn= c('Actual', 'Predicted'))

## Training random forests
library(randomForest)
set.seed(300)
rf <- randomForest(quality ~ ., data= WineData)
rf

# Evaluating random forest performance
