# Reference for data source (
# @misc{Lichman:2013 ,
# author = "M. Lichman",
# year = "2013",
# title = "{UCI} Machine Learning Repository",
# url = "http://archive.ics.uci.edu/ml",
# institution = "University of California, Irvine, School of Information and Computer Sciences" })

# Neural Networks
# Source of Data Set:- UCI Repository - (https://archive.ics.uci.edu/ml/datasets/Concrete+Compressive+Strength)
# Exploring and preparing the data
# Step 2: Exploring and preparing the data
# Read the csv file into a data frame titled ConcreteData.
library(xlsx)
ConcreteData <- read.xlsx("Concrete_Data.xls",1)
colnames(ConcreteData) <- c("Cement_comp", "BF_Slag", "Fly_Ash", "Water", "Superplasticizer", "Coarse_Aggregate", "Fine_Aggregate", "Age_Day", "Compressive_Strength")
str(ConcreteData)

# Transformation - normalizing numeric data

normalize <- function(x) {
	return ((x - min(x)) / (max(x) - min(x)))
}

# Apply normalize function to each list elememt. 
ConcreteData_n <- as.data.frame(lapply(ConcreteData, normalize))

summary(ConcreteData_n$Compressive_Strength)
summary(ConcreteData$Compressive_Strength)

# Data preparation - creating random training and test datasets
# Create random sample
# Divide the data into a training set and a test set randomly with ratio 75:25

set.seed(123)
train_sample <- sample(nrow(ConcreteData_n), 0.75 * nrow(ConcreteData_n))
ConcreteData_train <- ConcreteData_n[train_sample, ]
ConcreteData_test <- ConcreteData_n[-train_sample, ]

# Train model

# # NeuralNet 
# # # Training a model on the data
# # The neuralnet package can be installed via the install.packages("neuralnet") and 
# # loaded with the library(neuralnet) command.
library(neuralnet)
# The model is used to train simplest multilayer feedforward network with only a single hidden node
Concrete_Data_model <- neuralnet(Compressive_Strength ~ Cement_comp + BF_Slag + Fly_Ash + Water + Superplasticizer + Coarse_Aggregate + Fine_Aggregate + Age_Day, data = ConcreteData_train)
plot(Concrete_Data_model)