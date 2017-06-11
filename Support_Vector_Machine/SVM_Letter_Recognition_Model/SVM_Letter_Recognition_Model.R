# Reference for data source (
# @misc{Lichman:2013 ,
# author = "M. Lichman",
# year = "2013",
# title = "{UCI} Machine Learning Repository",
# url = "http://archive.ics.uci.edu/ml",
# institution = "University of California, Irvine, School of Information and Computer Sciences" })

# Support Vector Machine
# Source of Data Set:-  UCI Repository- Letter Recognition Data (https://archive.ics.uci.edu/ml/datasets/letter+recognition)

# # Collecting data
# # Download data from UCI repo
# LetterDataUrl <- "https://archive.ics.uci.edu/ml/machine-learning-databases/letter-recognition/letter-recognition.data"

# # Read the url html file into a data frame titled LetterData.
# LetterData <- read.csv(LetterDataUrl, header=FALSE)

# # Assigning attributes information 
# colnames(LetterData) <- c("lettr", "x_box", "y_box", "width", "high", "onpix", "x_bar", "y_bar", "x2bar", "y2bar", "xybar", "x2ybr", "xy2br", "x_ege", "xegvy", "y_ege", "yegvx")

# # Write a CSV file from LetterData
# LetterData <- write.csv(LetterData, file = "LetterData.csv", row.names = FALSE)

LetterData <- read.csv("LetterData.csv", header= TRUE)

str(LetterData)

# Data preparation - creating random training and test datasets
# Create random sample
# Divide the data into a training set and a test set randomly with ratio 75:25

set.seed(123)
train_sample <- sample(nrow(LetterData), 0.8 * nrow(LetterData))
LetterData_train <- LetterData[train_sample, ]
LetterData_test <- LetterData[-train_sample, ]