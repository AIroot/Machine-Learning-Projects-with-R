# Naive Bayes
# Source of data set : UCI Repository - http://archive.ics.uci.edu/ml/datasets/SMS+Spam+Collection

# Exploring and preparing the data 
# Read the CSV file into a data frame titled sms_data
sms_data <- read.csv("sms_spam.csv", header = TRUE)

# # Assign attribute information
colnames(sms_data) <- c("type", "text")

# Loading in the data, keeping only the first 2 columns
sms_data <- sms_data[,1:2]
# Displays description of each variable 
str(sms_data)
head(sms_data)
table(sms_data$type)
