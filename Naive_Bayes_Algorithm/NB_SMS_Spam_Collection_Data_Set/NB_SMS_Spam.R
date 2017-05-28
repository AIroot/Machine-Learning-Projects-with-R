# Naive Bayes
# Source of data set : UCI Repository - http://archive.ics.uci.edu/ml/datasets/SMS+Spam+Collection

# Exploring and preparing the data 
# Read the CSV file into a data frame titled sms_data
sms_data <- read.csv("SMSSpamCollection", sep="\t", header = FALSE, quote="", stringsAsFactors=FALSE)

# # Assign attribute information
colnames(sms_data) <- c("type", "text")

# The type element is currently a character vector. 
# Convert it into a factor.
sms_data$type <- factor(sms_data$type)

# Displays description of each variable 
str(sms_data)
head(sms_data)
table(sms_data$type)

# Data preparation - cleaning and standrdizing text data
# The tm package can be installed via the install.packages("tm") and 
# loaded with the library(tm) command. 
library(tm)
sms_corpus <- VCorpus(VectorSource(sms_data$text))
print(sms_corpus)
