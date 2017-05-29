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
# corpus can use to collect text documents
# In order to create a corpus, VCorpus() is used which is in the tm package
# The VectorSource is reader function to create a source object from the existing sms_data$text
sms_corpus <- VCorpus(VectorSource(sms_data$text))
print(sms_corpus)

# View a summary of the first and second SMS messages in the corpus
inspect(sms_corpus[1:2])

# The as.character() is used to view actual message text
as.character(sms_corpus[[1]])

# The lapply() function is used to apply procedure to each element of an R data structure.
lapply(sms_corpus[1:2], as.character) 

# Text transformation  
# The tm_map() function provides a method to apply a transformation 
# to a tm corpus. 
# New transformation save the result in a new object called sms_cleaned_corpus

# Convert text into lowercase. Here used following functions:
# content_transformer(); tm wrapper function
# tolower(); lowercase transformation function
sms_cleaned_corpus <- tm_map(sms_corpus, content_transformer(tolower))

# Check the difference between sms_corpus and sms_cleaned_corpus

as.character(sms_corpus[[1]])
as.character(sms_cleaned_corpus[[1]])

# Remove numbers from SMS messages
sms_cleaned_corpus <- tm_map(sms_cleaned_corpus, removeNumbers)

# Remove filler words using stopwords() and removeWords() functions
sms_cleaned_corpus <- tm_map(sms_cleaned_corpus, removeWords, stopwords())

# Remove punctuation characters
sms_cleaned_corpus <- tm_map(sms_cleaned_corpus, removePunctuation)

# Reducing words to their root form using stemming. The tm package provides 
# stemming functionality via integration with the SnowballC packge.

# The SnowballC package can be installed via the install.packages("SnowballC") and 
# loaded with the library(SnowballC) command. 
library(SnowballC)

# Apply stemming 
sms_cleaned_corpus <- tm_map(sms_cleaned_corpus, stemDocument)

# Remove additional whitespace
sms_cleaned_corpus <- tm_map(sms_cleaned_corpus, stripWhitespace)


