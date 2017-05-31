# Naive Bayes
# Source of data set : UCI Repository - http://archive.ics.uci.edu/ml/datasets/SMS+Spam+Collection
# Reference for data source (
# @misc{Lichman:2013 ,
# author = "M. Lichman",
# year = "2013",
# title = "{UCI} Machine Learning Repository",
# url = "http://archive.ics.uci.edu/ml",
# institution = "University of California, Irvine, School of Information and Computer Sciences" })

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


# Data preparation - splitting text documents into words(Tokenization)

# Create a data structure called a Document Term Matrix(DTM)
sms_dtm <- DocumentTermMatrix(sms_cleaned_corpus)
sms_dtm
# Divide the data into a training set and a test set with ratio 75:25
# The SMS messages are sorted in a random order.

sms_dtm_train <- sms_dtm[1:4181, ]
sms_dtm_test <- sms_dtm[4182:5574, ]

# Create labels that are not stored in the DTM
sms_train_lables <- sms_data[1:4181, ]$type
sms_test_lables <- sms_data[4182:5574, ]$type

# Compare the proportion of spam in the training and test data
prop.table(table(sms_train_lables))
prop.table(table(sms_test_lables))

# Visualizing text data using word clouds
# The wordcloud package can be installed via the install.packages("wordcloud") and 
# loaded with the library(wordcloud) command. 
library(wordcloud)

# Create wordcloud from a tm corpus object
pal <-brewer.pal(8,"Dark2")
wordcloud(sms_cleaned_corpus, min.freq=40, random.order = FALSE, colors=pal)

# Create wordcloud for spam and ham data subsets
spam <- subset(sms_data, type == "spam") 

wordcloud(spam$text, max.word = 40, scale = c(4, 0.8), colors=pal)

ham <- subset(sms_data, type == "ham")
wordcloud(ham$text, max.word = 40, scale = c(4, 0.8), colors=pal)


# Data preparation - Creating indicator features for frequent words
sms_frequent_words <- findFreqTerms(sms_dtm_train, 5)
str(sms_frequent_words)

sms_dtm_freq_train<- sms_dtm_train[ , sms_frequent_words]
sms_dtm_freq_test <- sms_dtm_test[ , sms_frequent_words]

# print the most frequent words in each class. 

sms_corpus_ham <- VCorpus(VectorSource(ham$text))
sms_corpus_spam <- VCorpus(VectorSource(spam$text))

sms_dtm_ham <- DocumentTermMatrix(sms_corpus_ham, control = list(tolower = TRUE,removeNumbers = TRUE,stopwords = TRUE,removePunctuation = TRUE,stemming = TRUE)) 
sms_dtm_spam <- DocumentTermMatrix(sms_corpus_spam, control = list(tolower = TRUE,removeNumbers = TRUE,stopwords = TRUE,removePunctuation = TRUE,stemming = TRUE)) 

sms_dtm_ham_frequent_words <- findFreqTerms(sms_dtm_ham, lowfreq= 0, highfreq = Inf)
head(sms_dtm_ham_frequent_words)
tail(sms_dtm_ham_frequent_words)

sms_dtm_spam_frequent_words <- findFreqTerms(sms_dtm_spam, lowfreq= 0, highfreq = Inf)
head(sms_dtm_spam_frequent_words)
tail(sms_dtm_spam_frequent_words)


# The following defines a convert_counts() function to convert counts to
# Yes / No strings:

convert_counts <- function(x) {
	x <- ifelse(x > 0, "Yes", "No")
}

# Apply above function to train and test data sets. 
sms_train <- apply(sms_dtm_freq_train, MARGIN = 2,convert_counts)
sms_test <- apply(sms_dtm_freq_test, MARGIN = 2,convert_counts)

# Training a model usig Naive Bayes
library(e1071)
sms_classifier <- naiveBayes(sms_train, sms_train_lables)


# Evaluating model 
sms_test_pred <- predict(sms_classifier, sms_test)
library(gmodels)
CrossTable(sms_test_pred, sms_test_lables,prop.chisq = FALSE, prop.t = FALSE,dnn = c('predicted', 'actual'))



# Accuracy : Measures of performance
library(caret)
confusionMatrix(sms_test_pred, sms_test_lables, positive = "spam")

# Improving model performance
# Adding Laplace estimator 

new_sms_classifier <- naiveBayes(sms_train, sms_train_lables, laplace = 1)
new_sms_classifier_pred <- predict(new_sms_classifier, sms_test)

# Compare the predicted classes to the actual classifications using cross table
CrossTable(new_sms_classifier_pred, sms_test_lables, prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE, dnn = c('Predicted', 'Actual'))
