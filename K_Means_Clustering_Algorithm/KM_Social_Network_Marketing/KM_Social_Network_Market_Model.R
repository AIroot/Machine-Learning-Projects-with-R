# Reference for data source 
# Lantz, B. (2013).Finding Groups of Data–Clustering with k-means, In Ashwin Nair & James Jones (Eds.), Machine Learning with R(pp. 285-310). Packt Publishing Ltd.

# K-Means clustering 

# # Collecting data
SNSData <- read.csv("SNSData.csv", header= TRUE)

# Explore the data

str(SNSData)
table(SNSData$gender, useNA = "ifany")


# Identify missing values using graphical view. See the Rplot.pdf and red colour stripes indicate the missing values.  
# library(Amelia)
# missmap(SNSData, main="Missing Data - SNS", col=c("red","grey"), legend=FALSE)



# Data preparation - imputing 
# Use age range: If the age is at least 13 and less than 20 years
SNSData$age <- ifelse(SNSData$age >= 13 & SNSData$age < 20, SNSData$age, NA)
summary(SNSData$age)

# remove the missing values before calculating mean
mean(SNSData$age, na.rm = TRUE)

# computes statistics for subgroups of data
aggregate(data = SNSData, age ~ gradyear, mean, na.rm = TRUE)

# get groups of mean as a vector form
ave_age <- ave(SNSData$age, SNSData$gradyear, FUN = function(x) mean(x, na.rm = TRUE))
length(ave_age)

SNSData$age <- ifelse(is.na(SNSData$age), ave_age, SNSData$age)
summary(SNSData$age)



