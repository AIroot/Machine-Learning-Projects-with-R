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

# Data preparation- dummy coding 
SNSData$female <- ifelse(SNSData$gender == "F" &  !is.na(SNSData$gender), 1, 0)
SNSData$no_gender <- ifelse(is.na(SNSData$gender), 1, 0)

table(SNSData$gender, useNA = "ifany")

table(SNSData$female, useNA = "ifany")

table(SNSData$no_gender, useNA = "ifany")

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

# Train the model
# Use R default stats package
library(stats)

# The kmeans() function requires a data frame containing only numeric data
# and a parameter specifying the desired number of clusters.

# Considering only the 36 features that represent the number of times various
# interest appeard on the SNSData

interests <- SNSData[5:40]

# Avoid features dominate 
# Use normalize or z-score standardize
interests_z <- as.data.frame(lapply(interests, scale))

# Apply kmeans() functions
set.seed(2345)

SNSData_Clusters <- kmeans(interests_z, 5)


# Evaluating model performance
# The basic ways to evaluate the cluster is size of cluster. 
SNSData_Clusters$size 

# Check the coordinates of the cluster centroids 
SNSData_Clusters$centers

# Improving model
# Add clusters to the data frame
SNSData$cluster <- SNSData_Clusters$cluster

# Check how the cluster relates to induvidual characteristics
SNSData[1:5, c("cluster", "gender", "age", "friends")] 

# Use aggregate() functions
aggregate(data = SNSData, age ~ cluster, mean)

# use aggregate() to check cluster and feamales
aggregate(data = SNSData, female ~ cluster, mean) 
