# Reference for data source (
# @misc{Lichman:2013 ,
# author = "M. Lichman",
# year = "2013",
# title = "{UCI} Machine Learning Repository",
# url = "http://archive.ics.uci.edu/ml",
# institution = "University of California, Irvine, School of Information and Computer Sciences" })

# Decision Trees
# Source of Data Set:- UCI Repository - Wine Quality Data(https://archive.ics.uci.edu/ml/datasets/wine+quality)

# Exploring and preparing the data
# Step 2: Exploring and preparing the data
# Read the csv file into a data frame titled WineData.
WineData <- read.table("winequality-red.csv", sep=";", header=TRUE)

head(WineData)
table(WineData$quality)


# Identify missing values using graphical view. See the Rplot.pdf and red colour stripes indicate the missing values.  
library(Amelia)
missmap(WineData, main="Missing Data - Red Wine Quality", col=c("red","grey"), legend=FALSE)

# Data Visualization
# plot histogram of fixed acidity

library(ggplot2) 

ggplot(WineData, aes(x = fixed.acidity)) +
  geom_histogram(binwidth = 0.1) +
  scale_x_continuous(breaks = seq(4, 16, by = 1)) +
  ggtitle("Fixed Acidity distribution") +
  xlab("Fixed Acidity") +
  ylab("Count")

# plot histogram of Volatile Acidity
plot1 <- ggplot(WineData, aes(x = volatile.acidity)) +
  geom_histogram(binwidth = 0.02) +
  scale_x_continuous(breaks = seq(0, 1.6, by = 0.1)) +
  ggtitle("Volatile Acidity distribution") +
  xlab("Volatile Acidity") +
  ylab("Count")

plot2 <- ggplot(WineData, aes(x = volatile.acidity)) +
  geom_histogram(binwidth = 0.02) +
  scale_x_log10(breaks = seq(0, 1.6, by = 0.5)) +
  ggtitle("Volatile Acidity distribution") +
  xlab("log(Volatile Acidity)") +
  ylab("Count")

# gridExtra: Miscellaneous Functions for "Grid" Graphics. 
library(gridExtra)
grid.arrange(plot1, plot2)


# plot histogram of pH
p1 <- ggplot(WineData, aes(x = pH)) +
  geom_histogram(binwidth = 0.02) +
  ggtitle("pH distribution") +
  xlab("pH") +
  ylab("Count")

# plot histogram of Free SO2
p2 <- ggplot(WineData, aes(x = free.sulfur.dioxide)) +
  geom_histogram(binwidth = 1) +
  ggtitle("Free SO2 distribution") +
  xlab("Free SO2") +
  ylab("Count")

# plot histogram of Total SO2
p3 <- ggplot(WineData, aes(x = total.sulfur.dioxide)) +
  geom_histogram(binwidth = 3) +
  ggtitle("Total SO2 distribution") +
  xlab("Total SO2") +
  ylab("Count")

# plot histogram of Alcohol
p4 <- ggplot(WineData, aes(x = alcohol)) +
  geom_histogram(binwidth = 0.1) +
  ggtitle("Alcohol distribution") +
  xlab("Alcohol") +
  ylab("Count")


grid.arrange(p1, p2, p3, p4, ncol = 2)

# plot histogram of Quality
ggplot(WineData, aes(x = quality)) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks = seq(3, 8, by = 1)) +
  ggtitle("Quality Distributions") +
  xlab("Quality") +
  ylab("Count")

# Positive correlation of alcohol and quality
ggplot(WineData, aes(x = alcohol)) +
  geom_density(aes(fill = "red", color = "red")) +
  facet_wrap(~quality) +
  theme(legend.position = "none") +
  ggtitle("Alcohol VS Quality") +
  xlab("Alcohol") +
  ylab("Quality")


# Negative correlation of volatile acidity and quality
ggplot(WineData, aes(x = volatile.acidity)) +
  geom_density(aes(fill = "red", color = "red")) +
  facet_wrap(~quality) +
  theme(legend.position = "none") +
  ggtitle("Volatile Acidity VS Quality") +
  xlab("Volatile Acidity") +
  ylab("Quality")

# Positive correlation of Free SO~2~ and Total SO~2~

ggplot(WineData, aes(x = free.sulfur.dioxide, y = total.sulfur.dioxide)) +
  geom_jitter(alpha = 1/5) +
  ggtitle("Free S02 vs Total SO2") +
  xlab("Free SO2") +
  ylab("Total SO2")

# residual sugar and quality relationship

ggplot(WineData, aes(x = residual.sugar)) +
  geom_density(aes(fill = "red", color = "red")) +
  facet_wrap(~quality) +
  theme(legend.position = "none") +
  ggtitle("Residual Sugar VS Quality") +
  xlab("Residual Sugar") +
  ylab("Quality")

# Density and Alchol 
ggplot(WineData, aes(x = density, y = alcohol)) +
  geom_jitter(alpha = 1/2) +
  ggtitle("Density VS Alcohol") +
  xlab("Density") +
  ylab("Alcohol")


# Creating a categorical variable for wine quality
# WineData$quality <- ifelse(WineData$quality == 3, "Lev_Three", ifelse(WineData$quality == 4, "Lev_Four", ifelse(WineData$quality == 5, "Lev_Five", ifelse(WineData$quality == 6, "Lev_Six", ifelse(WineData$quality == 7, "Lev_Seven", ifelse(WineData$quality == 8, "Lev_Eight", "Lev_Nine"))) )))
# WineData$quality <- as.factor(WineData$quality)
# str(WineData)

WineData$quality <- ifelse(WineData$quality < 5, 'bad', ifelse(WineData$quality > 6,'good','normal'))
WineData$quality <- as.factor(WineData$quality)
str(WineData$quality)




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

# # C5.0
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
confusionMatrix(WineData_test$quality, WineData_predict)

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
confusionMatrix(WineData_test$quality, WineData_boost10_predict)
