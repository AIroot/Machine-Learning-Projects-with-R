# Reference for data source (
# @misc{Lichman:2013 ,
# author = "M. Lichman",
# year = "2013",
# title = "{UCI} Machine Learning Repository",
# url = "http://archive.ics.uci.edu/ml",
# institution = "University of California, Irvine, School of Information and Computer Sciences" })


# Source of Data Set:-  UCI Repository- Hepatitis Data (https://archive.ics.uci.edu/ml/datasets/hepatitis)

# # Collecting data
# Download data from UCI repo
HepatitisDataUrl <- "https://archive.ics.uci.edu/ml/machine-learning-databases/hepatitis/hepatitis.data"

# Read the url html file into a data frame titled HepatitisData.
HepatitisData <- read.csv(HepatitisDataUrl, header=FALSE)


# Assigning attributes information 
colnames(HepatitisData) <- c("class", "Age", "Sex", "Steroid",
 "Antivirals", "Fatigue", "Malaise", "Anorexia", "Liver_Big", "Liver_Firm", 
 "Spleen_palpable", "Spiders", "Ascites", "Varices", 
 "Bilirubin", "Alk_Phosphate", "Sgot", "Albumin", "Protime", 
 "Histology")

str(HepatitisData)
table(HepatitisData$class)
# # list rows of data that have missing values 
HepatitisData[HepatitisData=='?'] <- NA
# HepatitisData[!complete.cases(HepatitisData),]

# head(HepatitisData)
# # # create new dataset without missing data 
# HepatitisData <- na.omit(HepatitisData)

str(HepatitisData)

# HepatitisData$Liver_Firm <- factor(HepatitisData$Liver_Firm)
# table(HepatitisData$Liver_Firm)

f=function(x){
   x<-as.numeric(as.character(x)) #first convert each column into numeric if it is from factor
   x[is.na(x)] =median(x, na.rm=TRUE) #convert the item with NA to median value from the column
   x #display the column
}
HepatitisData=data.frame(apply(HepatitisData,2,f))
str(HepatitisData)

summary(HepatitisData)

# Identify Outliers in the data set; See Rplots.pdf for visualisation
boxplot(HepatitisData$Age~HepatitisData$class, main="Age",ylab="",xlab="")
boxplot(HepatitisData$Bilirubin~HepatitisData$class, main="Bilirubin",ylab="",xlab="")
boxplot(HepatitisData$Alk_Phosphate~HepatitisData$class, main="Alk_Phosphate",ylab="",xlab="")
boxplot(HepatitisData$Sgot~HepatitisData$class, main="Sgot",ylab="",xlab="")
boxplot(HepatitisData$Albumin~HepatitisData$class, main="Albumin",ylab="",xlab="")
boxplot(HepatitisData$Protime~HepatitisData$class, main="Protime",ylab="",xlab="")

# Transformation - normalizing numeric data

normalize <- function(x) {
	return ((x - min(x)) / (max(x) - min(x)))
}

# Apply normalize function to each list elememt. 
HepatitisData <- as.data.frame(lapply(HepatitisData, normalize))

summary(HepatitisData)
str(HepatitisData)
table(HepatitisData$class)
HepatitisData_CV <- HepatitisData
# HepatitisData$class <- ifelse(HepatitisData$class == 0, 'Die','Live')
# HepatitisData$class <- as.factor(HepatitisData$class)

# Data preparation - creating random training and test datasets
# Create random sample
# Divide the data into a training set and a test set randomly with ratio 75:25
ind = sample(2,nrow(HepatitisData),replace=TRUE,prob=c(0.7,0.3)) # type ?sample for moredetail
HepatitisData_train = HepatitisData[ind==1,]
HepatitisData_test = HepatitisData[ind==2,]
# ### add age label to the trainset
HepatitisData_train$Die = HepatitisData_train$class== 0
HepatitisData_train$Live = HepatitisData_train$class ==1

# combine the column names and then tack on the response variable
col_list <- paste(c(colnames(HepatitisData_train[,-c(1,21,22)])), collapse="+")

col_list <- paste(c("Live + Die ~", col_list), collapse="")

# create formula for ANN model
f <- formula(col_list)
f
# table(abaloneData_test$Rings)

# Train model

# NeuralNet 
# # Training a model on the data
# The neuralnet package can be installed via the install.packages("neuralnet") and 
# loaded with the library(neuralnet) command.
library(neuralnet)
# The model is used to train simplest multilayer feedforward network 
#with only a single hidden node
HepatitisData_Model <- neuralnet(f, data = HepatitisData_train, hidden = 3)

# diagram of network connections
plot(HepatitisData_Model)

HepatitisData_Model
HepatitisData_Model$result.matrix #see the summary

head(HepatitisData_Model$generalized.weights[[1]])

# Generate a prediction probability matrix
net.predict = compute(HepatitisData_Model, HepatitisData_test[-1])$net.result

# Find other possible labels by finding the column with the greatest probability
net.prediction = c(1, 0)[apply(net.predict, 1, which.max)]

# Generate a classification table 
predict.table = table(HepatitisData_test$class, net.prediction)
predict.table

# Generate classAgreement from the classification table 
library(e1071)
classAgreement(predict.table)
library(caret)
confusionMatrix(predict.table)

#Plot ROC curve
detach(package:neuralnet,unload = T)

library(ROCR)
nn.pred = prediction(net.prediction, HepatitisData_test$class)
pref <- performance(nn.pred, "tpr", "fpr")
plot(pref)
abline(0, 1, col="lightgrey")

# calculate the AUC
perf.auc <- performance(nn.pred, measure = "auc")

str(perf.auc)

# Access the AUC values
unlist(perf.auc@y.values)


# Cross-validation
# perform the full 10-fold CV
library(irr) # to calculate kappa
library(neuralnet)
set.seed(123)

folds <- createFolds(HepatitisData_CV$class, k =10)

# apply a series of identical steps to the list of folds using the lapply()
cv_results <- lapply(folds, function(x) {
	#10 percent to the test dataset, and use the negative symbol to assign the remaining
	# 90 percent to the training dataset:
HepatitisData_train = HepatitisData_CV[-x,]
HepatitisData_test = HepatitisData_CV[x,]

# ### add age label to the trainset
HepatitisData_train$Die = HepatitisData_train$class== 0
HepatitisData_train$Live = HepatitisData_train$class ==1

# combine the column names and then tack on the response variable
col_list <- paste(c(colnames(HepatitisData_train[,-c(1,21,22)])), collapse="+")

col_list <- paste(c("Live + Die ~", col_list), collapse="")

# create formula for ANN model
f <- formula(col_list)
HepatitisData_Model <- neuralnet(f, data = HepatitisData_train, hidden = 3)
# Generate a prediction probability matrix
net.predict = compute(HepatitisData_Model, HepatitisData_test[-1])$net.result

# Find other possible labels by finding the column with the greatest probability
net.prediction = c(1, 0)[apply(net.predict, 1, which.max)]

Hepatitis_actual <- HepatitisData_test$class
# generates a set of
# predictions from the test data, and compares the predicted and actual values using
# the kappa2() function:
kappa <- kappa2(data.frame(Hepatitis_actual, net.prediction))$value
return(kappa)
})

# The resulting kappa statistics are compiled into a list stored in the cv_results
# object
str(cv_results)

# calculate the average of these 10 values

mean(unlist(cv_results))