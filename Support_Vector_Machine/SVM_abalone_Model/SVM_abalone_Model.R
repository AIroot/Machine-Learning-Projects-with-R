# Reference for data source (
# @misc{Lichman:2013 ,
# author = "M. Lichman",
# year = "2013",
# title = "{UCI} Machine Learning Repository",
# url = "http://archive.ics.uci.edu/ml",
# institution = "University of California, Irvine, School of Information and Computer Sciences" })

# Source of Data Set:-  UCI Repository- abalone Data (https://archive.ics.uci.edu/ml/datasets/Abalone)

# # Collecting data
# Download data from UCI repo
abaloneDataUrl <- "https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"

# Read the url html file into a data frame titled abaloneData.
abaloneData <- read.csv(abaloneDataUrl, header=FALSE)


# Assigning attributes information 
colnames(abaloneData) <- c("Sex", "Length", "Diameter", "Height",
 "Whole_weight", "Shucked_weight", "Viscera_weight", "Shell_weight", "Rings")
str(abaloneData)

abaloneData$Sex = factor(abaloneData$Sex, levels=c("F","I","M"), labels=c(1,2,3))
abaloneData$Sex <- as.numeric(as.character(abaloneData$Sex))
str(abaloneData)
summary(abaloneData)
table(abaloneData$Rings)
# Identify Outliers in the data set; See Rplots.pdf for visualisation  
# Create boxplots for continuous variables
par(mfrow=c(2,2))
boxplot(abaloneData$Length~abaloneData$Rings, main=" Length",ylab="Longest shell measurement",xlab="The age in years")
boxplot(abaloneData$Diameter~abaloneData$Rings, main=" Diameter",ylab="perpendicular to length",xlab="The age in years")
boxplot(abaloneData$Height~abaloneData$Rings, main=" with meat in shell",ylab="",xlab="The age in years")
boxplot(abaloneData$Whole_weight~abaloneData$Rings, main="whole abalone",ylab="",xlab="The age in years")
boxplot(abaloneData$Shucked_weight~abaloneData$Rings, main="weight of meat",ylab="",xlab="The age in years")
boxplot(abaloneData$Viscera_weight~abaloneData$Rings, main="gut weight",ylab="",xlab="The age in years")
boxplot(abaloneData$Shell_weight~abaloneData$Rings, main="after being dried",ylab="",xlab="The age in years")

# Remove outliers 
# removing the top 5  from each category
Remove_Outliers <- function (data,cols,n=5) { #Requires some data frame and the top N to remove
idx.to.remove <-integer(0) #Initialize a vector to hold  being removed
for (c in cols){ # For every column in the data we passed to this function
col.order <-order(data[,c],decreasing=T) #Sort column "c" in descending order (bigger on top)
#Order returns the sorted index (e.g. row 15, 3, 7, 1, ...) rather than the actual values sorted.
idx <-head(col.order, n) #Take the first n of the sorted column C to
idx.to.remove <-union(idx.to.remove,idx) #Combine and de-duplicate the row ids that need to be removed
}
return(idx.to.remove) #Return the indexes of  to be removed
}

Top_five <-Remove_Outliers(abaloneData,cols=1:9,n=5)
length(Top_five) #How Many data to be Removed?
abaloneData[Top_five,] #Examine the 
abaloneData<-abaloneData[-c(Top_five),] #Remove the data
summary(abaloneData)



boxplot(abaloneData$Length~abaloneData$Rings, main=" Length",ylab="Longest shell measurement",xlab="The age in years")
boxplot(abaloneData$Diameter~abaloneData$Rings, main=" Diameter",ylab="perpendicular to length",xlab="The age in years")
boxplot(abaloneData$Height~abaloneData$Rings, main=" with meat in shell",ylab="",xlab="The age in years")
boxplot(abaloneData$Whole_weight~abaloneData$Rings, main="whole abalone",ylab="",xlab="The age in years")
boxplot(abaloneData$Shucked_weight~abaloneData$Rings, main="weight of meat",ylab="",xlab="The age in years")
boxplot(abaloneData$Viscera_weight~abaloneData$Rings, main="gut weight",ylab="",xlab="The age in years")
boxplot(abaloneData$Shell_weight~abaloneData$Rings, main="after being dried",ylab="",xlab="The age in years")


# Rings are cut into three groups
# table(abaloneData$Rings)
# abaloneData$Rings = cut(abaloneData$Rings, 2, labels = c(1,2))
# table(abaloneData$Rings)
# Convert into num
# abaloneData$Rings = factor(abaloneData$Rings, levels=c(1,2), labels=c(1,2))
# abaloneData$Rings <- as.numeric(as.character(abaloneData$Rings))
# table(abaloneData$Rings)
abaloneData$Rings <- ifelse(abaloneData$Rings < 7, 'L1', ifelse(abaloneData$Rings > 12,'L3','L2'))
abaloneData$Rings <- as.factor(abaloneData$Rings)
str(abaloneData$Rings)
table(abaloneData$Rings)

# # Transformation - normalizing numeric data

# normalize <- function(x) {
# 	return ((x - min(x)) / (max(x) - min(x)))
# }

# # Apply normalize function to each list elememt. 
# abaloneData <- as.data.frame(lapply(abaloneData, normalize))


abaloneData_CV <- abaloneData

# Data preparation - creating random training and test datasets
# Create random sample
# Divide the data into a training set and a test set randomly with ratio 70:30
ind = sample(2,nrow(abaloneData),replace=TRUE,prob=c(0.7,0.3)) # type ?sample for moredetail
abaloneData_train = abaloneData[ind==1,]
abaloneData_test = abaloneData[ind==2,]

# Training the model 01 
library(kernlab)
abaloneData_model <- ksvm(Rings ~ ., data = abaloneData_train, kernel = "vanilladot")

# See the basic parameters and the fit of the model
abaloneData_model

# Evaluating model performance
abaloneData_Predictions <- predict(abaloneData_model, abaloneData_test)


# Examine how well our classifier performed
predict.table = table(abaloneData_Predictions, abaloneData_test$Rings)

# Considers only whether the prediction was correct or incorrect
predictionStatus <- abaloneData_Predictions == abaloneData_test$Rings

table(predictionStatus)

# Accuracy
prop.table(table(predictionStatus))

# Generate classAgreement from the classification table 
library(e1071)
classAgreement(predict.table)
library(caret)
confusionMatrix(predict.table)

# Cross-validation
# perform the full 10-fold CV
library(irr) # to calculate kappa
library(neuralnet)
set.seed(123)

folds <- createFolds(abaloneData_CV$Rings, k =10)

# apply a series of identical steps to the list of folds using the lapply()
cv_results <- lapply(folds, function(x) {
	#10 percent to the test dataset, and use the negative symbol to assign the remaining
	# 90 percent to the training dataset:
abaloneData_train = abaloneData_CV[-x,]
abaloneData_test = abaloneData_CV[x,]

abaloneData_model <- ksvm(Rings ~ ., data = abaloneData_train, kernel = "vanilladot")

# Evaluating model performance
abaloneData_Predictions <- predict(abaloneData_model, abaloneData_test)

abaloneData_actual <- abaloneData_test$Rings
# generates a set of
# predictions from the test data, and compares the predicted and actual values using
# the kappa2() function:
kappa <- kappa2(data.frame(abaloneData_actual, abaloneData_Predictions))$value
return(kappa)
})

# The resulting kappa statistics are compiled into a list stored in the cv_results
# object
str(cv_results)

# calculate the average of these 10 values

mean(unlist(cv_results))

