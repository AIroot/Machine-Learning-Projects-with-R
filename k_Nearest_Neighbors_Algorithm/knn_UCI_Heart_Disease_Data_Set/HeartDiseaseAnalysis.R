# Week 02 - Exercise 

# K Nearest Neighbour (KNN) 
# Source of Data Set:-  UCI Repository- Heart Disease Data (https://archive.ics.uci.edu/ml/datasets/Heart+Disease)
# Reference for data source (
# @misc{Lichman:2013 ,
# author = "M. Lichman",
# year = "2013",
# title = "{UCI} Machine Learning Repository",
# url = "http://archive.ics.uci.edu/ml",
# institution = "University of California, Irvine, School of Information and Computer Sciences" })


# Step 01 : Collecting data
# Download data from UCI repo
hdDataUrl <- "https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"

# Read the url html file into a data frame titled hd_data .
hd_data <- read.csv(hdDataUrl, header=FALSE)

# Assging attribute inforamtion
# The predicted attribute is "num"
colnames(hd_data) <- c("age","sex","cp","trestbps","chol","fbs","restecg","thalach","exang","oldpeak","slope","ca","thal","num")

# Write a CSV file from hd_data
hdData <- write.csv(hd_data, file = "hd_data.csv",row.names = FALSE)

# Step 2: Exploring and preparing the data
# Read the CSV file into a data frame titled hd_Data.
hd_Data <- read.csv("hd_data.csv", header=TRUE)

# Displays description of each variable
str(hd_Data)

# # list rows of data that have missing values 
hd_Data[hd_Data=='?'] <- NA
hd_Data[!complete.cases(hd_Data),]

# # # Identify missing values using graphical view. See the Rplot.pdf and red colour stripes indicate the missing values.  
# # library(Amelia)
# # missmap(hd_Data, main="Missing Data - Heart Disease", col=c("red","grey"), legend=FALSE)


# # create new dataset without missing data 

new_hd_Data <- na.omit(hd_Data)

table(new_hd_Data$num)
summary(new_hd_Data)


# Identify Outliers in the data set; See Rplots.pdf for visualisation  
# Create boxplots for continuous variables

boxplot(new_hd_Data$age~new_hd_Data$num, main=" AGE",ylab="Age of patient",xlab="angiographic disease status")
boxplot(new_hd_Data$trestbps~new_hd_Data$num, main=" Trestbps",ylab="Resting blood pressure",xlab="angiographic disease status")
boxplot(new_hd_Data$chol~new_hd_Data$num, main=" Cholestoral",ylab="Serum cholestoral in mg/dl",xlab="angiographic disease status")
boxplot(new_hd_Data$thalach~new_hd_Data$num, main="Heart Rate",ylab="Maximum heart rate achieved",xlab="angiographic disease status")
boxplot(new_hd_Data$oldpeak~new_hd_Data$num, main="Oldpeak",ylab="ST depression induced by exercise relative to rest",xlab="angiographic disease status")

## Barplots for Categorical Variables
barplot(table(new_hd_Data$sex),col="red",main="SEX")
barplot(table(new_hd_Data$cp),col="green",main="Chest pain type")
barplot(table(new_hd_Data$fbs),col="red",main="Fasting blood sugar")
barplot(table(new_hd_Data$restecg),col="red",main="Resting electrocardiographic results")
barplot(table(new_hd_Data$exang),col="red",main="Exercise induced angina (1 = yes; 0 = no)")
barplot(table(new_hd_Data$slope),col="red",main="The slope of the peak exercise ST segment")
barplot(table(new_hd_Data$ca),col="red",main="Number of major vessels (0-3)")
barplot(table(new_hd_Data$thal),col="red",main="3 = normal; 6 = fixed defect; 7 = reversable defect")



# ## Correlation Matrix among input continuous variables
new_hd_Data_cor<-data.frame(new_hd_Data$age,new_hd_Data$trestbps,new_hd_Data$chol,new_hd_Data$thalach,new_hd_Data$oldpeak)
str(new_hd_Data_cor)
cor(new_hd_Data_cor)







# ## Prior running the KNN model, the dataset has to be transformed to Numeric shown below
# ## Convert factors having character levels to numeric levels

hd_Data_transform<-new_hd_Data
hd_Data_transform$ca=factor(hd_Data_transform$ca,levels=c("?","0.0","1.0","2.0","3.0"),labels=c(1,2,3,4,5))
hd_Data_transform$thal=factor(hd_Data_transform$thal,levels=c("?","3.0","6.0","7.0"),labels=c(1,2,3,4))

## Now convert these numerical factors into numeric 
hd_Data_transform$age<-as.numeric(as.character(hd_Data_transform$age))
hd_Data_transform$sex<-as.numeric(as.character(hd_Data_transform$sex))
hd_Data_transform$cp<-as.numeric(as.character(hd_Data_transform$cp))
hd_Data_transform$trestbps<-as.numeric(as.character(hd_Data_transform$trestbps))
hd_Data_transform$chol<-as.numeric(as.character(hd_Data_transform$chol))
hd_Data_transform$fbs<-as.numeric(as.character(hd_Data_transform$fbs))
hd_Data_transform$restecg<-as.numeric(as.character(hd_Data_transform$restecg))
hd_Data_transform$thalach<-as.numeric(as.character(hd_Data_transform$thalach))
hd_Data_transform$exang<-as.numeric(as.character(hd_Data_transform$exang))
hd_Data_transform$oldpeak<-as.numeric(as.character(hd_Data_transform$oldpeak))
hd_Data_transform$slope<-as.numeric(as.character(hd_Data_transform$slope))
hd_Data_transform$ca<-as.numeric(as.character(hd_Data_transform$ca))
hd_Data_transform$thal<-as.numeric(as.character(hd_Data_transform$thal))
hd_Data_transform$num<-as.numeric(as.character(hd_Data_transform$num))
str(hd_Data_transform)

# Transformation - normalizing numeric data

normalize <- function(x) {
	return ((x - min(x)) / (max(x) - min(x)))
}

# Apply normalize function to each list elememt. 
new_hd_Data_n <- as.data.frame(lapply(hd_Data_transform[1:13], normalize))

summary(new_hd_Data_n)


# Step 03: Data preparation – creating training and test datasets

# # Creaing training and test datasets using normalized data
# Non-randomied method
# hd_Data_train <- new_hd_Data_n[1:209, ]
# hd_Data_test <- new_hd_Data_n[210:297, ]

# str(hd_Data_train)
# str(hd_Data_test)

# hd_Data_train_label <- hd_Data_transform[1:209, 14]
# hd_Data_test_label <- hd_Data_transform[210:297, 14]

# The caret package is a set of functions that attempt to streamline the process for creating predictive models. The package contains tools for:
# data splitting
# pre-processing
# feature selection
# model tuning using resampling
# variable importance estimation
# as well as other functionality.
library(caret)
# Divide the data into a training set and a test set randomly with ratio 70:30
set.seed(1234567)
indexes <- createDataPartition(hd_Data_transform$num,p=0.7,list=FALSE)
hd_Data_train <- new_hd_Data_n[indexes,]
dim(hd_Data_train)
hd_Data_test <- new_hd_Data_n[-indexes,]
dim(hd_Data_test)


# Crate label data set by using none normalized data set for training and evaluating knn classifier
hd_Data_train_label <- hd_Data_transform[indexes, 14]
dim(hd_Data_train_label)
hd_Data_test_label <- hd_Data_transform[-indexes, 14]
dim(hd_Data_test_label)




# Step 04: Training a model on the data
library("class")
## Apply Knn
hd_data_pred <- knn(train = hd_Data_train, test= hd_Data_test, cl = hd_Data_train_label, k = 25)

# hd_data_pred <- knn(train = hd_Data_train, test= hd_Data_test, cl = hd_Data_train_label, k = 30)


# Various R Programming Tools for Model Fitting
library(gmodels)

# create a cross tabulation indicating the agreement between the two vectors.
# Specifying prop.chisq = FALSE will remove the unnecessary chi-square
# values from the output:
CrossTable(x = hd_Data_test_label, y = hd_data_pred, prop.chisq = FALSE)

#Find optimal k and accuracy

hd_Data_transform$num <- factor(hd_Data_transform$num)
model <- train(num~.,data=hd_Data_transform, method='knn',tuneGrid=expand.grid(.k=1:75), metric='Accuracy', trControl=trainControl(method='repeatedcv', number=10, repeats=15))
plot(model)
confusionMatrix(model)