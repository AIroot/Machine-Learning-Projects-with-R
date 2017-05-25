### Week 02 - Project

# K Nearest Neighbour (KNN) 
# Source of Data Set:-  UCI Repository- Bank Marketing (http://archive.ics.uci.edu/ml/datasets/Bank+Marketing)


# Step 01 : Collecting data
# Download and unzip data from UCI repo
temp <- tempfile()
download.file("http://archive.ics.uci.edu/ml/machine-learning-databases/00222/bank.zip",temp, mode="wb")
unzip(temp, "bank-full.csv")
unlink(temp)
# Read the data file into a data frame titled bank.df .
bank.df <- read.table("bank-full.csv", sep=";", header=T)



## Step 02: Data Exploration
## Display statistical information of each variable
summary(bank.df) 
# Displays description of each variable
str(bank.df)
# Identify missing values using graphical view. See the Rplot.pdf and red colour stripes indicate the missing values.  
library(Amelia)
missmap(bank.df, main="Missing Data - Bank Marketing Campaign", col=c("red","grey"), legend=FALSE)


# Identify Outliers in the data set; See Rplots.pdf for visualisation  
# Create boxplots for continuous variables

boxplot(bank.df$age~bank.df$y, main=" AGE",ylab="age of customers",xlab="Subscribed")
boxplot(bank.df$balance~bank.df$y, main=" BALANCE",ylab="Balance of customers",xlab="Subscribed")
boxplot(bank.df$day~bank.df$y, main=" DAY",ylab="Last day of contact",xlab="Subscribed")
boxplot(bank.df$duration~bank.df$y, main="DURATION",ylab="Last duration of contact",xlab="Subscribed")
boxplot(bank.df$campaign~bank.df$y, main="CONTACTS",ylab="number of contacts",xlab="Subscribed")
boxplot(bank.df$pdays~bank.df$y, main=" Previous DAYS",ylab="Previous days of contact",xlab="Subscribed")
boxplot(bank.df$previous~bank.df$y, main=" Previous Contacts",ylab="Previous Contacts with customers",xlab="Subscribed")


## Barplots for Categorical Variables
barplot(table(bank.df$job),col="red",main="JOB")
barplot(table(bank.df$marital),col="green",main="Marital")
barplot(table(bank.df$education),col="red",main="Education")
barplot(table(bank.df$default),col="red",main="Credit Default")

## Since Credit Default is highly skewed towards NO, this shall be removed from further analysis
bank.df[5]<-NULL
str(bank.df)
barplot(table(bank.df$housing),col="red",main="Housing Loan")
barplot(table(bank.df$loan),col="blue",main="Personal Loan")
barplot(table(bank.df$contact),col="red",main="Communication type")
barplot(table(bank.df$month),col="violet",main="Last Month")
barplot(table(bank.df$poutcome),col="magenta",main="Previous Outcome")

## Correlation Matrix among input continuous variables
bank.df.cont<-data.frame(bank.df$age,bank.df$balance,bank.df$day,bank.df$duration,bank.df$campaign,bank.df$pdays,bank.df$previous)
str(bank.df.cont)
cor(bank.df.cont)
## It can be observed that No two variables are highly correlated

##USING K Nearest Neighbour (KNN) Model
## Prior running the KNN model, the dataset has top be transformed to Numeric as shown below
## as.numeric() to convert factors to numeric.
#convert factors having character levels to numeric levels
str(bank.df)
bank.df_transform<-bank.df
bank.df_transform$marital=factor(bank.df_transform$marital,levels=c("single","married","divorced"),labels=c(1,2,3))
bank.df_transform$job=factor(bank.df_transform$job,levels=c("admin.","blue-collar","entrepreneur","housemaid","management","retired","self-employed","services","student","technician","unemployed","unknown"),labels=c(1,2,3,4,5,6,7,8,9,10,11,12))
bank.df_transform$education=factor(bank.df_transform$education,levels=c("primary","secondary","tertiary","unknown"),labels=c(1,2,3,4))
bank.df_transform$housing=factor(bank.df_transform$housing,levels=c("no","yes"),labels=c(1,2))
bank.df_transform$loan=factor(bank.df_transform$loan,levels=c("no","yes"),labels=c(1,2))
bank.df_transform$y=factor(bank.df_transform$y,levels=c("no","yes"),labels=c(1,2))
bank.df_transform$contact=factor(bank.df_transform$contact,levels=c("cellular","telephone","unknown"),labels=c(1,2,3))
bank.df_transform$poutcome=factor(bank.df_transform$poutcome,levels=c("failure","other","success","unknown"),labels=c(1,2,3,4))
bank.df_transform$month=factor(bank.df_transform$month,levels=c("apr","aug","dec","feb","jan","jul","jun","mar","may","nov","oct","sep"),labels=c(1,2,3,4,5,6,7,8,9,10,11,12))
str(bank.df_transform)

## convert these numerical factors into numeric 
bank.df_transform$y<-as.numeric(as.character(bank.df_transform$y))
bank.df_transform$job<-as.numeric(as.character(bank.df_transform$job))
bank.df_transform$marital<-as.numeric(as.character(bank.df_transform$marital))
bank.df_transform$education<-as.numeric(as.character(bank.df_transform$education))
bank.df_transform$loan<-as.numeric(as.character(bank.df_transform$loan))
bank.df_transform$housing<-as.numeric(as.character(bank.df_transform$housing))
bank.df_transform$contact<-as.numeric(as.character(bank.df_transform$contact))
bank.df_transform$month<-as.numeric(as.character(bank.df_transform$month))
bank.df_transform$poutcome<-as.numeric(as.character(bank.df_transform$poutcome))
bank.df_transform$age<-as.numeric(as.character(bank.df_transform$age))
bank.df_transform$balance<-as.numeric(as.character(bank.df_transform$balance))
bank.df_transform$day<-as.numeric(as.character(bank.df_transform$day))
bank.df_transform$duration<-as.numeric(as.character(bank.df_transform$duration))
bank.df_transform$campaign<-as.numeric(as.character(bank.df_transform$campaign))
bank.df_transform$pdays<-as.numeric(as.character(bank.df_transform$pdays))
bank.df_transform$previous<-as.numeric(as.character(bank.df_transform$previous))
str(bank.df_transform)


# Transformation - normalizing numeric data

normalize <- function(x) {
	return ((x - min(x)) / (max(x) - min(x)))
}

# Apply normalize function to each list elememt excluding the target variable,y. 
new_bank.df_n <- as.data.frame(lapply(bank.df_transform[1:15], normalize))

summary(new_bank.df_n)

# Step 03: Data preparation – creating training and test datasets
# Creaing training and test datasets using normalized data
# Non-randomied method
# bank_Data_train <- new_bank.df_n[1:31648, ]
# bank_Data_test <- new_bank.df_n[31649:45211, ]


# bank_Data_train_label <- bank.df_transform[1:31648, 16]
# bank_Data_test_label <- bank.df_transform[31649:45211, 16]



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
indexes <- createDataPartition(bank.df_transform$y,p=0.7,list=FALSE)
bank_Data_train <- new_bank.df_n[indexes,]
dim(bank_Data_train)
bank_Data_test <- new_bank.df_n[-indexes,]
dim(bank_Data_test)


# Crate label data set by using none normalized data set for training and evaluating knn classifier
bank_Data_train_label <- bank.df_transform[indexes, 16]
dim(bank_Data_train_label)
bank_Data_test_label <- bank.df_transform[-indexes, 16]
dim(bank_Data_test_label)




# Step 04: Training a model on the data
library("class")
## Apply Knn
bank_data_pred <- knn(train = bank_Data_train, test= bank_Data_test, cl = bank_Data_train_label, k = 13)


# Various R Programming Tools for Model Fitting
library(gmodels)

# create a cross tabulation indicating the agreement between the two vectors.
# Specifying prop.chisq = FALSE will remove the unnecessary chi-square
# values from the output:
CrossTable(x = bank_Data_test_label, y = bank_data_pred, prop.chisq = FALSE)

#Find optimal k and accuracy 

bank.df_transform$num <- factor(bank.df_transform$y)
model <- train(num~.,data=bank.df_transform, method='knn',tuneGrid=expand.grid(.k=1:200), metric='Accuracy', trControl=trainControl(method='repeatedcv', number=10, repeats=15))
plot(model)
confusionMatrix(model)
