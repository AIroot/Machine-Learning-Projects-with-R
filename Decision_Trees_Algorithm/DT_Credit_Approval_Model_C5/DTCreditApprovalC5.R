# Reference for data source (
# @misc{Lichman:2013 ,
# author = "M. Lichman",
# year = "2013",
# title = "{UCI} Machine Learning Repository",
# url = "http://archive.ics.uci.edu/ml",
# institution = "University of California, Irvine, School of Information and Computer Sciences" })

# Decision Trees
# Source of Data Set:- UCI Repository - German Credit Data(https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data)

# Step 01: Collecting data 
# Download data from UCI repository
CreditDataUrl <- "https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data"

# Read the url html file into a data frame titled CreditData.
CreditData <- read.table(CreditDataUrl)

# Assging attribute information
# The target function column name is class
colnames(CreditData) <- c("chk_status", "mth_duration", "credit_history", "purpose", "credit_amount", "saving", "employ_time", "pct_dpi", "status_gender", "other_debts", "residency_time", "property", "age", "other_installments", "housing", "existing_credits", "job", "dependents_num", "phone", "foreign", "class")

# Write a CSV file from CreditData
Credit_Data <- write.csv(CreditData, file = "CreditData.csv", row.names = FALSE)

# Exploring and preparing the data
# Step 2: Exploring and preparing the data
# Read the csv file into a data frame titled CreditData.
CreditData <- read.csv("CreditData.csv", header=TRUE)

# Class columns convert into facator
CreditData$class <- ifelse(CreditData$class==1, "good","bad")
CreditData$class = as.factor(CreditData$class)

# Displays description of each variable
head(CreditData)

str(CreditData)

# Use table() function to look output for a couple of loan features 
# Attribute 1: (qualitative) 
# Status of existing checking account 
# A11 : ... < 0 DM 
# A12 : 0 <= ... < 200 DM 
# A13 : ... >= 200 DM / salary assignments for at least 1 year 
# A14 : no checking account 
table(CreditData$chk_status)

# Attribute 3: (qualitative) 
# Credit history 
# A30 : no credits taken/ all credits paid back duly 
# A31 : all credits at this bank paid back duly 
# A32 : existing credits paid back duly till now 
# A33 : delay in paying off in the past 
# A34 : critical account/ other credits existing (not at this bank) 
table(CreditData$credit_history)

# Attribute 4: (qualitative) 
# Purpose 
# A40 : car (new) 
# A41 : car (used) 
# A42 : furniture/equipment 
# A43 : radio/television 
# A44 : domestic appliances 
# A45 : repairs 
# A46 : education 
# A47 : (vacation - does not exist?) 
# A48 : retraining 
# A49 : business 
# A410 : others 
table(CreditData$purpose)

# Attibute 6: (qualitative) 
# Savings account/bonds 
# A61 : ... < 100 DM 
# A62 : 100 <= ... < 500 DM 
# A63 : 500 <= ... < 1000 DM 
# A64 : .. >= 1000 DM 
# A65 : unknown/ no savings account 
table(CreditData$saving)

# Attribute 7: (qualitative) 
# Present employment since 
# A71 : unemployed 
# A72 : ... < 1 year 
# A73 : 1 <= ... < 4 years 
# A74 : 4 <= ... < 7 years 
# A75 : .. >= 7 years 
table(CreditData$employ_time)

# Attribute 9: (qualitative) 
# Personal status and sex 
# A91 : male : divorced/separated 
# A92 : female : divorced/separated/married 
# A93 : male : single 
# A94 : male : married/widowed 
# A95 : female : single 
table(CreditData$status_gender)

# Attribute 10: (qualitative) 
# Other debtors / guarantors 
# A101 : none 
# A102 : co-applicant 
# A103 : guarantor 
table(CreditData$other_debts)

# Attribute 12: (qualitative) 
# Property 
# A121 : real estate 
# A122 : if not A121 : building society savings agreement/ life insurance 
# A123 : if not A121/A122 : car or other, not in attribute 6 
# A124 : unknown / no property 
table(CreditData$property)

# Attribute 14: (qualitative) 
# Other installment plans 
# A141 : bank 
# A142 : stores 
# A143 : none 
table(CreditData$other_installments)

# Attribute 15: (qualitative) 
# Housing 
# A151 : rent 
# A152 : own 
# A153 : for free 
table(CreditData$housing)

# Attribute 17: (qualitative) 
# Job 
# A171 : unemployed/ unskilled - non-resident 
# A172 : unskilled - resident 
# A173 : skilled employee / official 
# A174 : management/ self-employed/ 
# highly qualified employee/ officer
table(CreditData$job)

# Attribute 19: (qualitative) 
# Telephone 
# A191 : none 
# A192 : yes, registered under the customers name 
table(CreditData$phone)

# Attribute 20: (qualitative) 
# foreign worker 
# A201 : yes 
# A202 : no 
table(CreditData$foreign)

# Attribute 21: 
table(CreditData$class)

# Check credit amount
summary(CreditData$credit_amount)

# Check duration 
summary(CreditData$mth_duration)

# Data preparation - creating random training and test datasets
# Create random sample
# Divide the data into a training set and a test set randomly with ratio 90:10

set.seed(123)
train_sample <- sample(1000, 900)
CreditData_train <- CreditData[train_sample, ]
CreditData_test <- CreditData[-train_sample, ]

# Check whether data set fairly even split
prop.table(table(CreditData_train$class))

prop.table(table(CreditData_test$class))

