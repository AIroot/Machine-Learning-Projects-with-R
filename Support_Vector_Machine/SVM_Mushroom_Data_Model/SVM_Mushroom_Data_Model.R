# Reference for data source (
# @misc{Lichman:2013 ,
# author = "M. Lichman",
# year = "2013",
# title = "{UCI} Machine Learning Repository",
# url = "http://archive.ics.uci.edu/ml",
# institution = "University of California, Irvine, School of Information and Computer Sciences" })

# Support Vector Machine
# Source of Data Set:-  UCI Repository- Mushroom Data (http://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/)

# # Collecting data
# Download data from UCI repo
MushroomDataUrl <- "http://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data"

# Read the url html file into a data frame titled MushroomData.
MushroomData <- read.csv(MushroomDataUrl, header=FALSE)

# Assigning attributes information 
colnames(MushroomData) <- c("class", "cap_shape", "cap_surface", "cap_color", 
							"bruises", "odor", "gill_attachment", "gill_spacing", 
							"gill_size", "gill_color", "stalk_shape", "stalk_root", 
							"stalk_surface_above_ring", "stalk_surface_below_ring", 
							"stalk_color_above_ring", "stalk_color_below_ring", "veil_type", 
							"veil_color", "ring_number", "ring_type", "spore_print_color", 
							"population", "habitat")


# Write a CSV file from MushroomData
MushroomData <- write.csv(MushroomData, file = "MushroomData.csv", row.names = FALSE)

MushroomData <- read.csv("MushroomData.csv", header= TRUE)
#Removing one factors
MushroomData$veil_type=NULL
# str(MushroomData)
# summary(MushroomData)

# Load libraries.
library(ggplot2);
library(caret);
library(gridExtra);

# Explore the data.
qp1 = qplot(cap_color, habitat, color = class, data = MushroomData, geom = "jitter", main = "Mushroom data set - Cap color vs Habitat");
qp2 = qplot(cap_color, habitat, color = class, data = MushroomData, geom = "jitter", facets = bruises ~ ., main = "Mushroom data set by Bruises - Cap color vs Habitat");
qp3 = qplot(odor, spore_print_color, color = class, data = MushroomData, geom = "jitter", main = "Mushroom data set - Odor vs Spore Print Color");
qp4 = qplot(stalk_surface_below_ring, stalk_color_above_ring, color = class, data = MushroomData, geom = "jitter", main = "Mushroom data set - Stalk Surface below Ring vs Stalk Color above Ring");

grid.arrange(qp1, qp2, qp3, qp4, ncol = 2, nrow = 2);

# Explore the overlap where there is no odor and a spore print color of white.
MushroomData$class = as.character(MushroomData$class)
MushroomData$class[MushroomData$odor == "n" & MushroomData$spore_print_color == "w" & MushroomData$class == "poisonous"] = "poisonous2"
MushroomData$class[MushroomData$odor == "n" & MushroomData$spore_print_color == "w" & MushroomData$class == "edible"] = "edible2"
MushroomData$class = as.factor(MushroomData$class)

qp5 = qplot(cap_color, habitat, color = class, data = MushroomData, geom = "jitter", main = "Mushroom data set - Cap color vs Habitat");
qp6 = qplot(cap_color, habitat, color = class, data = MushroomData, geom = "jitter", facets = bruises ~ ., main = "Mushroom data set by Bruises - Cap color vs Habitat");
qp7 = qplot(odor, spore_print_color, color = class, data = MushroomData, geom = "jitter", main = "Mushroom data set - Odor vs Spore Print Color");
qp8 = qplot(stalk_surface_below_ring, stalk_color_above_ring, color = class, data = MushroomData, geom = "jitter", main = "Mushroom data set - Stalk Surface below Ring vs Stalk Color above Ring");

grid.arrange(qp5, qp6, qp7, qp8, ncol = 2, nrow = 2);

# Bar chart 
qplot(factor(class),data=MushroomData,geom = "bar",fill = factor(cap_color))
qplot(factor(class),data=MushroomData,geom = "bar",fill = factor(bruises))
qplot(factor(class),data=MushroomData,geom = "bar",fill = factor(odor))
qplot(factor(class),data=MushroomData,geom = "bar",fill = factor(gill_attachment))
qplot(factor(class),data=MushroomData,geom = "bar",fill = factor(gill_spacing))
qplot(factor(class),data=MushroomData,geom = "bar",fill = factor(gill_size))
qplot(factor(class),data=MushroomData,geom = "bar",fill = factor(gill_color))

# Identify missing values using graphical view. See the Rplot.pdf and red colour stripes indicate the missing values.  
library(Amelia)
missmap(MushroomData, main="Missing Data - Mushroom", col=c("red","grey"), legend=FALSE)

 ## Convert factors having character levels to numeric levels
table(MushroomData$cap_shape)
table(MushroomData$cap_surface)
table(MushroomData$cap_color)
table(MushroomData$bruises)
table(MushroomData$odor)
table(MushroomData$gill_attachment)
table(MushroomData$gill_spacing)
table(MushroomData$gill_size)
table(MushroomData$gill_color)
table(MushroomData$stalk_shape)
table(MushroomData$stalk_root)
table(MushroomData$stalk_surface_above_ring)
table(MushroomData$stalk_surface_below_ring)
table(MushroomData$stalk_color_above_ring)
table(MushroomData$stalk_color_below_ring)
table(MushroomData$veil_color)
table(MushroomData$ring_number)
table(MushroomData$ring_type)
table(MushroomData$spore_print_color)
table(MushroomData$population)
table(MushroomData$habitat)

# Convert factors having character levels to numeric levels
#MushroomData$class = factor(MushroomData$class, levels=c("e", "p"), labels=c(1,2))
MushroomData$cap_shape = factor(MushroomData$cap_shape, levels=c( "b","c","f","k","s","x"), labels=c(1,2,3,4,5,6))
MushroomData$cap_surface = factor(MushroomData$cap_surface, levels=c("f", "g","s", "y"), labels=c(1,2,3,4))
MushroomData$cap_color = factor(MushroomData$cap_color, levels=c( "b","c","e","g","n","p","r","u","w","y"), labels=c(1,2,3,4,5,6,7,8,9,10))
MushroomData$bruises = factor(MushroomData$bruises, levels=c("f", "t"), labels=c(1,2))
MushroomData$odor = factor(MushroomData$odor, levels=c("a","c","f","l","m","n","p","s","y"), labels=c(1,2,3,4,5,6,7,8,9))
MushroomData$gill_attachment = factor(MushroomData$gill_attachment, levels=c("a", "f"), labels=c(1,2))
MushroomData$gill_spacing = factor(MushroomData$gill_spacing, levels=c("c", "w"), labels=c(1,2))
MushroomData$gill_size = factor(MushroomData$gill_size, levels=c("b", "n"), labels=c(1,2))
MushroomData$gill_color = factor(MushroomData$gill_color, levels=c("b","e","g","h","k","n","o","p","r","u","w","y"), labels=c(1,2,3,4,5,6,7,8,9,10,11,12))
MushroomData$stalk_shape = factor(MushroomData$stalk_shape, levels=c("e", "t"), labels=c(1,2))
MushroomData$stalk_root = factor(MushroomData$stalk_root, levels=c("?","b","c","e","r"), labels=c(1,2,3,4,5))
MushroomData$stalk_surface_above_ring = factor(MushroomData$stalk_surface_above_ring, levels=c("f","k","s","y"), labels=c(1,2,3,4))
MushroomData$stalk_surface_below_ring = factor(MushroomData$stalk_surface_below_ring, levels=c("f","k","s","y"), labels=c(1,2,3,4))
MushroomData$stalk_color_above_ring = factor(MushroomData$stalk_color_above_ring, levels=c("b","c","e","g","n","o","p","w","y"), labels=c(1,2,3,4,5,6,7,8,9))
MushroomData$stalk_color_below_ring = factor(MushroomData$stalk_color_below_ring, levels=c("b","c","e","g","n","o","p","w","y"), labels=c(1,2,3,4,5,6,7,8,9))
MushroomData$veil_color = factor(MushroomData$veil_color, levels=c("n", "o","w","y"), labels=c(1,2,3,4))
MushroomData$ring_number = factor(MushroomData$ring_number, levels=c("n", "o","t"), labels=c(1,2,3))
MushroomData$ring_type = factor(MushroomData$ring_type, levels=c("e","f","l","n","p"), labels=c(1,2,3,4,5))
MushroomData$spore_print_color = factor(MushroomData$spore_print_color, levels=c("b","h","k","n","o","r","u","w","y"), labels=c(1,2,3,4,5,6,7,8,9))
MushroomData$population = factor(MushroomData$population, levels=c("a","c","n","s","v","y"), labels=c(1,2,3,4,5,6))
MushroomData$habitat = factor(MushroomData$habitat, levels=c("d","g","l","m","p","u","w"), labels=c(1,2,3,4,5,6,7))

# # Now convert these numerical factors into numeric
#MushroomData$class<-as.numeric(as.character(MushroomData$class))
MushroomData$cap_shape<-as.numeric(as.character(MushroomData$cap_shape))
MushroomData$cap_surface<-as.numeric(as.character(MushroomData$cap_surface))
MushroomData$cap_color<-as.numeric(as.character(MushroomData$cap_color))
MushroomData$bruises<-as.numeric(as.character(MushroomData$bruises))
MushroomData$odor<-as.numeric(as.character(MushroomData$odor))
MushroomData$gill_attachment<-as.numeric(as.character(MushroomData$gill_attachment))
MushroomData$gill_spacing<-as.numeric(as.character(MushroomData$gill_spacing))
MushroomData$gill_size<-as.numeric(as.character(MushroomData$gill_size))
MushroomData$gill_color<-as.numeric(as.character(MushroomData$gill_color))
MushroomData$stalk_shape<-as.numeric(as.character(MushroomData$stalk_shape))
MushroomData$stalk_root<-as.numeric(as.character(MushroomData$stalk_root))
MushroomData$stalk_surface_above_ring<-as.numeric(as.character(MushroomData$stalk_surface_above_ring))
MushroomData$stalk_surface_below_ring<-as.numeric(as.character(MushroomData$stalk_surface_below_ring))
MushroomData$stalk_color_above_ring<-as.numeric(as.character(MushroomData$stalk_color_above_ring))
MushroomData$stalk_color_below_ring<-as.numeric(as.character(MushroomData$stalk_color_below_ring))
MushroomData$veil_color<-as.numeric(as.character(MushroomData$veil_color))
MushroomData$ring_number<-as.numeric(as.character(MushroomData$ring_number))
MushroomData$ring_type<-as.numeric(as.character(MushroomData$ring_type))
MushroomData$spore_print_color<-as.numeric(as.character(MushroomData$spore_print_color))
MushroomData$population<-as.numeric(as.character(MushroomData$population))
MushroomData$habitat<-as.numeric(as.character(MushroomData$habitat))


# # # Transformation - normalizing numeric data
# table(MushroomData$cap_shape)
# table(MushroomData$cap_surface)
# table(MushroomData$cap_color)
# table(MushroomData$bruises)
# table(MushroomData$odor)
# table(MushroomData$gill_attachment)
# table(MushroomData$gill_spacing)
# table(MushroomData$gill_size)
# table(MushroomData$gill_color)
# table(MushroomData$stalk_shape)
# table(MushroomData$stalk_root)
# table(MushroomData$stalk_surface_above_ring)
# table(MushroomData$stalk_surface_below_ring)
# table(MushroomData$stalk_color_above_ring)
# table(MushroomData$stalk_color_below_ring)
# table(MushroomData$veil_color)
# table(MushroomData$ring_number)
# table(MushroomData$ring_type)
# table(MushroomData$spore_print_color)
# table(MushroomData$population)
# table(MushroomData$habitat)

# normalize <- function(x) {
# 	return ((x - min(x)) / (max(x) - min(x)))
# }

# # Apply normalize function to each list elememt. 
# MushroomData_n <- as.data.frame(lapply(MushroomData, normalize))



# Data preparation - creating random training and test datasets
# Create random sample
# Divide the data into a training set and a test set randomly with ratio 75:25

set.seed(123)
train_sample <- sample(nrow(MushroomData), 0.7 * nrow(MushroomData))
MushroomData_train <- MushroomData[train_sample, ]
MushroomData_test <- MushroomData[-train_sample, ]

# Training the model 01 
library(kernlab)
MushroomData_model <- ksvm(class ~ ., data = MushroomData_train, kernel = "vanilladot")

# See the basic parameters and the fit of the model
MushroomData_model

# Evaluating model performance
MushroomData_Predictions <- predict(MushroomData_model, MushroomData_test)


# Examine how well our classifier performed
table(MushroomData_Predictions, MushroomData_test$class)

# Considers only whether the prediction was correct or incorrect
predictionStatus <- MushroomData_Predictions == MushroomData_test$class

table(predictionStatus)

# Accuracy
prop.table(table(predictionStatus))

# Improving model 01 performance
# Use the Gaussian RBF kernel
MushroomData_model_RBF <- ksvm(class ~., data = MushroomData_train, kernel = "rbfdot")

# Make predictions
MushroomData_Predictions_RBF <- predict(MushroomData_model_RBF, MushroomData_test)

# Examine how well our classifier performed
table(MushroomData_Predictions_RBF, MushroomData_test$class)
# Compare the accuracy 
Prdeict_accuracy_RBF <- MushroomData_Predictions_RBF == MushroomData_test$class

table(Prdeict_accuracy_RBF)
# Probability of correct prdiction and incorrect prdiction
prop.table(table(Prdeict_accuracy_RBF))


# Training the model 02 
library(e1071)
MushroomData_model02 <- svm(class ~ ., data = MushroomData_train, kernel='linear', cost=1, scale=FALSE)

# See the basic parameters and the fit of the model
MushroomData_model02

# Evaluating model performance
MushroomData_Predictions02 <- predict(MushroomData_model02, MushroomData_test)


# Examine how well our classifier performed
table(MushroomData_Predictions02, MushroomData_test$class)

# Considers only whether the prediction was correct or incorrect
predictionStatus02 <- MushroomData_Predictions02 == MushroomData_test$class

table(predictionStatus02)

# Accuracy
prop.table(table(predictionStatus02))


# Training the model 03
print("Train model/ kernel=radial")
MushroomData_model03 <- svm(class ~ ., data = MushroomData_train, kernel='radial', cost=1, scale=FALSE)

# See the basic parameters and the fit of the model
MushroomData_model03

# Evaluating model performance
MushroomData_Predictions03 <- predict(MushroomData_model03, MushroomData_test)


# Examine how well our classifier performed
table(MushroomData_Predictions03, MushroomData_test$class)

# Considers only whether the prediction was correct or incorrect
predictionStatus03 <- MushroomData_Predictions03 == MushroomData_test$class

table(predictionStatus03)

# Accuracy
prop.table(table(predictionStatus03))


# Training the model 04
print("Train model/ kernel=polynomial")
MushroomData_model04 <- svm(class ~ ., data = MushroomData_train, kernel='polynomial', cost=1, scale=FALSE)

# See the basic parameters and the fit of the model
MushroomData_model04

# Evaluating model performance
MushroomData_Predictions04 <- predict(MushroomData_model04, MushroomData_test)


# Examine how well our classifier performed
table(MushroomData_Predictions04, MushroomData_test$class)

# Considers only whether the prediction was correct or incorrect
predictionStatus04 <- MushroomData_Predictions04 == MushroomData_test$class

table(predictionStatus04)

# Accuracy
prop.table(table(predictionStatus04))

# Training the model 05
print("Train model/ kernel=sigmoid")
MushroomData_model05 <- svm(class ~ ., data = MushroomData_train, kernel='sigmoid', cost=1, scale=FALSE)

# See the basic parameters and the fit of the model
MushroomData_model05

# Evaluating model performance
MushroomData_Predictions05 <- predict(MushroomData_model05, MushroomData_test)


# Examine how well our classifier performed
table(MushroomData_Predictions05, MushroomData_test$class)

# Considers only whether the prediction was correct or incorrect
predictionStatus05 <- MushroomData_Predictions05 == MushroomData_test$class

table(predictionStatus05)

# Accuracy
prop.table(table(predictionStatus05))