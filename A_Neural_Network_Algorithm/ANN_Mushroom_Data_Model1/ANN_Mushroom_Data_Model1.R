# Reference for data source (
# @misc{Lichman:2013 ,
# author = "M. Lichman",
# year = "2013",
# title = "{UCI} Machine Learning Repository",
# url = "http://archive.ics.uci.edu/ml",
# institution = "University of California, Irvine, School of Information and Computer Sciences" })

# Neural Networks
# Source of Data Set:-  UCI Repository- Mushroom Data (http://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/)

# # Collecting data
# Download data from UCI repo
MushroomDataUrl <- "http://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data"

# Read the url html file into a data frame titled MushroomData.
MushroomData <- read.csv(MushroomDataUrl, header=FALSE)

# Assigning attributes information 
colnames(MushroomData) <- c("class", "cap_shape", "cap_surface", "cap_color",
 "bruises", "odor", "gill_attachment", "gill_spacing", "gill_size", "gill_color", 
 "stalk_shape", "stalk_root", "stalk_surface_above_ring", "stalk_surface_below_ring", 
 "stalk_color_above_ring", "stalk_color_below_ring", "veil_type", "veil_color", "ring_number", 
 "ring_type", "spore_print_color", "population", "habitat")


# # Write a CSV file from MushroomData
# MushroomData <- write.csv(MushroomData, file = "MushroomData.csv", row.names = FALSE)

MushroomData <- read.csv("MushroomData.csv", header= TRUE)
MushroomData$veil_type = factor(MushroomData$veil_type, levels=c("p"), labels=c(1))
MushroomData$veil_type<-as.numeric(as.character(MushroomData$veil_type))

# Reorder by column numbers
MushroomData <- MushroomData[c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,1)]
# #Removing one factors
# MushroomData$veil_type=NULL
str(MushroomData)


# Data preparation - creating random training and test datasets
# Create random sample
# Divide the data into a training set and a test set randomly with ratio 75:25

# set.seed(123)
# train_sample <- sample(nrow(MushroomData), 0.7 * nrow(MushroomData))
# MushroomData_train <- MushroomData[train_sample, ]
# MushroomData_test <- MushroomData[-train_sample, ]
# table(MushroomData_test$class)



# # Apply model.matrix for all variables to create dummy variables
# MushroomData_train_m <- model.matrix(~cap_shape+cap_surface+cap_color+bruises+odor+
# 							gill_attachment+gill_spacing+gill_size+gill_color+stalk_shape+
# 							stalk_root+stalk_surface_above_ring+stalk_surface_below_ring+
# 							stalk_color_above_ring+stalk_color_below_ring+veil_type+veil_color+
# 							ring_number+ring_type+spore_print_color+population+habitat+class, data=MushroomData_train)


# colnames(MushroomData_train_m)

# MushroomData_test_m <- model.matrix(~cap_shape+cap_surface+cap_color+bruises+odor+
# 							gill_attachment+gill_spacing+gill_size+gill_color+stalk_shape+
# 							stalk_root+stalk_surface_above_ring+stalk_surface_below_ring+
# 							stalk_color_above_ring+stalk_color_below_ring+veil_type+veil_color+
# 							ring_number+ring_type+spore_print_color+population+habitat+class, data=MushroomData_test)


# colnames(MushroomData_test_m)

# # combine the column names and then tack on the response variable
# col_list <- paste(c(colnames(MushroomData_train_m[,-c(1,98)])), collapse="+")

# col_list <- paste(c("classp~", col_list), collapse="")

# # create formula for ANN model
# f <- formula(col_list)



# # Train model

# # NeuralNet 
# # # Training a model on the data
# # The neuralnet package can be installed via the install.packages("neuralnet") and 
# # loaded with the library(neuralnet) command.
# library(neuralnet)
# set.seed(8896329)
# # The model is used to train simplest multilayer feedforward network 
# #with only a single hidden node
# # By default, using the Resilient Backpropogation algorithm (RPROP+).

# # Model 01
# Mushroom_Data_model <- neuralnet(f, data=MushroomData_train_m, hidden = 1,
# 						threshold = 0.01,
# 						learningrate.limit = NULL,
# 						learningrate.factor = list(minus = 0.5, plus = 1.2),
# 						algorithm = "rprop+")
# plot(Mushroom_Data_model)
# Mushroom_Data_model$result.matrix

# # Evaluating model performance
# model_results <- compute(Mushroom_Data_model, MushroomData_test_m[,-c(1,98)], rep=1)
# Predicted_class <- model_results$net.result
# summary(model_results)

# # cor() function is used to obtain a correlation between two numeric vectors
# cor(Predicted_class, MushroomData_test_m[,98])

# # Model 02
# #Changing Hidden Nodes and Backprop

# Mushroom_Data_model2 <- neuralnet(f, data=MushroomData_train_m, hidden = 5)
# plot(Mushroom_Data_model2)
# Mushroom_Data_model2$result.matrix

# # Evaluating model performance
# model_results2 <- compute(Mushroom_Data_model2, MushroomData_test_m[,-c(1,98)], rep=1)
# Predicted_class2 <- model_results2$net.result
# summary(model_results2)

# # cor() function is used to obtain a correlation between two numeric vectors
# cor(Predicted_class2, MushroomData_test_m[,98])

# # Model 03
# #Multiple hidden layers

# Mushroom_Data_model3 <- neuralnet(f, data=MushroomData_train_m,
# 								 , algorithm = "rprop+" ,hidden = c(10,3),
# 								 threshold = 0.01)
# plot(Mushroom_Data_model3)
# Mushroom_Data_model3$result.matrix

# # Evaluating model performance
# model_results3 <- compute(Mushroom_Data_model3, MushroomData_test_m[,-c(1,98)], rep=1)
# Predicted_class3 <- model_results3$net.result
# summary(model_results3)

# # cor() function is used to obtain a correlation between two numeric vectors
# cor(Predicted_class3, MushroomData_test_m[,98])

# # Model 04
# #Multiple hidden layers

# Mushroom_Data_model4 <- neuralnet(f, data=MushroomData_train_m,
# 								 , algorithm = "rprop+" ,hidden = c(10,5,3),
# 								 threshold = 0.01, act.fct = "logistic")
# plot(Mushroom_Data_model4)
# Mushroom_Data_model4$result.matrix

# # Evaluating model performance
# model_results4 <- compute(Mushroom_Data_model4, MushroomData_test_m[,-c(1,98)])
# Predicted_class4 <- model_results4$net.result
# summary(model_results4)

# # cor() function is used to obtain a correlation between two numeric vectors
# cor(Predicted_class4, MushroomData_test_m[,98])


# # Model 05


# Mushroom_Data_model5 <- neuralnet(f, data=MushroomData_train_m,
# 								 , algorithm = "rprop+" ,hidden = c(10,5,3),
# 								 threshold = 0.01, act.fct = "tanh")
# plot(Mushroom_Data_model5)
# Mushroom_Data_model5$result.matrix

# # Evaluating model performance
# model_results5 <- compute(Mushroom_Data_model5, MushroomData_test_m[,-c(1,98)])
# Predicted_class5 <- model_results5$net.result
# summary(model_results5)

# # cor() function is used to obtain a correlation between two numeric vectors
# cor(Predicted_class5, MushroomData_test_m[,98])

# # Model 06

# sigmoid = function(x) {
#   1 / (1 + exp(-x))
# }
# Mushroom_Data_model6 <- neuralnet(f, data=MushroomData_train_m,
# 								 , algorithm = "rprop+" ,hidden = c(12,6,3),
# 								 threshold = 0.01, act.fct = sigmoid)
# plot(Mushroom_Data_model6)
# Mushroom_Data_model6$result.matrix

# # Evaluating model performance
# model_results6 <- compute(Mushroom_Data_model6, MushroomData_test_m[,-c(1,98)])
# Predicted_class6 <- model_results6$net.result
# summary(model_results6)

# # cor() function is used to obtain a correlation between two numeric vectors
# cor(Predicted_class6, MushroomData_test_m[,98])