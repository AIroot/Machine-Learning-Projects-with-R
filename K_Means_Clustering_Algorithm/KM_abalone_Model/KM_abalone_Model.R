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