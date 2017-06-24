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
# # # Write a CSV file from HepatitisData
# # HepatitisData <- write.csv(HepatitisData, file = "HepatitisData.csv", row.names = FALSE)
