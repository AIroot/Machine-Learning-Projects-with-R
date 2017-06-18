# Reference for data source (
# @misc{Lichman:2013 ,
# author = "M. Lichman",
# year = "2013",
# title = "{UCI} Machine Learning Repository",
# url = "http://archive.ics.uci.edu/ml",
# institution = "University of California, Irvine, School of Information and Computer Sciences" })

# Source of Data Set:-  UCI Repository- Wholesale customers Data (https://archive.ics.uci.edu/ml/datasets/Wholesale+customers)

# K-Means clustering 

# # Collecting data
WholesaleData <- read.csv("Wholesale_customers_data.csv", header= TRUE)

# Explore the data
str(WholesaleData)

# #Identify missing values using graphical view. See the Rplot.pdf and red colour stripes indicate the missing values.  
# library(Amelia)
# missmap(WholesaleData, main="Missing Data - Wholesale", col=c("red","grey"), legend=FALSE)
