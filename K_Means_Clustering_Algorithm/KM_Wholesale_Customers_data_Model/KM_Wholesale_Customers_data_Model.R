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



# Identify Outliers in the data set; See Rplots.pdf for visualisation  
# Create boxplots for continuous variables
# par(mfrow=c(2,3))

boxplot(WholesaleData$Fresh~WholesaleData$Channel, main=" Fresh",ylab="Fresh",xlab="Channel")
boxplot(WholesaleData$Milk~WholesaleData$Channel, main=" Milk",ylab="Milk",xlab="Channel")
boxplot(WholesaleData$Grocery~WholesaleData$Channel, main=" Grocery",ylab="Grocery",xlab="Channel")
boxplot(WholesaleData$Frozen~WholesaleData$Channel, main="Frozen",ylab="Frozen",xlab="Channel")
boxplot(WholesaleData$Detergents_Paper~WholesaleData$Channel, main="Detergents_Paper",ylab="Detergents_Paper",xlab="Channel")
boxplot(WholesaleData$Delicassen~WholesaleData$Channel, main="Delicassen",ylab="Delicassen",xlab="Channel")

