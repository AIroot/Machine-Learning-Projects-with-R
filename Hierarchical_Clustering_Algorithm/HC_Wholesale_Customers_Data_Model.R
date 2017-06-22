# Reference for data source (
# @misc{Lichman:2013 ,
# author = "M. Lichman",
# year = "2013",
# title = "{UCI} Machine Learning Repository",
# url = "http://archive.ics.uci.edu/ml",
# institution = "University of California, Irvine, School of Information and Computer Sciences" })

# Source of Data Set:-  UCI Repository- Wholesale customers Data (https://archive.ics.uci.edu/ml/datasets/Wholesale+customers)

# Hierarchical Cluster
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

# z-score standardize
WholesaleData_z <- as.data.frame(lapply(WholesaleData, scale))

# Check the z-score effect for outliers
boxplot(WholesaleData_z$Fresh~WholesaleData_z$Channel, main=" Fresh",ylab="Fresh",xlab="Channel")
boxplot(WholesaleData_z$Milk~WholesaleData_z$Channel, main=" Milk",ylab="Milk",xlab="Channel")
boxplot(WholesaleData_z$Grocery~WholesaleData_z$Channel, main=" Grocery",ylab="Grocery",xlab="Channel")
boxplot(WholesaleData_z$Frozen~WholesaleData_z$Channel, main="Frozen",ylab="Frozen",xlab="Channel")
boxplot(WholesaleData_z$Detergents_Paper~WholesaleData_z$Channel, main="Detergents_Paper",ylab="Detergents_Paper",xlab="Channel")
boxplot(WholesaleData_z$Delicassen~WholesaleData_z$Channel, main="Delicassen",ylab="Delicassen",xlab="Channel")


# Transformation - normalizing numeric data

normalize <- function(x) {
	return ((x - min(x)) / (max(x) - min(x)))
}

# Apply normalize function to each list elememt. 
WholesaleData_n <- as.data.frame(lapply(WholesaleData, normalize))
# Check the normalize effect for outliers
boxplot(WholesaleData_n$Fresh~WholesaleData_n$Channel, main=" Fresh",ylab="Fresh",xlab="Channel")
boxplot(WholesaleData_n$Milk~WholesaleData_n$Channel, main=" Milk",ylab="Milk",xlab="Channel")
boxplot(WholesaleData_n$Grocery~WholesaleData_n$Channel, main=" Grocery",ylab="Grocery",xlab="Channel")
boxplot(WholesaleData_n$Frozen~WholesaleData_n$Channel, main="Frozen",ylab="Frozen",xlab="Channel")
boxplot(WholesaleData_n$Detergents_Paper~WholesaleData_n$Channel, main="Detergents_Paper",ylab="Detergents_Paper",xlab="Channel")
boxplot(WholesaleData_n$Delicassen~WholesaleData_n$Channel, main="Delicassen",ylab="Delicassen",xlab="Channel")

# Normalizing / scaling the data won’t necessarily remove those outliers.
# removing the top 5 customers from each category
Remove_Outliers <- function (data,cols,n=5) { #Requires some data frame and the top N to remove
idx.to.remove <-integer(0) #Initialize a vector to hold customers being removed
for (c in cols){ # For every column in the data we passed to this function
col.order <-order(data[,c],decreasing=T) #Sort column "c" in descending order (bigger on top)
#Order returns the sorted index (e.g. row 15, 3, 7, 1, ...) rather than the actual values sorted.
idx <-head(col.order, n) #Take the first n of the sorted column C to
idx.to.remove <-union(idx.to.remove,idx) #Combine and de-duplicate the row ids that need to be removed
}
return(idx.to.remove) #Return the indexes of customers to be removed
}

Top_five <-Remove_Outliers(WholesaleData,cols=3:8,n=5)
length(Top_five) #How Many Customers to be Removed?
WholesaleData[Top_five,] #Examine the customers
Remove_Top_Five<-WholesaleData[-c(Top_five),] #Remove the Customers

# Visualize data set without outliers
boxplot(Remove_Top_Five$Fresh~Remove_Top_Five$Channel, main=" Fresh",ylab="Fresh",xlab="Channel")
boxplot(Remove_Top_Five$Milk~Remove_Top_Five$Channel, main=" Milk",ylab="Milk",xlab="Channel")
boxplot(Remove_Top_Five$Grocery~Remove_Top_Five$Channel, main=" Grocery",ylab="Grocery",xlab="Channel")
boxplot(Remove_Top_Five$Frozen~Remove_Top_Five$Channel, main="Frozen",ylab="Frozen",xlab="Channel")
boxplot(Remove_Top_Five$Detergents_Paper~Remove_Top_Five$Channel, main="Detergents_Paper",ylab="Detergents_Paper",xlab="Channel")
boxplot(Remove_Top_Five$Delicassen~Remove_Top_Five$Channel, main="Delicassen",ylab="Delicassen",xlab="Channel")




# # Data Visualization

for (i in c(3:8))
    hist(WholesaleData[,c(i)], breaks = 200, main = colnames(WholesaleData)[i], xlab="Annual Spending (m.u.)", ylab = "No. of Customers")


for (i in c(3:8))
    hist(Remove_Top_Five[,c(i)], breaks = 200, main = colnames(WholesaleData)[i], xlab="Annual Spending (m.u.)", ylab = "No. of Customers")



summary(Remove_Top_Five)

# Use normalize or z-score standardize
 
WholesaleData_n <- as.data.frame(lapply(Remove_Top_Five[3:8], scale))

summary(WholesaleData_n)


# Hierarchical Clustering on Different Feature
# require(fastcluster)
# require(graphics)
library(gplots)
library(cluster)
library(NbClust)

# Compute pairewise distance matrices
WholesaleData_Dist_n <- dist(WholesaleData_n,method = "euclidean")


# Use ward.D2 method in hclust()
WholesaleData_HC_WD <- hclust(WholesaleData_Dist_n, "ward.D2")
# Visualization of hclust
plot(WholesaleData_HC_WD, labels = FALSE, hang = -1)
# Add rectangle around 3 groups
rect.hclust(WholesaleData_HC_WD, k = 4, border = 2:4) 
cut.cluster_WD = cutree(WholesaleData_HC_WD, k = 4)
table(cut.cluster_WD)

# cutting hight = 20 
cut.cluster_WD_h20 = cutree(WholesaleData_HC_WD, h = 20.5)
table(cut.cluster_WD_h20)
identical(cut.cluster_WD, cut.cluster_WD_h20)
# cutting hight = 10 
cut.cluster_WD_h10 = cutree(WholesaleData_HC_WD, h = 10)
table(cut.cluster_WD_h10)
identical(cut.cluster_WD, cut.cluster_WD_h10)
# cutting hight = 5 
cut.cluster_WD_h5 = cutree(WholesaleData_HC_WD, h = 5)
table(cut.cluster_WD_h5)
identical(cut.cluster_WD, cut.cluster_WD_h5)

# Compare dendrograms using complete dissimilarity measure method
WholesaleData_HC_n <- hclust(WholesaleData_Dist_n,method="complete")
# Visualization of hclust
plot(WholesaleData_HC_n, labels = FALSE, hang = -1)
# Add rectangle around 3 groups
rect.hclust(WholesaleData_HC_n, k = 4, border = 2:4) 
cut.cluster_CO = cutree(WholesaleData_HC_n, k = 4)
table(cut.cluster_CO)

WholesaleData_HC_single <- hclust(WholesaleData_Dist_n,method="single")
# Visualization of hclust
plot(WholesaleData_HC_single, labels = FALSE, hang = -1)
# Add rectangle around 3 groups
rect.hclust(WholesaleData_HC_single, k = 4, border = 2:4) 
cut.cluster_single = cutree(WholesaleData_HC_single, k = 4)
table(cut.cluster_single)

WholesaleData_HC_average <- hclust(WholesaleData_Dist_n,method="average")
# Visualization of hclust
plot(WholesaleData_HC_average, labels = FALSE, hang = -1)
# Add rectangle around 3 groups
rect.hclust(WholesaleData_HC_average, k = 4, border = 2:4) 
cut.cluster_average = cutree(WholesaleData_HC_average, k = 4)
table(cut.cluster_average)

WholesaleData_HC_centroid <- hclust(WholesaleData_Dist_n,method="centroid")
# Visualization of hclust
plot(WholesaleData_HC_centroid, labels = FALSE, hang = -1)
# Add rectangle around 3 groups
rect.hclust(WholesaleData_HC_centroid, k = 4, border = 2:4)
cut.cluster_centroid = cutree(WholesaleData_HC_centroid, k = 4)
table(cut.cluster_centroid)

# Use ward.D2 method in hclust()
WholesaleData_HC_WD <- hclust(WholesaleData_Dist_n, "ward.D2")
# Visualization of hclust
plot(WholesaleData_HC_WD, labels = FALSE, hang = -1)
# Add rectangle around 3 groups
rect.hclust(WholesaleData_HC_WD, k = 5, border = 2:4) 
cut.cluster_WD = cutree(WholesaleData_HC_WD, k = 5)
table(cut.cluster_WD)

# cutting hight = 18 
cut.cluster_WD_h18 = cutree(WholesaleData_HC_WD, h = 18)
table(cut.cluster_WD_h18)
identical(cut.cluster_WD, cut.cluster_WD_h18)
# cutting hight = 10 
cut.cluster_WD_h10 = cutree(WholesaleData_HC_WD, h = 10)
table(cut.cluster_WD_h10)
identical(cut.cluster_WD, cut.cluster_WD_h10)
# cutting hight = 5 
cut.cluster_WD_h5 = cutree(WholesaleData_HC_WD, h = 5)
table(cut.cluster_WD_h5)
identical(cut.cluster_WD, cut.cluster_WD_h5)

# Compare dendrograms using complete dissimilarity measure method
WholesaleData_HC_n <- hclust(WholesaleData_Dist_n,method="complete")
# Visualization of hclust
plot(WholesaleData_HC_n, labels = FALSE, hang = -1)
# Add rectangle around 3 groups
rect.hclust(WholesaleData_HC_n, k = 5, border = 2:4) 
cut.cluster_CO = cutree(WholesaleData_HC_n, k = 5)
table(cut.cluster_CO)

WholesaleData_HC_single <- hclust(WholesaleData_Dist_n,method="single")
# Visualization of hclust
plot(WholesaleData_HC_single, labels = FALSE, hang = -1)
# Add rectangle around 3 groups
rect.hclust(WholesaleData_HC_single, k = 5, border = 2:4) 
cut.cluster_single = cutree(WholesaleData_HC_single, k = 5)
table(cut.cluster_single)

WholesaleData_HC_average <- hclust(WholesaleData_Dist_n,method="average")
# Visualization of hclust
plot(WholesaleData_HC_average, labels = FALSE, hang = -1)
# Add rectangle around 3 groups
rect.hclust(WholesaleData_HC_average, k = 5, border = 2:4) 
cut.cluster_average = cutree(WholesaleData_HC_average, k = 5)
table(cut.cluster_average)

WholesaleData_HC_centroid <- hclust(WholesaleData_Dist_n,method="centroid")
# Visualization of hclust
plot(WholesaleData_HC_centroid, labels = FALSE, hang = -1)
# Add rectangle around 3 groups
rect.hclust(WholesaleData_HC_centroid, k = 5, border = 2:4)
cut.cluster_centroid = cutree(WholesaleData_HC_centroid, k = 5)
table(cut.cluster_centroid)

