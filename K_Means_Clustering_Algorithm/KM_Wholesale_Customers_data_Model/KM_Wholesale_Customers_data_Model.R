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

# Train the model
# Use R default stats package
library(stats)
library(cluster)
library(NbClust)

# The kmeans() function requires a data frame containing only numeric data
# and a parameter specifying the desired number of clusters.


# Apply kmeans() functions for normalizing numeric data
set.seed(2345)

WholesaleData_Clusters_N <- kmeans(WholesaleData_n, 4)


# Visualize k-means clusters
clusplot(WholesaleData_n, WholesaleData_Clusters_N$cluster, color=TRUE, shade=TRUE,labels=2, lines=0)
points(WholesaleData_Clusters_N$centers, pch=3, cex=2) 

# Centroid plot 
library(fpc)
plotcluster(WholesaleData_n, WholesaleData_Clusters_N$cluster)


# Evaluating model performance
# The basic ways to evaluate the cluster is size of cluster. 
WholesaleData_Clusters_N$size 

# Check the coordinates of the cluster centroids 
WholesaleData_Clusters_N$centers


# Improving model
# Add clusters to the data frame
Remove_Top_Five$cluster <- WholesaleData_Clusters_N$cluster

# Check how the cluster relates to channel and region characteristics
Remove_Top_Five[1:5, c("cluster", "Channel", "Region")] 

# Use aggregate() functions
aggregate(data = Remove_Top_Five, Channel ~ cluster, mean)

# use aggregate() to check cluster and feamales
aggregate(data = Remove_Top_Five, Region ~ cluster, mean)



# Elbow method for determining the optimal number of clusters
set.seed(123)
# Compute and plot wss for k = 2 to k = 15
k.max <- 15 # Maximal number of clusters
wss <- sapply(1:k.max, 
        function(k){kmeans(WholesaleData_n, k, nstart=10 )$tot.withinss})
plot(1:k.max, wss,
       type="b", pch = 19, frame = FALSE, 
       xlab="Number of clusters K",
       ylab="Total within-clusters sum of squares")
abline(v = 5, lty =2)

# Apply kmeans() functions for normalizing numeric data with optimal K=5
set.seed(2345)

WholesaleData_Clusters <- kmeans(WholesaleData_n, 5)


# Visualize k-means clusters
clusplot(WholesaleData_n, WholesaleData_Clusters$cluster, color=TRUE, shade=TRUE,labels=2, lines=0)
points(WholesaleData_Clusters_N$centers, pch=3, cex=2) 

# Centroid plot 
library(fpc)
plotcluster(WholesaleData_n, WholesaleData_Clusters$cluster)


# Evaluating model performance
# The basic ways to evaluate the cluster is size of cluster. 
WholesaleData_Clusters$size 

# Check the coordinates of the cluster centroids 
WholesaleData_Clusters$centers


# Improving model
# Add clusters to the data frame
Remove_Top_Five$cluster <- WholesaleData_Clusters$cluster

# Check how the cluster relates to channel and region characteristics
Remove_Top_Five[1:5, c("cluster", "Channel", "Region")] 

# Use aggregate() functions
aggregate(data = Remove_Top_Five, Channel ~ cluster, mean)

# use aggregate() to check cluster and feamales
aggregate(data = Remove_Top_Five, Region ~ cluster, mean)

