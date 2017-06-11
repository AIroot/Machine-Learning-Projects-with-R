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

# Read the url html file into a data frame titled LetterData.
MushroomData <- read.csv(MushroomDataUrl, header=FALSE)

# Assigning attributes information 
colnames(MushroomData) <- c("class", "cap_shape", "cap_surface", "cap_color", "bruises", "odor", "gill_attachment", "gill_spacing", "gill_size", "gill_color", "stalk_shape", "stalk_root", "stalk_surface_above_ring", "stalk_surface_below_ring", "stalk_color_above_ring", "stalk_color_below_ring", "veil_type", "veil_color", "ring_number", "ring_type", "spore_print_color", "population", "habitat")


# Write a CSV file from LetterData
MushroomData <- write.csv(MushroomData, file = "MushroomData.csv", row.names = FALSE)

MushroomData <- read.csv("MushroomData.csv", header= TRUE)

str(MushroomData)

