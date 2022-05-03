library(ggplot2)
library(dplyr)
library(devtools)
library(ggbiplot)


# ------- df initialization -------

# Read in file, replace with pathname of file on local machine 
data <- read.csv("/Users/andresbonilla/Desktop/PASC 14 most comon SX.csv", row.names = NULL)

# Replace N/A data with 0 
data[is.na(data)] <- 0

# ------- gender -------

# Create two dataframes, filtering by gender 
maledata <- filter(data, Gender == "Male")
femaledata <- filter(data, Gender == "Female")

# Sort data for plot with both genders
gendersorteddata <- data[order(data$Gender),]

# Principal component data for columns 9-22, which contain symptom information
maledata.pca <- prcomp(maledata[,c(9:22)], center = TRUE, scale. = TRUE)
femaledata.pca <- prcomp(femaledata[,c(9:22)], center = TRUE, scale. = TRUE)

# Principal component with columns 9-22 for combined gender 
gendersorteddata.pca <- prcomp(gendersorteddata[,c(9:22)], center = TRUE, scale. = TRUE)

# Optional summary 
summary(maledata.pca)
summary(femaledata.pca)

# Create groups needed for PCA ellipses, rep repeats the instance of the string a specified number of times 
maledata.gender <- c(rep("Male", 55))
femaledata.gender <- c(rep("Female", 79))

# Plot the data 
ggbiplot(maledata.pca,ellipse=TRUE, groups=maledata.gender) + theme_minimal()
ggbiplot(femaledata.pca,ellipse=TRUE, groups=femaledata.gender) + theme_minimal()





# As data is already sorted, groups can be constructed manually with rep. Otherwise, would need to copy the column values into a list
gendersorteddata.gender <- c(rep("Female", 79), rep("Male", 55))

# Plot 
ggbiplot(gendersorteddata.pca, ellipse = TRUE, groups = gendersorteddata.gender) + theme_minimal()

# ------- age -------

# Sort data by age
agesorteddata <- data[order(data$Age..years.),]

# Filter data by age groups 
group1 <- filter(data, Age..years. < 48)
group2 <- filter(data, Age..years. >= 48)

# Calculate principal component info 
group1.pca <- prcomp(group1[,c(9:22)], center = TRUE, scale. = TRUE)
group2.pca <- prcomp(group2[,c(9:22)], center = TRUE, scale. = TRUE)

# Principal component for combined age data 
agesorteddata.pca <- prcomp(agesorteddata[,c(9:22)], center = TRUE, scale. = TRUE)

# Create groups manually as data is already sorted 
group1.age <- c(rep("20-47", 70))
group2.age <- c(rep("48-88", 64))
agesorteddata.age <- c(rep("20-47", 70), rep("48-88", 64))

# Plot data
ggbiplot(group1.pca, ellipse = TRUE, groups = group1.age) + theme_minimal()
ggbiplot(group2.pca, ellipse = TRUE, groups = group2.age) + theme_minimal()
ggbiplot(agesorteddata.pca, ellipse = TRUE, groups = agesorteddata.age) + theme_minimal()


