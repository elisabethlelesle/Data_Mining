## Question 1

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(MASS)
library(stats)

# Load the dataset
data <- read.csv("gender_outlier.csv")
plot(data)

# Check structure of the data
str(data$Height)


# Display histograms w.r.t. height, weight, and waist
par(mfrow=c(1,3))  # Set up the plotting area
hist(data$Height, main="Height Histogram", xlab="Height (cm)", col="lightblue", border="black")
hist(data$Weight, main="Weight Histogram", xlab="Weight (kg)", col="lightgreen", border="black")
hist(data$Waist, main="Waist Histogram", xlab="Waist (cm)", col="lightcoral", border="black")

# Box plots to identify outliers
par(mfrow=c(1,3))  # Set up the plotting area
boxplot(data$Height, main="Height Box Plot", horizontal=TRUE, col="lightblue")
boxplot(data$Weight, main="Weight Box Plot", horizontal=TRUE, col="lightgreen")
boxplot(data$Waist, main="Waist Box Plot", horizontal=TRUE, col="lightcoral")

# Multi-dimensional scaling
# Prepare the data for MDS
print(colnames(data))
mds_data <- data[c("Height", "Weight", "Waist")]

# Calculate the distance matrix using Euclidean distance
dist_matrix <- dist(mds_data, method = 'euclidean')

# Perform MDS
mds_result <- cmdscale(dist_matrix, k = 2, eig = TRUE)

# Extract MDS coordinates
mdsx <- mds_result$points[, 1]
mdsy <- mds_result$points[, 2]

# Create a data frame for ggplot
mds_df <- data.frame(MDS1 = mdsx, MDS2 = mdsy, Gender = factor(data$Gender))

# Base R plot for MDS
plot(mdsx, mdsy, pch = as.numeric(mds_df$Gender), 
     col = as.numeric(mds_df$Gender) + 1, 
     main = "MDS for Gender Data", 
     xlab = "MDS Dimension 1", 
     ylab = "MDS Dimension 2")

# ggplot for MDS
p <- ggplot(mds_df, aes(x = MDS1, y = MDS2)) +
  geom_point(size = 3, aes(colour = Gender, shape = Gender)) +
  labs(title = "MDS for Gender Data", x = "MDS Dimension 1", y = "MDS Dimension 2") +
  theme_minimal()

# Display the ggplot
print(p)
