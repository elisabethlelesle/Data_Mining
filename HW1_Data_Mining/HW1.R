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
<<<<<<< HEAD
print(p)


## Question 2
##install.packages("isotree")
library(isotree)

# Load the dataset
pima <- read.csv("Pima.csv", stringsAsFactors = TRUE)
plot(pima) # Visualization

### Outlier test based on Mahalanobis distance
mean_vector <- colMeans(pima[, -ncol(pima)])  # Exclude the last column if it's a factor (Outcome)
mean_vector

cov_matrix <- cov(pima[, -ncol(pima)], use = "pairwise")
cov_matrix

pima$mdis <- mahalanobis(pima[, -ncol(pima)], mean_vector, cov_matrix)

# Chi-square test for 10% threshold
threshold <- qchisq(df = ncol(pima) - 1, p = 0.9)  # df: degrees of freedom
pima$maout <- (pima$mdis > threshold)

# Identify indices of Mahalanobis outliers
mahalanobis_outliers <- which(pima$maout == TRUE)
cat("Mahalanobis Outliers Indices:", mahalanobis_outliers, "\n")

### Outlier test based on Isolation Forest
iso_model <- isolation.forest(pima[, -ncol(pima)], ntrees = 100, nthreads = -1)
predictions <- predict(iso_model, pima[, -ncol(pima)])

# Identify data with highest outlier score
pima[which.max(predictions),]  # Data with the highest outlier score
isolation_forest_outliers <- which(predictions > 0.6)

cat("Isolation Forest Outliers Indices:", isolation_forest_outliers, "\n")

### Comparing Results

# Common outliers
common_outliers <- intersect(mahalanobis_outliers, isolation_forest_outliers)
cat("Common Outliers Indices:", common_outliers, "\n")

# Differences
unique_mahalanobis <- setdiff(mahalanobis_outliers, isolation_forest_outliers)
unique_isolation_forest <- setdiff(isolation_forest_outliers, mahalanobis_outliers)

cat("Unique Mahalanobis Outliers Indices:", unique_mahalanobis, "\n")
cat("Unique Isolation Forest Outliers Indices:", unique_isolation_forest, "\n")



## Question 4

library(arules)
