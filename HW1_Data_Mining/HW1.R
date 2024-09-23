## Question 1

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(MASS)

# Load the dataset
data <- read.csv("gender_outlier.csv")

# Display histograms for height, weight, and waist
par(mfrow=c(1,3))  # Set up the plotting area
hist(data$height, main="Height Histogram", xlab="Height (cm)", col="lightblue", border="black")
hist(data$weight, main="Weight Histogram", xlab="Weight (kg)", col="lightgreen", border="black")
hist(data$waist, main="Waist Histogram", xlab="Waist (cm)", col="lightcoral", border="black")

# Box plots to identify outliers
par(mfrow=c(1,3))  # Set up the plotting area
boxplot(data$height, main="Height Box Plot", horizontal=TRUE, col="lightblue")
boxplot(data$weight, main="Weight Box Plot", horizontal=TRUE, col="lightgreen")
boxplot(data$waist, main="Waist Box Plot", horizontal=TRUE, col="lightcoral")

# Multi-dimensional scaling
# Prepare the data for MDS
mds_data <- data %>% select(height, weight, waist)
mds_result <- isoMDS(dist(mds_data))

# Create a data frame for MDS results
mds_df <- data.frame(MDS1 = mds_result$points[,1],
                     MDS2 = mds_result$points[,2],
                     Gender = factor(data$gender))

# Plotting the MDS results
ggplot(mds_df, aes(x=MDS1, y=MDS2, color=Gender, shape=Gender)) +
  geom_point(size=3) +
  labs(title="Multi-Dimensional Scaling of Gender Data",
       x="MDS Dimension 1",
       y="MDS Dimension 2") +
  theme_minimal() +
  scale_color_manual(values=c("blue", "red"))  # Change colors as needed
