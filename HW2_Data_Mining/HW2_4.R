#install.packages("e1071")
library(e1071)

data <- read.csv("Pima.csv")

# clean the data
data$Glucose[data$Glucose == 0] <- NA
data$BloodPressure[data$BloodPressure == 0] <- NA
data$SkinThickness[data$SkinThickness == 0] <- NA
data$Insulin[data$Insulin == 0] <- NA
data$BMI[data$BMI == 0] <- NA
# remove rows with NA
data <- na.omit(data)

outcome_labels <- ifelse(data$Outcome == 1, "Yes", "No")  # Yes/No labels
#exclude outcome column
data <- data[, -ncol(data)]

# fuzzy C-means 

min.nc <- 2  # Minimum number of clusters
max.nc <- 10  # Maximum number of clusters


CM= array(0, c(max.nc-min.nc+1, 2))
for (nc in min.nc : max.nc)
{ 
  fitcm=cmeans(data, centers=nc, m=2, verbose=TRUE, method="cmeans")
  CM[nc-(min.nc-1), 1]= fclustIndex(fitcm, data, index="xie.beni")
  CM[nc-(min.nc-1), 2]= fclustIndex(fitcm, data, index="fukuyama.sugeno")
}

# Find the optimal number of clusters based on the minimum index values
best_xie_beni <- which(CM[, 1] == min(CM[, 1])) + min.nc - 1
best_fukuyama <- which(CM[, 2] == min(CM[, 2])) + min.nc - 1

# Print the best cluster numbers
cat("Best number of clusters (Xie-Beni):", best_xie_beni, "\n")
cat("Best number of clusters (Fukuyama-Sugeno):", best_fukuyama, "\n")


# Perform fuzzy c-means clustering with the best number of clusters (from Xie-Beni)
optimal_clusters <- best_xie_beni[1]  # Using Xie-Beni index result
result <- cmeans(data, centers = optimal_clusters, m = 2, iter.max = 200, verbose = TRUE, method = "cmeans")

# Print the centroids
cat("\nCluster Centroids:\n")
print(result$centers)

# Create a contingency table between Outcome labels and cluster assignments
cat("\nContingency Table between Outcome Labels and Clusters:\n")
table(outcome_labels, result$cluster)

# Calculate Euclidean distances between all pairs of centroids
cat("\nEuclidean Distances between Centroids:\n")
centroids <- result$centers

# Function to calculate Euclidean distance
euclidean_distance <- function(x, y) {
  sqrt(sum((x - y) ^ 2))
}

# Calculate and print the distances
for (i in 1:(nrow(centroids) - 1)) {
  for (j in (i + 1):nrow(centroids)) {
    cat(sprintf("Distance between centroid %d and %d: %.4f\n", i, j,
                euclidean_distance(centroids[i, ], centroids[j, ])))
  }
}


# Perform fuzzy c-means clustering with the best number of clusters (from fukuyama)
optimal_clusters2 <- best_fukuyama[1]  # Using fukuyama index result
result2 <- cmeans(data, centers = optimal_clusters2, m = 2, iter.max = 200, verbose = TRUE, method = "cmeans")

# Print the centroids
cat("\nCluster Centroids:\n")
print(result2$centers)

# Create a contingency table between Outcome labels and cluster assignments
cat("\nContingency Table between Outcome Labels and Clusters:\n")
table(outcome_labels, result2$cluster)

# Calculate Euclidean distances between all pairs of centroids
cat("\nEuclidean Distances between Centroids:\n")
centroids2 <- result2$centers

# Calculate and print the distances
for (i in 1:(nrow(centroids2) - 1)) {
  for (j in (i + 1):nrow(centroids2)) {
    cat(sprintf("Distance between centroid %d and %d: %.4f\n", i, j,
                euclidean_distance(centroids2[i, ], centroids2[j, ])))
  }
}


