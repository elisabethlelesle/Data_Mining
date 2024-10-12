#install.packages("mclust")
library(mclust) #BIC: Bayesian information criteria

data <- read.csv("Pima.csv")

# clean the data
data$Glucose[data$Glucose == 0] <- NA
data$BloodPressure[data$BloodPressure == 0] <- NA
data$SkinThickness[data$SkinThickness == 0] <- NA
data$Insulin[data$Insulin == 0] <- NA
data$BMI[data$BMI == 0] <- NA
# remove rows with NA
data <- na.omit(data)

outcome_labels <- ifelse(data$Outcome == 1, "Yes", "No")
#exclude outcome column
data <- data[, -ncol(data)]

### Perform Gaussian Mixture Clustering (GMC)
fitEM <- Mclust(data)  # Fit GMM model to the dataset
# 1. Show the best-fitted number of clusters
cat("Best number of clusters (based on BIC):", fitEM$G, "\n")

# 2. Associated centroids (means) and covariances
cat("\nCluster Centroids (Means):\n")
print(fitEM$parameters$mean)

cat("\nCovariance Matrices for Each Cluster:\n")
print(fitEM$parameters$variance$sigma)

# 3. Create a table between Outcome labels and cluster assignments
cat("\nContingency Table between Outcome Labels and Cluster Assignments:\n")
print(table(outcome_labels, fitEM$classification))

# 4. Calculate Mahalanobis distances between pairs of centroids
cat("\nMahalanobis Distances between Centroids:\n")

# Function to compute Mahalanobis distance between two centroids
mahalanobis_distance <- function(mean1, mean2, cov_matrix) {
  diff <- mean1 - mean2
  sqrt(t(diff) %*% solve(cov_matrix) %*% diff)
}

# Calculate and print Mahalanobis distances for all pairs of centroids
centroids <- fitEM$parameters$mean
for (i in 1:(ncol(centroids) - 1)) {
  for (j in (i + 1):ncol(centroids)) {
    dist <- mahalanobis_distance(
      centroids[, i], centroids[, j], 
      fitEM$parameters$variance$sigma[, , i]
    )
    cat(sprintf("Mahalanobis distance between centroid %d and %d: %.4f\n", i, j, dist))
  }
}
