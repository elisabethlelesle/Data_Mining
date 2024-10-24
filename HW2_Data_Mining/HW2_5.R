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

# 4. Calculate Mahalanobis distances between each point and its assigned cluster centroid
cat("\nAverage Mahalanobis Distances within Each Group:\n")

# Function to compute Mahalanobis distance between a data point and a centroid
mahalanobis_distance_point <- function(point, mean, cov_matrix) {
  diff <- point - mean
  sqrt(t(diff) %*% solve(cov_matrix) %*% diff)
}

# Calculate average Mahalanobis distance for each group (cluster)
centroids <- fitEM$parameters$mean
covariances <- fitEM$parameters$variance$sigma
cluster_assignments <- fitEM$classification
num_clusters <- fitEM$G

# Loop through each cluster and compute average Mahalanobis distance
for (cluster in 1:num_clusters) {
  # Get points assigned to this cluster
  points_in_cluster <- data[cluster_assignments == cluster, ]
  
  # Get the corresponding centroid and covariance matrix for this cluster
  centroid <- centroids[, cluster]
  covariance <- covariances[, , cluster]
  
  # Compute Mahalanobis distance for each point in the cluster
  distances <- apply(points_in_cluster, 1, mahalanobis_distance_point, mean = centroid, cov_matrix = covariance)
  
  # Calculate the average Mahalanobis distance for this cluster
  avg_distance <- mean(distances)
  cat(sprintf("Average Mahalanobis distance for cluster %d: %.4f\n", cluster, avg_distance))
}
