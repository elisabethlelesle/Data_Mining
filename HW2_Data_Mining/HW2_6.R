# Install and load clusterSim if needed
# install.packages("clusterSim")
#install.packages("R2HTML")
library(clusterSim)

# Load the gender_outlier dataset
data <- read.csv("gender_outlier.csv")

# Separate features and labels
data_features <- data[, -ncol(data)]  # Features
gender_labels <- data[, ncol(data)]   # Gender (Male/Female)

# Define the range of clusters (2 to 9)
min.nc <- 2
max.nc <- 9

# Initialize an array to store DB index values for different linkage methods
HC <- array(0, c(max.nc - min.nc + 1, 4))  # 4 methods: single, complete, average, ward

# Perform hierarchical clustering using different linkage methods
for (nc in min.nc:max.nc) {
  
  # 1. Single Linkage Method
  fithc_single <- hclust(dist(data_features), method = "single")
  ct_single <- cutree(fithc_single, k = nc)
  HC[nc - min.nc + 1, 1] <- index.DB(data_features, ct_single, centrotypes = "centroids")$DB
  
  # 2. Complete Linkage Method
  fithc_complete <- hclust(dist(data_features), method = "complete")
  ct_complete <- cutree(fithc_complete, k = nc)
  HC[nc - min.nc + 1, 2] <- index.DB(data_features, ct_complete, centrotypes = "centroids")$DB
  
  # 3. Group Average Linkage Method
  fithc_average <- hclust(dist(data_features), method = "average")
  ct_average <- cutree(fithc_average, k = nc)
  HC[nc - min.nc + 1, 3] <- index.DB(data_features, ct_average, centrotypes = "centroids")$DB
  
  # 4. Ward's Method
  fithc_ward <- hclust(dist(data_features), method = "ward.D")
  ct_ward <- cutree(fithc_ward, k = nc)
  HC[nc - min.nc + 1, 4] <- index.DB(data_features, ct_ward, centrotypes = "centroids")$DB
}

# Find the best number of clusters based on the minimum DB index for each method
best_single <- which.min(HC[, 1]) + min.nc - 1
best_complete <- which.min(HC[, 2]) + min.nc - 1
best_average <- which.min(HC[, 3]) + min.nc - 1
best_ward <- which.min(HC[, 4]) + min.nc - 1

# Print the best number of clusters for each method
cat("Best number of clusters (Single Link):", best_single, "\n")
cat("Best number of clusters (Complete Link):", best_complete, "\n")
cat("Best number of clusters (Group Average):", best_average, "\n")
cat("Best number of clusters (Ward's Method):", best_ward, "\n")



#Use single method with the best number of clusters
best_clusters_1 <- cutree(fithc_ward, k = best_single)

# Create a contingency table to show gender distribution in each cluster
cat("\nContingency Table (single Method):\n")
gender_cluster_table_1 <- table(gender_labels, best_clusters_1)
print(gender_cluster_table_1)

# Determine the majority class (Male or Female) in each cluster
majority_class_1 <- apply(gender_cluster_table_1, 2, function(col) {
  if (col["female"] > col["male"]) {
    return("Female")
  } else {
    return("Male")
  }
})

cat("\nMajority class in each cluster (single Method):\n")
print(majority_class_1)




#Use complete method with the best number of clusters
best_clusters_2 <- cutree(fithc_ward, k = best_complete)

# Create a contingency table to show gender distribution in each cluster
cat("\nContingency Table (complete Method):\n")
gender_cluster_table_2 <- table(gender_labels, best_clusters_2)
print(gender_cluster_table_2)

# Determine the majority class (Male or Female) in each cluster
majority_class_2 <- apply(gender_cluster_table_2, 2, function(col) {
  if (col["female"] > col["male"]) {
    return("Female")
  } else {
    return("Male")
  }
})

cat("\nMajority class in each cluster (complete Method):\n")
print(majority_class_2)





#Use average method with the best number of clusters
best_clusters_3 <- cutree(fithc_ward, k = best_average)

# Create a contingency table to show gender distribution in each cluster
cat("\nContingency Table (average Method):\n")
gender_cluster_table_3 <- table(gender_labels, best_clusters_3)
print(gender_cluster_table_3)

# Determine the majority class (Male or Female) in each cluster
majority_class_3 <- apply(gender_cluster_table_3, 2, function(col) {
  if (col["female"] > col["male"]) {
    return("Female")
  } else {
    return("Male")
  }
})

cat("\nMajority class in each cluster (average Method):\n")
print(majority_class_3)





#Use Ward's method with the best number of clusters
best_clusters_4 <- cutree(fithc_ward, k = best_ward)

# Create a contingency table to show gender distribution in each cluster
cat("\nContingency Table (Ward's Method):\n")
gender_cluster_table_4 <- table(gender_labels, best_clusters_4)
print(gender_cluster_table_4)

# Determine the majority class (Male or Female) in each cluster
majority_class_4 <- apply(gender_cluster_table_4, 2, function(col) {
  if (col["female"] > col["male"]) {
    return("Female")
  } else {
    return("Male")
  }
})

cat("\nMajority class in each cluster (Ward's Method):\n")
print(majority_class_4)
