################################# Question 1
#install.packages(c("NbClust", "cluster", "factoextra"))
library(NbClust)
library(cluster)
library(factoextra)
library(ggplot2)

red_wine_data <- read.csv("winequality-red.csv")
summary(red_wine_data)

quality_data <- red_wine_data$quality 
#red_wine_data <- red_wine_data[, -ncol(red_wine_data)]  # Remove the 'quality' column
red_wine_data <- red_wine_data[, -which(names(red_wine_data) == "quality")]

### K-means
nb_clusters <- NbClust(data = red_wine_data, min.nc = 2, max.nc = 9, method = "kmeans") #The graphs showed a significant drop in Dindex values when moving from 2 to 4 or 5 clusters, which suggests that while adding clusters initially improves the model, the rate of improvement decreases after that point.
print(nb_clusters)
table(nb_clusters$Best.n[1, ]) # 3 clusters is the most frequently suggested option, with 10 methods recommending it.

optimal_clusters <- 4  
kmeans_result <- kmeans(red_wine_data, centers = optimal_clusters, nstart = 25)
print(kmeans_result)
# overfitting if 9 so we choose 4

# Centroids
kmeans_centroids <- kmeans_result$centers
red_wine_data$kmeans_cluster <- kmeans_result$cluster
print(red_wine_data$kmeans_cluster)

avg_quality_kmeans <- aggregate(quality_data, by = list(red_wine_data$kmeans_cluster), FUN = mean)
# Rename columns for clarity
colnames(avg_quality_kmeans) <- c("KMeans_Cluster", "Average_Quality")
print(avg_quality_kmeans)

# Display centroid coordinates
centroid_coordinates <- as.data.frame(kmeans_result$centers)
excluded_columns <- c("kmeans_cluster", "pam_cluster")
colnames(centroid_coordinates) <- names(red_wine_data)[!names(red_wine_data) %in% excluded_columns]
centroid_coordinates$cluster <- 1:optimal_clusters  # Add cluster number for reference

print("Centroid Coordinates for Each K-Means Cluster:")
print(centroid_coordinates)

# Visualize clusters using fviz_cluster()
fviz_cluster(
  kmeans_result, 
  data = red_wine_data[, -which(names(red_wine_data) == "kmeans_cluster")],  # Exclude the cluster column
  palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FF5733"),  # Adjust colors as needed
  geom = "point",
  ellipse.type = "convex",
  ggtheme = theme_bw()
) + 
  ggtitle("Cluster Plot Using K-Means") 

### K-medoids
pam_result <- pam(red_wine_data, k = optimal_clusters)
print(pam_result)

# Medoids
kmedoids_centroids <- pam_result$medoids
red_wine_data$pam_cluster <- pam_result$clustering
print(red_wine_data$pam_cluster)

avg_quality_pam <- aggregate(quality_data, by = list(red_wine_data$pam_cluster), FUN = mean)
colnames(avg_quality_pam) <- c("Pam_Cluster", "Average_Quality")
print(avg_quality_pam)

# Display medoid coordinates
medoid_coordinates <- as.data.frame(pam_result$medoids)
excluded_columns <- c("kmeans_cluster", "pam_cluster", "NA")
colnames(medoid_coordinates) <- names(red_wine_data)[!names(red_wine_data) %in% excluded_columns]
medoid_coordinates$cluster <- 1:optimal_clusters  # Add cluster number for reference

print("Medoid Coordinates for Each K-Means Cluster:")
print(medoid_coordinates)

# Visualize clusters using fviz_cluster()
fviz_cluster(
  pam_result, 
  data = red_wine_data[, -which(names(red_wine_data) == "pam_cluster")],  # Exclude the cluster column
  palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FF5733"),  # Adjust colors as needed
  geom = "point",
  ellipse.type = "convex",
  ggtheme = theme_bw()
) + 
  ggtitle("Cluster Plot Using K-Medoids")  # Add title here



################################# Question 2
#install.packages("caret")
#install.packages("Metrics")
library(ggplot2)
library(caret)
library(class)
library(Metrics)  # For performance metrics

### Load the dataset
white_wine_data <- read.csv("winequality-white.csv")
summary(white_wine_data)

### Fit the MLR model
mlr_model <- lm(quality ~ ., data = white_wine_data)
summary(mlr_model)  # Show summary to identify significant predictors

### Extract significant predictors (p-value < 0.05)
significant_predictors <- rownames(summary(mlr_model)$coefficients)[summary(mlr_model)$coefficients[, 4] < 0.05]
significant_predictors <- significant_predictors[significant_predictors != "(Intercept)"]
print(significant_predictors)

### Split the data into training (80%) and testing (20%) sets
train_index <- createDataPartition(white_wine_data$quality, p = 0.8, list = FALSE)
train_data <- white_wine_data[train_index, ]
test_data <- white_wine_data[-train_index, ]

### Subset data to include only significant predictors
train_data_significant <- train_data[, c(significant_predictors, "quality")]
test_data_significant <- test_data[, c(significant_predictors, "quality")]

### Normalize the data (excluding the target variable)
train_data_normalized <- as.data.frame(scale(train_data_significant[, -which(names(train_data_significant) == "quality")]))
train_data_normalized$quality <- train_data_significant$quality
test_data_normalized <- as.data.frame(scale(test_data_significant[, -which(names(test_data_significant) == "quality")]))
test_data_normalized$quality <- test_data_significant$quality

### Choice of k: 
# Range of k values to test
k_values <- 1:20
rmse_values <- numeric(length(k_values))
mae_values <- numeric(length(k_values))
mape_values <- numeric(length(k_values))

# Evaluate KNN for different k values
for (k in k_values) {
  knn_predictions <- knn(train = train_data_normalized[, -which(names(train_data_normalized) == "quality")],
                         test = test_data_normalized[, -which(names(test_data_normalized) == "quality")],
                         cl = train_data_normalized$quality,
                         k = k)
  
  knn_predictions_numeric <- as.numeric(knn_predictions)
  
  # Calculate performance metrics
  rmse_values[k] <- rmse(test_data$quality, knn_predictions_numeric)
  mae_values[k] <- mae(test_data$quality, knn_predictions_numeric)
  mape_values[k] <- mape(test_data$quality, knn_predictions_numeric)
}

### Create separate plots for RMSE, MAE, and MAPE

# RMSE Plot
ggplot(data.frame(k = k_values, RMSE = rmse_values), aes(x = k, y = RMSE)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "RMSE vs. Number of Neighbors (k)",
       x = "Number of Neighbors (k)",
       y = "Root Mean Squared Error (RMSE)") +
  theme_minimal()

# MAE Plot
ggplot(data.frame(k = k_values, MAE = mae_values), aes(x = k, y = MAE)) +
  geom_line(color = "green", size = 1) +
  geom_point(color = "orange", size = 2) +
  labs(title = "MAE vs. Number of Neighbors (k)",
       x = "Number of Neighbors (k)",
       y = "Mean Absolute Error (MAE)") +
  theme_minimal()

# MAPE Plot
ggplot(data.frame(k = k_values, MAPE = mape_values), aes(x = k, y = MAPE)) +
  geom_line(color = "purple", size = 1) +
  geom_point(color = "pink", size = 2) +
  labs(title = "MAPE vs. Number of Neighbors (k)",
       x = "Number of Neighbors (k)",
       y = "Mean Absolute Percentage Error (MAPE)") +
  theme_minimal()

### Final choice of k by interpreting graph (choose elbow value)
k <- 3
knn_predictions <- knn(train = train_data_normalized[, -which(names(train_data_normalized) == "quality")],
                       test = test_data_normalized[, -which(names(test_data_normalized) == "quality")],
                       cl = train_data_normalized$quality,
                       k = k)

# Convert KNN predictions to numeric
knn_predictions_numeric <- as.numeric(knn_predictions)

# Calculate performance metrics for KNN
knn_rmse <- rmse(test_data$quality, knn_predictions_numeric)
knn_mae <- mae(test_data$quality, knn_predictions_numeric)
knn_mape <- mape(test_data$quality, knn_predictions_numeric)

# Output performance metrics
cat("KNN Performance Metrics:\n")
cat("Lowest RMSE for k=3:", knn_rmse, "\n")
cat("Lowest MAE for k=3:", knn_mae, "\n")
cat("Lowest MAPE for k=3:", knn_mape, "\n")

### Compare with MLR predictions
mlr_predictions <- predict(mlr_model, newdata = test_data)

# Calculate performance metrics for MLR
mlr_rmse <- rmse(test_data$quality, mlr_predictions)
mlr_mae <- mae(test_data$quality, mlr_predictions)
mlr_mape <- mape(test_data$quality, mlr_predictions)

cat("MLR Performance Metrics:\n")
cat("RMSE:", mlr_rmse, "\n")
cat("MAE:", mlr_mae, "\n")
cat("MAPE:", mlr_mape, "\n")



################################# Question 3
#install.packages("ggplot2")
#install.packages("caret", dependencies = TRUE)

library(ggplot2)
library(caret)
library(class)
library(Metrics)
library(NbClust)
library(cluster)
library(factoextra)


### Load the dataset
white_wine_data <- read.csv("winequality-white.csv")

# Convert quality to a binary factor
white_wine_data$quality <- as.factor(ifelse(white_wine_data$quality <= 5, "Poor", "Good"))

train_index <- createDataPartition(white_wine_data$quality, p = 0.8, list = FALSE)
train_data <- white_wine_data[train_index, ]
test_data <- white_wine_data[-train_index, ]

train_data_normalized <- as.data.frame(scale(train_data[, -which(names(train_data) == "quality")]))
train_data_normalized$quality <- train_data$quality
test_data_normalized <- as.data.frame(scale(test_data[, -which(names(test_data) == "quality")]))
test_data_normalized$quality <- test_data$quality

k_values <- 1:20
accuracy_values <- numeric(length(k_values))

for (k in k_values) {
  # Make predictions using KNN
  knn_predictions <- knn(train = train_data_normalized[, -which(names(train_data_normalized) == "quality")],
                         test = test_data_normalized[, -which(names(test_data_normalized) == "quality")],
                         cl = train_data_normalized$quality,
                         k = k)
  
  # Calculate overall accuracy
  accuracy_values[k] <- mean(knn_predictions == test_data_normalized$quality)
}

# Plot accuracy for different k values
ggplot(data.frame(k = k_values, Accuracy = accuracy_values), aes(x = k, y = Accuracy)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "Accuracy vs. Number of Neighbors (k)",
       x = "Number of Neighbors (k)",
       y = "Accuracy") +
  theme_minimal()

k<- 5 # 2 peak values: 5 and 8 , overall accuracy is better by 0.002 for k=5

# Make predictions with the best k
final_knn_predictions <- knn(train = train_data_normalized[, -which(names(train_data_normalized) == "quality")],
                             test = test_data_normalized[, -which(names(test_data_normalized) == "quality")],
                             cl = train_data_normalized$quality,
                             k = k)

# Calculate overall accuracy
overall_accuracy <- mean(final_knn_predictions == test_data_normalized$quality)

# Calculate class-wise accuracies
confusion_matrix <- table(test_data_normalized$quality, final_knn_predictions)
class_accuracy <- diag(prop.table(confusion_matrix, margin = 1))

cat("Overall accuracy:", overall_accuracy, "\n")
cat("Accuracy for Poor quality:", class_accuracy["Poor"], "\n")
cat("Accuracy for Good quality:", class_accuracy["Good"], "\n")



######### Question 4 ----
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


##### Question 5 ----
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


##### Question 6----
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
