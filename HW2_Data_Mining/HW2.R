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
library(ggplot2)
library(caret)
library(class)
library(Metrics)  

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