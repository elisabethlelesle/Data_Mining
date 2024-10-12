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

