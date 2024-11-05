################### Question 1
library(dplyr)
library(caret)
library(ggplot2)

pima_data <- read.csv("Pima.csv")
summary(pima_data)

# Remove rows with zero or missing values
pima_clean <- pima_data %>%
  filter_all(all_vars(. != 0 & !is.na(.)))

# Set 'Outcome' as a factor (label)
pima_clean$Outcome <- as.factor(pima_clean$Outcome)

# Scale numeric predictors
pima_clean_scaled <- pima_clean %>%
  mutate(across(where(is.numeric), scale))

# Build the logistic regression model
logit_model <- glm(Outcome ~ ., data = pima_clean_scaled, family = "binomial")
summary(logit_model)

# Identify significant predictors (e.g., p-value < 0.05)
significant_predictors <- names(coef(logit_model))[which(summary(logit_model)$coefficients[,4] < 0.05)]
print(significant_predictors)




################### Question 2
#install.packages("earth")
library(dplyr)
library(caret)
library(earth)
library(Metrics)

wine_data <- read.csv("winequality-red.csv")

# Convert quality to numeric if it’s not already
wine_data$quality <- as.numeric(wine_data$quality)
summary(wine_data)

##MLR
# Fit a multiple linear regression model
mlr_model <- lm(quality ~ ., data = wine_data)

# Summary to identify significant predictors (p < 0.05)
summary(mlr_model)

# Select significant predictors (p-value < 0.05)
significant_predictors <- names(coef(mlr_model))[which(summary(mlr_model)$coefficients[,4] < 0.05)]
significant_predictors <- significant_predictors[significant_predictors != "(Intercept)"]
print(significant_predictors)

wine_significant <- wine_data %>% select(all_of(significant_predictors), quality)

# Split the data into training and testing sets
set.seed(123)
index <- createDataPartition(wine_significant$quality, p = 0.7, list = FALSE)
train_data <- wine_significant[index, ]
test_data <- wine_significant[-index, ]

##KNN
# Train and tune KNN model with cross-validation
train_control <- trainControl(method = "cv", number = 10)
knn_model <- train(quality ~ ., data = train_data, method = "knn", 
                   trControl = train_control, tuneLength = 10)

# Optimal K
best_k <- knn_model$bestTune$k
print(best_k)

#Plot
library(ggplot2)
ggplot(knn_model$results, aes(x = k, y = RMSE)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "KNN Model Performance with Varying K",
       x = "Number of Neighbors (K)",
       y = "RMSE") +
  theme_minimal()

# Predict on the test set
knn_predictions <- predict(knn_model, newdata = test_data)

##MARS
# Train and tune MARS model with cross-validation
mars_grid <- expand.grid(degree = 1:2, nprune = seq(2, 10, by = 2))
mars_model <- train(quality ~ ., data = train_data, method = "earth", 
                    tuneGrid = mars_grid, trControl = train_control)

# Best parameters for MARS
best_mars <- mars_model$bestTune
print(best_mars)

#Plot
# Extract the tuning results from the MARS model
mars_results <- mars_model$results

# Plot nprune vs. RMSE with different colors for each degree
library(ggplot2)
ggplot(mars_results, aes(x = nprune, y = RMSE, color = factor(degree))) +
  geom_line() +
  geom_point() +
  labs(title = "MARS Model Performance with Varying nprune and Degree",
       x = "Number of Terms (nprune)",
       y = "RMSE",
       color = "Degree") +
  theme_minimal()


# Predict on the test set
mars_predictions <- predict(mars_model, newdata = test_data)

##Performance
# Calculate performance metrics for KNN
knn_rmse <- rmse(test_data$quality, knn_predictions)
knn_mae <- mae(test_data$quality, knn_predictions)
knn_mape <- mape(test_data$quality, knn_predictions)

# Calculate performance metrics for MARS
mars_rmse <- rmse(test_data$quality, mars_predictions)
mars_mae <- mae(test_data$quality, mars_predictions)
mars_mape <- mape(test_data$quality, mars_predictions)

# Display results
cat("KNN Performance:\n", "RMSE:", knn_rmse, "\nMAE:", knn_mae, "\nMAPE:", knn_mape, "\n\n")
cat("MARS Performance:\n", "RMSE:", mars_rmse, "\nMAE:", mars_mae, "\nMAPE:", mars_mape, "\n")


################### Question 3
