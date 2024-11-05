################### Question 1
library(dplyr)
library(caret)
library(ggplot2)

pima_data <- read.csv("Pima.csv")
summary(pima_data)

# Remove rows with zero or missing values
pima_clean <- pima_data %>%
  filter_at(vars(-Outcome), all_vars(. != 0 & !is.na(.)))


# Set 'Outcome' as a factor (label)
pima_clean$Outcome <- as.factor(pima_clean$Outcome)

# Build the logistic regression model
logit_model <- glm(Outcome ~ ., data = pima_clean, family = "binomial")
summary(logit_model)

# Identify significant predictors (e.g., p-value < 0.05)
significant_predictors <- names(coef(logit_model))[which(summary(logit_model)$coefficients[,4] < 0.05)]
significant_predictors <- significant_predictors[significant_predictors != "(Intercept)"]
print(significant_predictors)

pima_significant <- pima_data[, c(significant_predictors, "Outcome")]

##KNN

# Define training and test sets
set.seed(123)
index <- createDataPartition(pima_significant$Outcome, p = 0.7, list = FALSE)
train_data <- pima_significant[index, ]
test_data <- pima_significant[-index, ]

train_data$Outcome <- factor(train_data$Outcome, levels = c(0, 1))

# Train and tune KNN model with cross-validation
train_control <- trainControl(method = "cv", number = 10)
knn_model <- train(Outcome ~ ., data = train_data, method = "knn", 
                   trControl = train_control, tuneLength = 10)

# Check best K value
#best_k <- knn_model$bestTune$k
#print(best_k)

# Predict on the test set
knn_predictions <- predict(knn_model, newdata = test_data)


#Plot
library(ggplot2)
ggplot(knn_model$results, aes(x = k, y = Accuracy)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "KNN Model Performance with Varying K",
       x = "Number of Neighbors (K)",
       y = "Accuracy") +
  theme_minimal()
#best k = 7

##Naive Bayes
# Train Naive Bayes model
nb_model <- naiveBayes(Outcome ~ ., data = train_data)

# Predict on the test set
nb_predictions <- predict(nb_model, newdata = test_data)


##Performance
# KNN performance metrics
knn_conf_matrix <- confusionMatrix(knn_predictions, test_data$Outcome)
knn_accuracy <- knn_conf_matrix$overall["Accuracy"]
knn_precision <- knn_conf_matrix$byClass["Precision"]
knn_recall <- knn_conf_matrix$byClass["Recall"]
knn_f1 <- 2 * ((knn_precision * knn_recall) / (knn_precision + knn_recall))

# Naive Bayes performance metrics
nb_conf_matrix <- confusionMatrix(nb_predictions, test_data$Outcome)
nb_accuracy <- nb_conf_matrix$overall["Accuracy"]
nb_precision <- nb_conf_matrix$byClass["Precision"]
nb_recall <- nb_conf_matrix$byClass["Recall"]
nb_f1 <- 2 * ((nb_precision * nb_recall) / (nb_precision + nb_recall))

# Print performance comparison
cat("KNN Performance:\n", "Accuracy:", knn_accuracy, "\nPrecision:", knn_precision,
    "\nRecall:", knn_recall, "\nF1 Score:", knn_f1, "\n\n")
cat("Naive Bayes Performance:\n", "Accuracy:", nb_accuracy, "\nPrecision:", nb_precision,
    "\nRecall:", nb_recall, "\nF1 Score:", nb_f1, "\n")


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
library(dplyr)
library(caret)
library(e1071)

bank_data <- read.csv("bank.csv", sep = ";")

# Remove specified columns (job, month, day)
#bank_data <- bank_data %>% select(-job, -month, -day)
bank_data <- bank_data[, !(names(bank_data) %in% c("job", "month", "day"))]

# Convert "y" to a factor (label)
bank_data$y <- as.factor(bank_data$y)
summary(bank_data)

##Logistic regression model
logit_model <- glm(y ~ ., data = bank_data, family = "binomial"(link="logit"))
summary(logit_model)

# Select significant predictors (p < 0.05)
significant_predictors <- names(coef(logit_model))[which(summary(logit_model)$coefficients[,4] < 0.05)]
significant_predictors <- significant_predictors[significant_predictors != "(Intercept)"]
print(significant_predictors)

##Derive the probability respectively for y-yes or y-no by including a new sample
# Define a function to calculate mode
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Create a new sample with mode for discrete and median for numeric features
#new_sample <- bank_data %>%
#  summarise(across(where(is.numeric), median), 
#            across(where(is.factor), ~ get_mode(.)))
new_sample <- bank_data %>%
  summarise(
    age = median(age, na.rm = TRUE),
    marital = Mode(marital),
    education = Mode(education),
    default = Mode(default),
    balance = median(balance, na.rm = TRUE),
    housing = Mode(housing),
    loan = Mode(loan),
    contact = Mode(contact),
    duration = median(duration, na.rm = TRUE),
    campaign = median(campaign, na.rm = TRUE),
    pdays = median(pdays, na.rm = TRUE),
    previous = median(previous, na.rm = TRUE),
    poutcome = Mode(poutcome)
  )

# Predict probability for new sample
probability <- predict(logit_model, newdata = new_sample, type = "response")
cat("Probability for y=yes:", probability, "\n")
cat("Probability for y=no:", 1 - probability, "\n")

###Naive Bayes
##Using all predictors
nb_model_all <- naiveBayes(y ~ ., data = bank_data)

# Predict on training set
nb_predictions_all <- predict(nb_model_all, bank_data)

# Confusion matrix and accuracy for all predictors
conf_matrix_all <- confusionMatrix(nb_predictions_all, bank_data$y)
overall_accuracy_all <- conf_matrix_all$overall["Accuracy"]
accuracy_yes_all <- conf_matrix_all$byClass["Sensitivity"]
accuracy_no_all <- conf_matrix_all$byClass["Specificity"]

cat("Naïve Bayes with All Predictors:\n",
    "Overall Accuracy:", overall_accuracy_all, "\n",
    "Accuracy for y-yes:", accuracy_yes_all, "\n",
    "Accuracy for y-no:", accuracy_no_all, "\n\n")

##Using partial predictors
sig_predictors <- c("marital", "education", "housing", "loan", "contact", "duration", "campaign", "poutcome")

bank_partial <- bank_data[, c(sig_predictors, "y")]

# Predict on training set
nb_predictions_partial <- predict(nb_model_partial, bank_partial)

# Confusion matrix and accuracy for partial predictors
conf_matrix_partial <- confusionMatrix(nb_predictions_partial, bank_data$y)
overall_accuracy_partial <- conf_matrix_partial$overall["Accuracy"]
accuracy_yes_partial <- conf_matrix_partial$byClass["Sensitivity"]
accuracy_no_partial <- conf_matrix_partial$byClass["Specificity"]

cat("Naïve Bayes with Partial Predictors:\n",
    "Overall Accuracy:", overall_accuracy_partial, "\n",
    "Accuracy for y-yes:", accuracy_yes_partial, "\n",
    "Accuracy for y-no:", accuracy_no_partial, "\n")
