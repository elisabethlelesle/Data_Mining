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
test_data$Outcome <- factor(test_data$Outcome, levels = c(0, 1))

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
#best k = 15

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


################### Question 4
library(earth)
library(ggplot2)
library(dplyr)
library(lubridate)

# Read the data
data <- read.csv("container.csv")
data <- na.omit(data)

# Convert Date to Date format and extract Year
data$Date <- as.Date(data$Date, format = "%Y/%m/%d")
data$Year <- year(data$Date)

# Split data into training and testing sets
train_data <- subset(data, Year >= 2011 & Year <= 2020)
test_data <- subset(data, Year >= 2021 & Year <= 2023)

#---------------------------
# Identify KPIs using MLR
#---------------------------
# Fit MLR model
mlr_model <- lm(Container ~ ., data = train_data)
mlr_summary <- summary(mlr_model)

# Extract significant variables (p-value < 0.05)
names(coef(mlr_model))[which(mlr_summary$coefficients[, "Pr(>|t|)"] < 0.05)]
significant_vars_mlr <- names(coef(mlr_model))[which(mlr_summary$coefficients[, "Pr(>|t|)"] < 0.05)]
significant_vars_mlr <- significant_vars_mlr[significant_vars_mlr != "(Intercept)"]

#----------------------------
# Identify KPIs using MARS
#----------------------------
# Fit MARS model with default parameters
mars_model <- earth(Container ~ ., data = train_data)
mars_evimp <- evimp(mars_model)

# Extract variables used in MARS model
rownames(mars_evimp)
significant_vars_mars <- rownames(mars_evimp)

#-------------------------------
# Union of significant variables
#-------------------------------
key_vars <- union(significant_vars_mlr, significant_vars_mars)

#--------------------------------
# Refit models using key_vars
#--------------------------------
# Update formulas
formula <- as.formula(paste("Container ~", paste(key_vars, collapse = " + ")))

# Refit MLR model
mlr_container <- lm(formula, data = train_data)
summary(mlr_container)

# Refit MARS model
# Tuning the degree for optimal MARS model
min_mape <- Inf
best_degree <- 1

for(i in 1:4) {
  mars_model <- earth(formula, degree = i, data = train_data)
  mars_test_preds <- predict(mars_model, newdata = test_data)
  mape <- mean(abs((test_data$Container - mars_test_preds) / test_data$Container))
  
  if (mape < min_mape) {
    min_mape <- mape
    best_degree <- i
  }
}
print(paste("Best degree for MARS:", best_degree))

# Final MARS model with optimal degree
mars_container <- earth(formula, degree = best_degree, data = train_data)
summary(mars_container)

#-------------------------------
# Predictions and Performance
#-------------------------------
# Predictions on testing data
mlr_test_preds <- predict(mlr_container, newdata = test_data)
mars_test_preds <- predict(mars_container, newdata = test_data)

# Calculate performance metrics for MLR
mlr_rmse <- sqrt(mean((test_data$Container - mlr_test_preds)^2))
mlr_mae <- mean(abs(test_data$Container - mlr_test_preds))
mlr_mape <- mean(abs((test_data$Container - mlr_test_preds) / test_data$Container))

# Calculate performance metrics for MARS
mars_rmse <- sqrt(mean((test_data$Container - mars_test_preds)^2))
mars_mae <- mean(abs(test_data$Container - mars_test_preds))
mars_mape <- mean(abs((test_data$Container - mars_test_preds) / test_data$Container))

# Display metrics
performance_table <- data.frame(
  Method = c("MLR", "MARS"),
  RMSE = c(mlr_rmse, mars_rmse),
  MAE = c(mlr_mae, mars_mae),
  MAPE = c(mlr_mape, mars_mape)
)
print(performance_table)

#-------------------------------
# Managerial Insights
#-------------------------------
# Interpret the significant variables and discuss their impact on Container
# (This section would be written in your report or presentation)

#-------------------------------
# Plotting
#-------------------------------
# Combine actual and predicted values into one dataframe
# Combine actual and predicted values into one dataframe
plot_data <- data.frame(
  Date = test_data$Date,
  Actual = test_data$Container,
  MLR_Predicted = mlr_test_preds,
  MARS_Predicted = mars_test_preds
)

# Melt the data for plotting
plot_data_melted <- melt(plot_data, id.vars = "Date")

# Plot with legend adjustments
ggplot(plot_data_melted, aes(x = Date, y = value, color = variable, linetype = variable)) +
  geom_line(size = 1) +
  labs(title = "Actual vs Predicted Container Values",
       x = "Date", y = "Container") +
  scale_color_manual(
    values = c("blue", "red", "green"),
    labels = c("Actual", "MLR Predicted", "MARS Predicted")
  ) +
  scale_linetype_manual(
    values = c("solid", "dashed", "dotted"),
    labels = c("Actual", "MLR Predicted", "MARS Predicted")
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())


################### Question 5
library(dplyr)
library(e1071)  # For Naive Bayes
library(pROC)   # For AUC calculation
#install.packages("klaR")
library(klaR)

# Load Titanic dataset
titanic <- read.csv("titanic.csv")

# Handle missing values if necessary
titanic <- na.omit(titanic)

titanic$gender <- ifelse(titanic$gender == 0, "male", "female")

# Convert gender to factor
titanic$gender <- as.factor(titanic$gender)

# Recode survival variable to "died" and "survived"
titanic$survival <- ifelse(titanic$survival == 0, "died", "survived")


# Convert survival to factor
titanic$survival <- as.factor(titanic$survival)

# Convert 'class' to factor 
titanic$class <- as.factor(titanic$class)

# Set seed for reproducibility
set.seed(11355)

# Split the data into training and testing sets (70% training, 30% testing)
train_size <- floor(0.7 * nrow(titanic))
train_indices <- sample(seq_len(nrow(titanic)), size = train_size)
train_data <- titanic[train_indices, ]
test_data <- titanic[-train_indices, ]

# Build Na�ve Bayes model
nb_model <- NaiveBayes(survival ~ class + gender + age + fare, data = train_data)

# Build Logistic Regression model
logit_model <- glm(survival ~ class + gender + age + fare, data = train_data, family=binomial(link="logit"))

# Scenario 1: Impact of Gender (Male vs Female), given Age = 30, Class = 2, Fare = 20
scenario1 <- data.frame(
  class = factor(2, levels = levels(train_data$class)),
  age = 30,
  fare = 20,
  gender = factor(c("male", "female"), levels = levels(train_data$gender))
)

# Na�ve Bayes predictions for Scenario 1
nb_pred1 <- predict(nb_model, scenario1, type = "raw")$posterior[, "survived"]

# Logistic Regression predictions for Scenario 1
logit_pred1 <- predict(logit_model, scenario1, type = "response")

# Display predictions for Scenario 1
scenario1_results <- data.frame(
  Gender = scenario1$gender,
  Naive_Bayes = nb_pred1,
  Logistic_Regression = logit_pred1
)
print("Scenario 1: Impact of Gender (Male vs Female), given Age = 30, Class = 2, Fare = 20")
print(scenario1_results)

# Scenario 2: Impact of Age (1 vs 25), given Gender = Male, Class = 1, Fare = 40
scenario2 <- data.frame(
  class = factor(1, levels = levels(train_data$class)),
  age = c(1, 25),
  fare = 40,
  gender = factor("male", levels = levels(train_data$gender))
)

# Na�ve Bayes predictions for Scenario 2
nb_pred2 <- predict(nb_model, scenario2, type = "raw")$posterior[, "survived"]

# Logistic Regression predictions for Scenario 2
logit_pred2 <- predict(logit_model, scenario2, type = "response")

# Display predictions for Scenario 2
scenario2_results <- data.frame(
  Age = scenario2$age,
  Naive_Bayes = nb_pred2,
  Logistic_Regression = logit_pred2
)
print("Scenario 2: Impact of Age (1 vs 25), given Gender = Male, Class = 1, Fare = 40")
print(scenario2_results)

# Scenario 3: Impact of Class (1, 2, 3), given Gender = Female, Age = 25, Fare = 40
scenario3 <- data.frame(
  class = factor(c(1, 2, 3), levels = levels(train_data$class)),
  age = 25,
  fare = 40,
  gender = factor("female", levels = levels(train_data$gender))
)

# Na�ve Bayes predictions for Scenario 3
nb_pred3 <- predict(nb_model, scenario3, type = "raw")$posterior[, "survived"]

# Logistic Regression predictions for Scenario 3
logit_pred3 <- predict(logit_model, scenario3, type = "response")

# Display predictions for Scenario 3
scenario3_results <- data.frame(
  Class = scenario3$class,
  Naive_Bayes = nb_pred3,
  Logistic_Regression = logit_pred3
)
print("Scenario 3: Impact of Class (1, 2, 3), given Gender = Female, Age = 25, Fare = 40")
print(scenario3_results)


################### Question 6
library(glmnet)

# Remove samples with missing values
handysize <- read.csv("handysize.csv")
handysize <- na.omit(handysize)

# Extract the date variable
# Assume the date column is named 'Date' in your dataset
date <- handysize$Date

# Convert the date variable to Date format if it's not already
# Adjust the format string according to your date format
date <- as.Date(date, format="%Y/%m/%d")  # Example format

# Prepare the response and predictor matrix
y <- handysize$Handysize

# Exclude 'Handysize' and 'Date' from predictors
predictor_names <- names(handysize)[!(names(handysize) %in% c("Handysize"))]
x <- data.matrix(handysize[ , predictor_names])


# Set seed for reproducibility
set.seed(11355)

# Initialize a table to store performance metrics
performance_table <- matrix(NA, nrow=3, ncol=3)
rownames(performance_table) <- c("RMSE", "MAE", "MAPE")
colnames(performance_table) <- c("Ridge", "Lasso", "ElasticNet")

############# Ridge Regression

# Fit the final Ridge model
ridge_model <- glmnet(x, y,family= "gaussian", alpha=0, lambda=1)
ridge_beta <- as.vector(ridge_model$beta)
names(ridge_beta) <- predictor_names

# Prediction and performance metrics
pred_ridge <- predict(ridge_model, x)
performance_table[1,1] <- sqrt(mean((y - pred_ridge)^2))  # RMSE
performance_table[2,1] <- mean(abs(y - pred_ridge))       # MAE
performance_table[3,1] <- mean(abs((y - pred_ridge) / y)) * 100  # MAPE in percentage

############# Lasso Regression

# Fit the final Lasso model with the best lambda
lasso_model <- glmnet(x, y,family="gaussian", alpha=1, lambda=1)
lasso_beta <- as.vector(lasso_model$beta)
names(lasso_beta) <- colnames(x)

# Prediction and performance metrics
pred_lasso <- predict(lasso_model, x)
performance_table[1,2] <- sqrt(mean((y - pred_lasso)^2))  # RMSE
performance_table[2,2] <- mean(abs(y - pred_lasso))       # MAE
performance_table[3,2] <- mean(abs((y - pred_lasso) / y)) * 100  # MAPE in percentage

############# ElasticNet Regression

# Fit the final Elastic Net model with the best lambda
elastic_model <- glmnet(x, y,family="gaussian", alpha=0.5, lambda=1)
elastic_beta <- as.vector(elastic_model$beta)
names(elastic_beta) <- colnames(x)

# Prediction and performance metrics
pred_elastic <- predict(elastic_model, x)
performance_table[1,3] <- sqrt(mean((y - pred_elastic)^2))  # RMSE
performance_table[2,3] <- mean(abs(y - pred_elastic))       # MAE
performance_table[3,3] <- mean(abs((y - pred_elastic) / y)) * 100  # MAPE in percentage

# Display the performance table
print(performance_table)

# Identify significant predictors
# For Ridge Regression, rank predictors based on absolute coefficient values
ridge_coefficients <- data.frame(
  Predictor = predictor_names,
  Coefficient = ridge_beta,
  AbsCoefficient = abs(ridge_beta)
)

# Sort predictors by absolute coefficient values in decreasing order
ridge_coefficients <- ridge_coefficients[order(-ridge_coefficients$AbsCoefficient), ]

# We select predictors with absolute coefficients above x percent
threshold <- quantile(ridge_coefficients$AbsCoefficient, 0.75)  # Top 25%
significant_predictors_ridge <- ridge_coefficients$Predictor[ridge_coefficients$AbsCoefficient >= threshold]

# Print significant predictors
print("Significant predictors in Ridge Regression (Top 25% by absolute coefficient):")
print(significant_predictors_ridge)

# Identify significant predictors
lasso_coefficients <- data.frame(
  Predictor = predictor_names,
  Coefficient = ridge_beta,
  AbsCoefficient = abs(ridge_beta)
)
lasso_coefficients <- lasso_coefficients[order(-lasso_coefficients$AbsCoefficient), ]
threshold_lasso <- quantile(lasso_coefficients$AbsCoefficient, 0.75)  # Top 25%
significant_predictors_lasso <- lasso_coefficients$Predictor[lasso_coefficients$AbsCoefficient >= threshold_lasso]



elastic_coefficients <- data.frame(
  Predictor = predictor_names,
  Coefficient = elastic_beta,
  AbsCoefficient = abs(elastic_beta)
)
elastic_coefficients <- elastic_coefficients[order(-elastic_coefficients$AbsCoefficient), ]
threshold_elastic <- quantile(elastic_coefficients$AbsCoefficient, 0.75)  # Top 25%
significant_predictors_elastic <- elastic_coefficients$Predictor[elastic_coefficients$AbsCoefficient >= threshold_elastic]

# Print significant predictors
print("Significant predictors in Lasso Regression:")
print(significant_predictors_lasso)

print("Significant predictors in Elastic Net Regression:")
print(significant_predictors_elastic)

# Plotting actual vs predicted values over time (Date)
plot(date, y, type='l', col='black', lwd=2, ylab='Value', xlab='Date', main='Actual vs Predicted Values')
lines(date, pred_ridge, col='blue', lwd=2)
lines(date, pred_lasso, col='red', lwd=2)
lines(date, pred_elastic, col='green', lwd=2)
legend("topright", legend=c("Actual", "Ridge", "Lasso", "ElasticNet"), col=c("black", "blue", "red", "green"), lwd=2)