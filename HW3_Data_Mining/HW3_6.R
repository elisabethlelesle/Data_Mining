# Load necessary libraries
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
X <- data.matrix(handysize[ , !(names(handysize) %in% c("Handysize", "Date"))])

# Set seed for reproducibility
set.seed(123)

# Initialize a table to store performance metrics
performance_table <- matrix(NA, nrow=3, ncol=3)
rownames(performance_table) <- c("RMSE", "MAE", "MAPE")
colnames(performance_table) <- c("Ridge", "Lasso", "ElasticNet")

############# Ridge Regression
# Use cross-validation to find the best lambda
ridge_cv <- cv.glmnet(X, y, alpha=0, nfolds=10)
best_lambda_ridge <- ridge_cv$lambda.min

# Fit the final Ridge model with the best lambda
ridge_model <- glmnet(X, y, alpha=0, lambda=best_lambda_ridge)
ridge_beta <- as.vector(ridge_model$beta)
names(ridge_beta) <- colnames(X)

# Prediction and performance metrics
pred_ridge <- predict(ridge_model, X)
performance_table[1,1] <- sqrt(mean((y - pred_ridge)^2))  # RMSE
performance_table[2,1] <- mean(abs(y - pred_ridge))       # MAE
performance_table[3,1] <- mean(abs((y - pred_ridge) / y)) * 100  # MAPE in percentage

############# Lasso Regression
# Use cross-validation to find the best lambda
lasso_cv <- cv.glmnet(X, y, alpha=1, nfolds=10)
best_lambda_lasso <- lasso_cv$lambda.min

# Fit the final Lasso model with the best lambda
lasso_model <- glmnet(X, y, alpha=1, lambda=best_lambda_lasso)
lasso_beta <- as.vector(lasso_model$beta)
names(lasso_beta) <- colnames(X)

# Prediction and performance metrics
pred_lasso <- predict(lasso_model, X)
performance_table[1,2] <- sqrt(mean((y - pred_lasso)^2))  # RMSE
performance_table[2,2] <- mean(abs(y - pred_lasso))       # MAE
performance_table[3,2] <- mean(abs((y - pred_lasso) / y)) * 100  # MAPE in percentage

############# ElasticNet Regression
# Use cross-validation to find the best lambda
elastic_cv <- cv.glmnet(X, y, alpha=0.5, nfolds=10)
best_lambda_elastic <- elastic_cv$lambda.min

# Fit the final Elastic Net model with the best lambda
elastic_model <- glmnet(X, y, alpha=0.5, lambda=best_lambda_elastic)
elastic_beta <- as.vector(elastic_model$beta)
names(elastic_beta) <- colnames(X)

# Prediction and performance metrics
pred_elastic <- predict(elastic_model, X)
performance_table[1,3] <- sqrt(mean((y - pred_elastic)^2))  # RMSE
performance_table[2,3] <- mean(abs(y - pred_elastic))       # MAE
performance_table[3,3] <- mean(abs((y - pred_elastic) / y)) * 100  # MAPE in percentage

# Display the performance table
print(performance_table)

# Identify significant predictors
significant_predictors_lasso <- names(lasso_beta)[lasso_beta != 0]
significant_predictors_elastic <- names(elastic_beta)[elastic_beta != 0]

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
