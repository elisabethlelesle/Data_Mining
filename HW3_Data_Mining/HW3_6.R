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
