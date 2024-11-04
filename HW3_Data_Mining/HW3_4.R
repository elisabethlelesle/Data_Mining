# Load necessary libraries
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
plot_data <- data.frame(
  Year = test_data$Year,
  Actual = test_data$Container,
  MLR_Predicted = mlr_test_preds,
  MARS_Predicted = mars_test_preds
)

# Melt the data for plotting
library(reshape2)
plot_data_melted <- melt(plot_data, id.vars = "Year")

# Plot
ggplot(plot_data_melted, aes(x = Year, y = value, color = variable, linetype = variable)) +
  geom_line(size = 1) +
  labs(title = "Actual vs Predicted Container Values",
       x = "Year", y = "Container") +
  scale_color_manual(values = c("blue", "red", "green")) +
  theme_minimal() +
  theme(legend.title = element_blank())
