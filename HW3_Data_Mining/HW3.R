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