# Load necessary libraries
library(dplyr)
library(e1071)  # For Naive Bayes
library(pROC)   # For AUC calculation

# Load Titanic dataset
titanic <- read.csv("titanic.csv")

# Handle missing values if necessary
titanic <- na.omit(titanic)

# Recode gender variable to "male" and "female" if necessary
# Check unique values in gender
unique_genders <- unique(titanic$gender)
if (all(unique_genders %in% c(0, 1))) {
  titanic$gender <- ifelse(titanic$gender == 0, "male", "female")
}

# Convert gender to factor
titanic$gender <- as.factor(titanic$gender)

# Recode survival variable to "died" and "survived" if necessary
unique_survival <- unique(titanic$survival)
if (all(unique_survival %in% c(0, 1))) {
  titanic$survival <- ifelse(titanic$survival == 0, "died", "survived")
}

# Convert survival to factor
titanic$survival <- as.factor(titanic$survival)

# Convert 'class' to factor if not already
titanic$class <- as.factor(titanic$class)

# Set seed for reproducibility
set.seed(42)

# Split the data into training and testing sets (70% training, 30% testing)
train_size <- floor(0.7 * nrow(titanic))
train_indices <- sample(seq_len(nrow(titanic)), size = train_size)
train_data <- titanic[train_indices, ]
test_data <- titanic[-train_indices, ]

# Build Naïve Bayes model
nb_model <- naiveBayes(survival ~ class + gender + age + fare, data = train_data)

# Build Logistic Regression model
logit_model <- glm(survival ~ class + gender + age + fare, data = train_data, family = "binomial")

# Scenario 1: Impact of Gender (Male vs Female), given Age = 30, Class = 2, Fare = 20
scenario1 <- data.frame(
  class = factor(2, levels = levels(train_data$class)),
  age = 30,
  fare = 20,
  gender = factor(c("male", "female"), levels = levels(train_data$gender))
)

# Naïve Bayes predictions for Scenario 1
nb_pred1 <- predict(nb_model, scenario1, type = "raw")[, "survived"]

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

# Naïve Bayes predictions for Scenario 2
nb_pred2 <- predict(nb_model, scenario2, type = "raw")[, "survived"]

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

# Naïve Bayes predictions for Scenario 3
nb_pred3 <- predict(nb_model, scenario3, type = "raw")[, "survived"]

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
