## Question 5

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(MASS)
library(readxl)
library(forecast)
library(arules)

data <- read.csv("Pima.csv")
removedZeroes <- filter(data, data$Glucose != 0, data$BloodPressure != 0,
                        data$SkinThickness != 0, data$Insulin != 0, data$BMI != 0)

# Convert 'Outcome' to factor with labels 'Yes' and 'No'
removedZeroes$Outcome <- factor(removedZeroes$Outcome, levels = c(0,1), labels = c("No", "Yes"))


filtered_data <- removedZeroes %>%
  mutate(
    Glucose_level = ifelse(Glucose > median(Glucose), "High", "Low"),
    BloodPressure_level = ifelse(BloodPressure > median(BloodPressure), "High", "Low"),
    SkinThickness_level = ifelse(SkinThickness > median(SkinThickness), "High", "Low"),
    Insulin_level = ifelse(Insulin > median(Insulin), "High", "Low"),
    BMI_level = ifelse(BMI > median(BMI), "High", "Low"),
  )

filtered_data$Glucose_level <- as.factor(filtered_data$Glucose_level)
filtered_data$BloodPressure_level <- as.factor(filtered_data$BloodPressure_level)
filtered_data$SkinThickness_level <- as.factor(filtered_data$SkinThickness_level)
filtered_data$Insulin_level <- as.factor(filtered_data$Insulin_level)
filtered_data$BMI_level <- as.factor(filtered_data$BMI_level)

# Select the relevant columns for association rule mining
trans_data <- filtered_data %>%
  dplyr::select(Glucose_level, BloodPressure_level, SkinThickness_level, Insulin_level, BMI_level, Outcome)

# Convert the data frame to transactions format
transactions <- as(trans_data, "transactions")

# Generate association rules with RHS = 'Outcome=Yes'
rules_yes <- apriori(transactions,
                     parameter = list(supp = 0.1, conf = 0.5, minlen = 2),
                     appearance = list(rhs = c("Outcome=Yes"), default = "lhs"))

# Generate association rules with RHS = 'Outcome=No'
rules_no <- apriori(transactions,
                    parameter = list(supp = 0.1, conf = 0.5, minlen = 2),
                    appearance = list(rhs = c("Outcome=No"), default = "lhs"))



# Get LHS item frequencies for 'Outcome=Yes'
inspect(head(rules_yes))
inspect(head(rules_no))


