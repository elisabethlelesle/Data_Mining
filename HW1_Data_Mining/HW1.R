# Question 1 ----


# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(MASS)
library(stats)

# Load the dataset
data <- read.csv("gender_outlier.csv")
plot(data)

# Check structure of the data
#str(data$Height)

# Display histograms w.r.t. height, weight, and waist
par(mfrow=c(1,3))  # Set up the plotting area
hist(data$Height, main="Height Histogram", xlab="Height (cm)", col="lightblue", border="black")
hist(data$Weight, main="Weight Histogram", xlab="Weight (kg)", col="lightgreen", border="black")
hist(data$Waist, main="Waist Histogram", xlab="Waist (cm)", col="lightcoral", border="black")

# Box plots to identify outliers
par(mfrow=c(1,3))  # Set up the plotting area
boxplot(data$Height, main="Height Box Plot", horizontal=FALSE, col="lightblue")
boxplot(data$Weight, main="Weight Box Plot", horizontal=FALSE, col="lightgreen")
boxplot(data$Waist, main="Waist Box Plot", horizontal=FALSE, col="lightcoral")

# Multi-dimensional scaling
# Prepare the data for MDS
#print(colnames(data))
mds_data <- data[c("Height", "Weight", "Waist")]
# Calculate the distance matrix using Euclidean distance
dist_matrix <- dist(mds_data, method = 'euclidean')
# Perform MDS
mds_result <- cmdscale(dist_matrix, k = 2, eig = TRUE)
# Extract MDS coordinates
mdsx <- mds_result$points[, 1]
mdsy <- mds_result$points[, 2]
# Create a data frame for ggplot
mds_df <- data.frame(MDS1 = mdsx, MDS2 = mdsy, Gender = factor(data$Gender))
# ggplot for MDS
p <- ggplot(mds_df, aes(x = MDS1, y = MDS2)) +
  geom_point(size = 3, aes(colour = Gender, shape = Gender)) +
  labs(title = "MDS for Gender Data", x = "Correlation of Height and Weight", y = "Waist Measurement Influence") +
  theme_minimal()
# Display the ggplot
print(p)


# Question 2 ----


#install.packages("isotree")
library(isotree)

# Load the dataset
pima <- read.csv("Pima.csv", stringsAsFactors = TRUE)
plot(pima) # Visualization

### Outlier test based on Mahalanobis distance
mean_vector <- colMeans(pima[, -ncol(pima)])  # Exclude the last column if it's a factor (Outcome)
mean_vector

cov_matrix <- cov(pima[, -ncol(pima)], use = "pairwise")
cov_matrix

pima$mdis <- mahalanobis(pima[, -ncol(pima)], mean_vector, cov_matrix)

# Chi-square test for 10% threshold
threshold <- qchisq(df = ncol(pima) - 1, p = 0.9)  # df: degrees of freedom
pima$maout <- (pima$mdis > threshold)

# Identify indices of Mahalanobis outliers
mahalanobis_outliers <- which(pima$maout == TRUE)
cat("Mahalanobis Outliers Indices:", mahalanobis_outliers, "\n")


### Outlier test based on Isolation Forest
iso_model <- isolation.forest(pima[, -ncol(pima)], ntrees = 100, nthreads = -1)
predictions <- predict(iso_model, pima[, -ncol(pima)])

# Identify data with highest outlier score
pima[which.max(predictions),]  # Data with the highest outlier score
isolation_forest_outliers <- which(predictions > 0.6)

cat("Isolation Forest Outliers Indices:", isolation_forest_outliers, "\n")

### Comparing Results

# Common outliers
common_outliers <- intersect(mahalanobis_outliers, isolation_forest_outliers)
cat("Common Outliers Indices:", common_outliers, "\n")

# Differences
unique_mahalanobis <- setdiff(mahalanobis_outliers, isolation_forest_outliers)
unique_isolation_forest <- setdiff(isolation_forest_outliers, mahalanobis_outliers)

cat("Unique Mahalanobis Outliers Indices:", unique_mahalanobis, "\n")
cat("Unique Isolation Forest Outliers Indices:", unique_isolation_forest, "\n")

# Section Question 3 ----

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(MASS)
library(readxl)
library(forecast)

#read the data
data <- read_excel("Asia_Stock.xlsx")

data_Japan <- data$JAPAN
data_India <- data$INDIA

#calculate the moving average
#slide 1 ensures that we use the current and previous values and not future values
sma_10 <- stats::filter(data_Japan, rep(1/10, 10), sides=1)
sma_20 <- stats::filter(data_Japan, rep(1/20, 20), sides=1)


dates <- data$Date

# Plot the SMA results
plot_df_SMA <- data.frame(Date = dates, Japan = data_Japan, SMA_10 = sma_10, SMA_20 = sma_20)

ggplot(plot_df_SMA, aes(x = Date)) +
  geom_line(aes(y = SMA_10, color = "SMA 10"), size = 1) +
  geom_line(aes(y = SMA_20, color = "SMA 20"), size = 1) +
  labs(title = "Simple Moving Average (SMA) Forecast for Japan", x = "Date", y = "Stock Price") +
  theme_minimal() +
  scale_color_manual(values = c("SMA 10" = "green", "SMA 20" = "red"))

calc_performance <- function(actual, forecast){
  
  valid_indices <- !is.na(forecast)
  actual <- actual[valid_indices]
  forecast <- forecast[valid_indices]
  
  rmse <- sqrt(mean((actual - forecast)^2))
  mae <- mean(abs(actual - forecast))
  mape <- mean(abs((actual - forecast) / actual)) * 100
  
  return(list(rmse,mae,mape))
}

performace_sma_10 <- calc_performance(data_Japan, sma_10)
print(performace_sma_10)

performance_sma_20 <- calc_performance(data_Japan,sma_20)
print(performance_sma_20)


##Part 2 

lambda1 <- 0.25
lambda2 <- 0.45

#Holtwinters with beat and gamma set to FALSE helps us do exponential smoothing 
exp_smooth_025 <- HoltWinters(data_India, alpha = lambda1, beta = FALSE, gamma = FALSE)
exp_smooth_045 <- HoltWinters(data_India, alpha = lambda2, beta = FALSE, gamma = FALSE)

#add one value at the beginning since expontial smoothing turn a array of size n in
# an array of size n-1 because we want to compare it with the original data that has
# 1 more value
fitted_025 <- exp_smooth_025$fitted[, 1]  
fitted_045 <- exp_smooth_045$fitted[, 1]

fitted_025 <- c(rep(NA, length(data_India) - length(fitted_025)), fitted_025)
fitted_045 <- c(rep(NA, length(data_India) - length(fitted_045)), fitted_045)

#plotting everything
plot_df_ES <- data.frame(Date = dates, India = data_India,ES25 = fitted_025, ES45 = fitted_045)

ggplot(plot_df_ES, aes(x = Date)) +
  geom_line(aes(y = ES25, color = "ES 25"), size = 1) +
  geom_line(aes(y = ES45, color = "ES 45"), size = 1) +
  labs(title = "Exponential smoothing for India", x = "Date", y = "Stock Price") +
  theme_minimal() +
  scale_color_manual(values = c("ES 25" = "blue", "ES 45" = "orange"))

#printing the performance
performace_SE_25 <- calc_performance(data_India, fitted_025)
print(performace_SE_25)

performance_SE_45 <- calc_performance(data_India,fitted_045)
print(performance_SE_45)


# Question 4 ----

library(arules)
library(arulesViz)

# Load the dataset
bank_data <- read.csv("bank.csv", stringsAsFactors = TRUE)

### Preparing the data 

# Rename the single column to a temporary name
colnames(bank_data) <- "data"

# Split the single column into multiple columns
bank_data <- bank_data %>%
  separate(data, into = c("age", "job", "marital", "education", "default", 
                          "balance", "housing", "loan", "contact", 
                          "day", "month", "duration", "campaign", 
                          "pdays", "previous", "poutcome", "y"), 
           sep = ";")

# Remove specified columns
bank_data <- bank_data %>%
  dplyr::select(-job, -contact, -pdays, -poutcome, -month, -day)

### Discretize the age column
bank_data$age <- as.numeric(as.character(bank_data$age))  # Convert age to numeric
bank_data$age <- case_when(
  bank_data$age < 35 ~ "young",
  bank_data$age >= 35 & bank_data$age <= 55 ~ "adult",
  bank_data$age > 55 ~ "old"
)

### Convert remaining numeric columns to factors based on median

# Identify numeric columns
check_numeric_cols <-sapply(bank_data, function(x) {
  # Try converting and check if there are any NAs
  suppressWarnings(all(!is.na(as.numeric(as.character(x)))))
})
numeric_cols <- names(bank_data)[check_numeric_cols]
#Convert numeric columns to factors based on median using dplyr
bank_data[numeric_cols] <- lapply(bank_data[numeric_cols], function(x) {
  factor(ifelse(x < median(x, na.rm = TRUE), "low", "high"))
})

### Analyze remaining categorical columns with the frequency distribution

# Create a new dataframe with only categorical columns
categorical_cols <- setdiff(names(bank_data), numeric_cols)

# Iterate over all categorical columns and create frequency tables
for (col_name in categorical_cols) {
  category_table <- table(bank_data[[col_name]])
  # Print the category table for each categorical column
  cat("\nFrequency table for", col_name, ":\n")
  print(category_table)
}

# For the marital column
bank_data$marital <- ifelse(median(c(0, 0, 1)) < ifelse(bank_data$marital == "married", 1, 0), "high", "low")

# For the education column
# Assign numeric values for education categories (excluding "unknown")
bank_data$education_numeric <- ifelse(bank_data$education == "primary", 0,
                                      ifelse(bank_data$education == "secondary", 1,
                                             ifelse(bank_data$education == "tertiary", 2, NA)))
# Compute the median of the numeric values (excluding NAs)
education_median <- median(bank_data$education_numeric, na.rm = TRUE)
# Classify as "high" or "low" based on the median
bank_data$education <- ifelse(bank_data$education_numeric > education_median, "high", "low")


# For the default column (same steps as education column)
bank_data$default_numeric <- ifelse(bank_data$default == "no", 0,
                                    ifelse(bank_data$default == "yes", 1, NA))

default_median <- median(bank_data$default_numeric, na.rm = TRUE)

bank_data$default <- ifelse(bank_data$default_numeric > default_median, "high", "low")

# For the housing (same steps as education column)
bank_data$housing_numeric <- ifelse(bank_data$housing == "no", 0,
                                    ifelse(bank_data$housing == "yes", 1, NA))

housing_median <- median(bank_data$housing_numeric, na.rm = TRUE)

bank_data$housing <- ifelse(bank_data$housing_numeric > housing_median, "high", "low")


#For the loan (same steps as education column)
bank_data$loan_numeric <- ifelse(bank_data$loan == "no", 0,
                                 ifelse(bank_data$loan == "yes", 1, NA))

loan_median <- median(bank_data$loan_numeric, na.rm = TRUE)

bank_data$loan <- ifelse(bank_data$loan_numeric > loan_median, "high", "low")


### Prepare the "high"/"low" formatted data: Convert all columns to factors
bank_data[] <- lapply(bank_data, as.factor)

### Generate rules with y as the RHS

# 
# # Adjust parameters to reduce the number of rules
# rule0 <- apriori(as.data.frame(bank_data), 
#                  parameter = list(minlen = 2, supp = 0.5, conf = 0.8, maxlen = 5),
#                  appearance = list(rhs = c("y=yes", "y=no"), default = "lhs"))
# 
# # Sort by lift
# sorted0 <- sort(rule0, by = "lift")
# cat("Rules with y as RHS (sorted by lift):\n")
# inspect(head(sorted0))
# 
# # Rule 1: Rules with y=yes as RHS
# rule1 <- apriori(as.data.frame(bank_data), 
#                  parameter = list(minlen = 2, supp = 0.1, conf = 0.1),
#                  appearance = list(rhs = "y=yes", default = "lhs"))
# 
# # Sort by confidence
# sorted1 <- sort(rule1, by = "count")
# cat("Rules with y=yes (sorted by count):\n")
# inspect(head(sorted1))
# 
# # Rule 2: Rules with y=no as RHS
# rule2 <- apriori(as.data.frame(bank_data), 
#                  parameter = list(minlen = 2, supp = 0.5, conf = 0.8, maxlen = 5),
#                  appearance = list(rhs = "y=no", default = "lhs"))
# 
# # Identify significant rules
# rules_sorted <- sort(rules, by = "lift")
# inspect(head(rules_sorted, 20))
# 
# # Count feature frequencies in LHS
# lhs_items <- labels(lhs(rules))
# lhs_features <- unlist(strsplit(lhs_items, ","))
# 
# feature_counts <- sort(table(lhs_features), decreasing = TRUE)
# print(feature_counts)
# 

# Generate association rules with adjusted parameters
rules <- apriori(bank_data, 
                 parameter = list(supp = 0.01, conf = 0.25, maxlen = 5),
                 appearance = list(rhs = c("y=yes", "y=no"), default = "lhs"))

# Identify significant rules
rules_sorted <- sort(rules, by = "lift")
inspect(head(rules_sorted, 20))

<<<<<<< HEAD
# Count feature frequencies in LHS
lhs_items <- labels(lhs(rules))
lhs_features <- unlist(strsplit(lhs_items, ","))
=======
# Rule 1: Rules with y=yes as RHS
rule1 <- apriori(as.data.frame(bank_data), 
                 parameter = list(minlen = 2, supp = 0.1, conf = 0.1),
                 appearance = list(rhs = "y=yes", default = "lhs"))

# Sort by count
sorted1 <- sort(rule1, by = "count")
cat("Rules with y=yes (sorted by count):\n")
inspect(head(sorted1))

# Rule 2: Rules with y=no as RHS
rule2 <- apriori(as.data.frame(bank_data), 
                 parameter = list(minlen = 2, supp = 0.5, conf = 0.8, maxlen = 5),
                 appearance = list(rhs = "y=no", default = "lhs"))

# Sort by count
sorted2 <- sort(rule2, by = "count")
cat("Rules with y=no (sorted by count):\n")
inspect(head(sorted2))
>>>>>>> e1ab97ffab0969e094f8a3bd3508a51ad0d85c18

feature_counts <- sort(table(lhs_features), decreasing = TRUE)
print(feature_counts)

# Question 5 ----

# Load necessary libraries
library(dplyr)
library(arules)

# Read the data
data <- read.csv("Pima.csv")

# Filter out zeroes
removedZeroes <- filter(data, Glucose != 0, BloodPressure != 0,
                        SkinThickness != 0, Insulin != 0, BMI != 0)

# Convert 'Outcome' to factor with labels 'Yes' and 'No'
removedZeroes$Outcome <- factor(removedZeroes$Outcome, levels = c(0, 1), labels = c("No", "Yes"))

# Create new factors based on median values and convert them to factors
filtered_data <- removedZeroes %>%
  mutate(
    Glucose_level = ifelse(Glucose > median(Glucose), "High", "Low"),
    BloodPressure_level = ifelse(BloodPressure > median(BloodPressure), "High", "Low"),
    SkinThickness_level = ifelse(SkinThickness > median(SkinThickness), "High", "Low"),
    Insulin_level = ifelse(Insulin > median(Insulin), "High", "Low"),
    BMI_level = ifelse(BMI > median(BMI), "High", "Low")
  ) %>%
  mutate_at(vars(Glucose_level, BloodPressure_level, SkinThickness_level, Insulin_level, BMI_level), as.factor)

# Select the relevant columns
trans_data <- filtered_data %>%
  dplyr::select(Glucose_level, BloodPressure_level, SkinThickness_level, Insulin_level, BMI_level, Outcome)

# Convert the data frame to transactions format
transactions <- as(trans_data, "transactions")

# Generate association rules with RHS = 'Outcome=Yes'
rules_yes <- apriori(transactions,
                     parameter = list(supp = 0.1, conf = 0.3, minlen = 2),
                     appearance = list(rhs = c("Outcome=Yes"), default = "lhs"))

# Generate association rules with RHS = 'Outcome=No'
rules_no <- apriori(transactions,
                    parameter = list(supp = 0.1, conf = 0.3, minlen = 2),
                    appearance = list(rhs = c("Outcome=No"), default = "lhs"))

### Function to sort rules based on hierarchical presence of features ###
sort_rules_by_feature_hierarchy <- function(rules) {
  # Extract LHS items as a list
  lhs_items_list <- LIST(lhs(rules))
  
  # Flatten the list to get all LHS items
  lhs_features <- unlist(lhs_items_list)
  
  # Count the frequency of each feature in the LHS
  feature_counts <- table(lhs_features)
  
  # Get features sorted by frequency in descending order
  sorted_features <- names(sort(feature_counts, decreasing = TRUE))
  
  # For each rule, create a presence vector indicating the presence (1) or absence (0) of each feature
  presence_matrix <- sapply(lhs_items_list, function(items) {
    as.integer(sorted_features %in% items)
  })
  
  # Transpose the matrix so that each row corresponds to a rule
  presence_matrix <- t(presence_matrix)
  
  # Convert the presence matrix to a data frame
  presence_df <- as.data.frame(presence_matrix)
  
  # Use do.call(order, ...) to get the order of the rules based on the presence vectors
  # We use '-' to sort in decreasing order (presence first)
  order_args <- lapply(presence_df, function(x) -x)
  sorted_indices <- do.call(order, order_args)
  
  # Sort the rules
  sorted_rules <- rules[sorted_indices]
  
  return(sorted_rules)
}

### Processing 'Outcome=Yes' Rules ###

# Sort rules for 'Outcome=Yes'
sorted_rules_yes <- sort_rules_by_feature_hierarchy(rules_yes)

# Extract the top 20 sorted rules
top20_sorted_rules_yes <- head(sorted_rules_yes, 20)

# Inspect the top 20 sorted rules for 'Outcome=Yes'
inspect(top20_sorted_rules_yes)

### Processing 'Outcome=No' Rules ###

# Sort rules for 'Outcome=No'
sorted_rules_no <- sort_rules_by_feature_hierarchy(rules_no)

# Extract the top 20 sorted rules
top20_sorted_rules_no <- head(sorted_rules_no, 20)

# Inspect the top 20 sorted rules for 'Outcome=No'
inspect(top20_sorted_rules_no)


# Question 6 ----

library(ggplot2)
library(dplyr)
library(tidyr)
library(MASS)
library(readxl)
library(forecast)
library(arules)


Asia_stock <- read_excel("Asia_Stock.xlsx")
model <- lm(TAIWAN ~ ., data = Asia_stock)


# Extract R-squared and Adjusted R-squared from the summary
r_squared <- summary(model)$r.squared
adj_r_squared <- summary(model)$adj.r.squared

# View p-values for significance of each feature
p_values <- summary(model)$coefficients[,4]

print(r_squared)
print(adj_r_squared)
print(head(sort(p_values)))


