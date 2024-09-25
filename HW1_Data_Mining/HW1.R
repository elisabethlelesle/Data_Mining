## Question 1


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
str(data$Height)


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
print(colnames(data))
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

# Base R plot for MDS
plot(mdsx, mdsy, pch = as.numeric(mds_df$Gender), 
     col = as.numeric(mds_df$Gender) + 1, 
     main = "MDS for Gender Data", 
     xlab = "MDS Dimension 1", 
     ylab = "MDS Dimension 2")

# ggplot for MDS
p <- ggplot(mds_df, aes(x = MDS1, y = MDS2)) +
  geom_point(size = 3, aes(colour = Gender, shape = Gender)) +
  labs(title = "MDS for Gender Data", x = "MDS Dimension 1", y = "MDS Dimension 2") +
  theme_minimal()

# Display the ggplot
<<<<<<< HEAD
print(p)


## Question 2
##install.packages("isotree")
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



## Question 4

library(arules)
library(arulesViz)

# Load the dataset
bank_data <- read.csv("bank.csv", stringsAsFactors = TRUE)

# Step 1: Split the single column into multiple columns
# Rename the single column to a temporary name
colnames(bank_data) <- "data"
bank_data <- bank_data %>%
  separate(data, into = c("age", "job", "marital", "education", "default", 
                          "balance", "housing", "loan", "contact", 
                          "day", "month", "duration", "campaign", 
                          "pdays", "previous", "poutcome", "y"), 
           sep = ";")

# Step 2: Remove specified columns
bank_data <- bank_data %>%
  select(-job, -contact, -pdays, -poutcome, -month, -day)

# Step 3: Discretize the age column
bank_data$age <- as.numeric(as.character(bank_data$age))  # Convert age to numeric
bank_data$age <- case_when(
  bank_data$age < 35 ~ "young",
  bank_data$age >= 35 & bank_data$age <= 55 ~ "adult",
  bank_data$age > 55 ~ "old"
)

# Step 4: Convert remaining numeric columns to factors based on median
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

# Step 5: Analyze remaining categorical columns with the frequency distribution
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


# For the default column
# Assign numeric values for the default column
bank_data$default_numeric <- ifelse(bank_data$default == "no", 0,
                                    ifelse(bank_data$default == "yes", 1, NA))

# Compute the median of the numeric values (excluding NAs)
default_median <- median(bank_data$default_numeric, na.rm = TRUE)

# Classify as "high" or "low" based on the median
bank_data$default <- ifelse(bank_data$default_numeric > default_median, "high", "low")

# For the housing
# Assign numeric values for the housing column
bank_data$housing_numeric <- ifelse(bank_data$housing == "no", 0,
                                    ifelse(bank_data$housing == "yes", 1, NA))

# Compute the median of the numeric values (excluding NAs)
housing_median <- median(bank_data$housing_numeric, na.rm = TRUE)

# Classify as "high" or "low" based on the median
bank_data$housing <- ifelse(bank_data$housing_numeric > housing_median, "high", "low")


#For the loan
# Assign numeric values for the loan column
bank_data$loan_numeric <- ifelse(bank_data$loan == "no", 0,
                                 ifelse(bank_data$loan == "yes", 1, NA))

# Compute the median of the numeric values (excluding NAs)
loan_median <- median(bank_data$loan_numeric, na.rm = TRUE)

# Classify as "high" or "low" based on the median
bank_data$loan <- ifelse(bank_data$loan_numeric > loan_median, "high", "low")


# Step 1: Prepare the data
# Convert all columns to factors
bank_data[] <- lapply(bank_data, as.factor)

# Step 2: Generate rules with y as the RHS
# Adjust parameters to reduce the number of rules
rule0 <- apriori(as.data.frame(bank_data), 
                 parameter = list(minlen = 2, supp = 0.5, conf = 0.8, maxlen = 5),
                 appearance = list(rhs = c("y=yes", "y=no"), default = "lhs"))

# Sort by lift
sorted0 <- sort(rule0, by = "lift")
cat("Rules with y as RHS (sorted by lift):\n")
inspect(head(sorted0))

# Rule 1: Rules with y=yes as RHS
rule1 <- apriori(as.data.frame(bank_data), 
                 parameter = list(minlen = 2, supp = 0.5, conf = 0.8, maxlen = 5),
                 appearance = list(rhs = "y=yes", default = "lhs"))

# Sort by confidence
sorted1 <- sort(rule1, by = "confidence")
cat("Rules with y=yes (sorted by confidence):\n")
inspect(head(sorted1))

# Rule 2: Rules with y=no as RHS
rule2 <- apriori(as.data.frame(bank_data), 
                 parameter = list(minlen = 2, supp = 0.5, conf = 0.8, maxlen = 5),
                 appearance = list(rhs = "y=no", default = "lhs"))

# Sort by confidence
sorted2 <- sort(rule2, by = "confidence")
cat("Rules with y=no (sorted by confidence):\n")
inspect(head(sorted2))

###################################

