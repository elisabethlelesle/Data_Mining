###Question 6

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
print(tail(sort(p_values)))


