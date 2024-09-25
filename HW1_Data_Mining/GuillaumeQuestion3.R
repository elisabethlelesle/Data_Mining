## Question 3


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

# Plot the SMA results
plot_df_SMA <- data.frame(Date = 1:length(data_Japan), Japan = data_Japan, SMA_10 = sma_10, SMA_20 = sma_20)

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
fitted_025 <- c(exp_smooth_025[1,1], exp_smooth_025$fitted[,1])
fitted_045 <- c(exp_smooth_045[1,1], exp_smooth_045$fitted[,1])

#plotting everything
plot_df_ES <- data.frame(Date = 1:length(data_India), India = data_India,ES25 = fitted_025, ES45 = fitted_045)

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

