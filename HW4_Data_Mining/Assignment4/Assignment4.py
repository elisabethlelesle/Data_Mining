import pandas as pd
import numpy as np
from scipy import stats
import cvxpy as cp
from itertools import combinations

# Assuming the ETF data is in an Excel file named 'ETF_data_1128.xlsx'
# Replace 'Sheet1' with the actual sheet name if it is different
df = pd.read_excel('ETF_data_1128.xlsx', sheet_name='World_ETF', parse_dates=['Date'], index_col='Date')

# List of ETFs to consider
etf_list = ['ETF00662', 'ETF00668', 'ETF00646', 'ETF00645', 'ETF00652']

# Calculate daily returns
daily_returns = df[etf_list].pct_change().dropna()

# Calculate daily variances
daily_variances = daily_returns.var()

# Convert daily returns and variances to annual returns and variances
trading_days = 252  # Number of trading days in a year
annual_returns = daily_returns.mean() * trading_days
annual_variances = daily_variances * trading_days

# T-test for equality of annual returns
print("T-test results for annual returns:")
for etf1, etf2 in combinations(etf_list, 2):
    returns1 = daily_returns[etf1]
    returns2 = daily_returns[etf2]
    t_stat, p_value = stats.ttest_ind(returns1, returns2, equal_var=False)
    print(f"{etf1} vs {etf2}: t-statistic = {t_stat:.4f}, p-value = {p_value:.4f}")

# F-test for equality of variances
print("\nF-test results for variances:")
for etf1, etf2 in combinations(etf_list, 2):
    var1 = daily_returns[etf1].var()
    var2 = daily_returns[etf2].var()
    f_stat = var1 / var2 if var1 > var2 else var2 / var1
    df1 = len(daily_returns[etf1]) - 1
    df2 = len(daily_returns[etf2]) - 1
    p_value = stats.f.cdf(f_stat, df1, df2)
    p_value = 2 * min(p_value, 1 - p_value)  # Two-tailed test
    print(f"{etf1} vs {etf2}: F-statistic = {f_stat:.4f}, p-value = {p_value:.4f}")

# Calculate annual covariance matrix
annual_covariance = daily_returns.cov() * trading_days

# Optimization Problem 1: Maximize return with annual risk <= 0.25
n = len(etf_list)
mu = annual_returns.values  # Expected returns
cov_matrix = annual_covariance.values  # Covariance matrix

# Define optimization variables
w1 = cp.Variable(n)

# Define the objective and constraints
portfolio_return1 = mu @ w1
portfolio_variance1 = cp.quad_form(w1, cov_matrix)
constraints1 = [
    cp.sum(w1) == 1,
    w1 >= 0,
    cp.sqrt(portfolio_variance1) <= 0.25
]

# Solve the optimization problem (Maximize return with annual risk <= 0.25)
prob1 = cp.Problem(cp.Maximize(portfolio_return1), constraints1)
prob1.solve(qcp=True, solver=cp.SCS)  # Specify a compatible solver like SCS

# Display results for Problem 1
print("\nOptimization Problem 1:")
print("Status:", prob1.status)
print("Optimal annual return:", portfolio_return1.value)
print("Optimal annual risk (std dev):", np.sqrt(portfolio_variance1.value))
print("Optimal weights:")
for i, etf in enumerate(etf_list):
    print(f"{etf}: {w1.value[i]:.4f}")

# Optimization Problem 2: Minimize risk with annual return >= 0.12
w2 = cp.Variable(n)
portfolio_return2 = mu @ w2
portfolio_variance2 = cp.quad_form(w2, cov_matrix)
constraints2 = [
    cp.sum(w2) == 1,
    w2 >= 0,
    portfolio_return2 >= 0.12
]

# Solve the optimization problem (Minimize risk with annual return >= 0.12)
prob2 = cp.Problem(cp.Minimize(portfolio_variance2), constraints2)
prob2.solve(qcp=True, solver=cp.SCS)  # Specify the same solver

# Display results for Problem 2
print("\nOptimization Problem 2:")
print("Status:", prob2.status)
print("Optimal annual return:", portfolio_return2.value)
print("Optimal annual risk (std dev):", np.sqrt(portfolio_variance2.value))
print("Optimal weights:")
for i, etf in enumerate(etf_list):
    print(f"{etf}: {w2.value[i]:.4f}")
