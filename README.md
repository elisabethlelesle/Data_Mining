# Data_Mining

## Introduction

Data mining refers to the process of analyzing large datasets by using machine learning, statistics, and database systems.
Key tasks in data mining include classification, clustering, regression, association rule mining, and anomaly detection.

This repository contains coursework from the Data Science/Data Mining curriculum at NYCU Taiwan. 
---

## Homework 1

**Topics Covered:**
1. **Outlier Detection & Visualization (Gender_Outlier Dataset):**
   - Generate histograms and box plots to identify outliers for height, weight, and waist measurements.
   - Perform multi-dimensional scaling (MDS) to visualize the data in 2D, distinguishing genders by color and symbols.

2. **Outlier Detection (Pima Dataset):**
   - Apply the Chi-square test with Mahalanobis distance to identify 10% of outliers.
   - Compare the results with an Isolation Forest method and evaluate similarities and differences.

3. **Time Series Forecasting (Asia_Stock Dataset):**
   - Forecast stock prices using simple moving averages (SMA) with window sizes (n=10, n=20) for Japan.
   - Use exponential smoothing (λ=0.25, λ=0.45) for India, calculating performance metrics such as RMSE, MAE, and MAPE.

4. **Association Rule Mining (Bank Dataset):**
   - Clean the dataset by removing irrelevant columns and discretizing age.
   - Apply association rule mining to extract significant patterns based on the "y" response variable (yes/no for subscribing to a product).

5. **Medical Insights via Association Rules (Pima Dataset):**
   - Remove zeros in selected columns and apply association rule mining to analyze relationships between features and diabetes outcomes.

6. **Linear Regression (Asia_Stock Dataset):**
   - Apply linear regression to forecast Taiwan stock prices and calculate performance metrics (R², adjusted R², and significance levels).

---

## Homework 2

**Topics Covered:**
1. **Clustering (WineQuality Dataset):**
   - Perform K-means and K-medoids clustering on the winequality-red dataset using the NbClust package to determine the optimal number of clusters.
   - Evaluate the average quality of wine within each cluster.

2. **Regression & Classification (WineQuality Dataset):**
   - Conduct multiple linear regression (MLR) and K-nearest neighbors (KNN) on the winequality-white dataset to forecast wine quality and compare their performance using RMSE, MAE, and MAPE.
   - Reclassify the wine dataset into two levels of quality (poor and good) and apply KNN for classification, evaluating accuracy.

3. **Fuzzy C-Means Clustering (Pima Dataset):**
   - Perform fuzzy C-means clustering after data cleaning to identify clusters in the Pima dataset and compare groupings with diabetes outcomes.

4. **Gaussian Mixture Clustering (Pima Dataset):**
   - Conduct Gaussian mixture clustering and calculate Mahalanobis distances between clusters to compare their relationship with diabetes outcomes.

5. **Hierarchical Clustering (Gender_Outlier Dataset):**
   - Apply hierarchical clustering using single link, complete link, group average, and Ward’s method.
   - Evaluate the clustering performance using the DB index and analyze the majority class within each group.

---

## How to Run

1. Clone this repository:
   ```
   git clone https://github.com/yourusername/Data_Mining.git
   cd Data_Mining
   ```

2. Ensure you have the required packages installed, such as:
   - R libraries: `NbClust`, `factoextra`, `ggplot2`
   - Python packages: `scikit-learn`, `pandas`, `matplotlib`

3. Navigate to the appropriate homework folder to run individual scripts or Jupyter notebooks.

---

## Datasets

The datasets used in these homework assignments include:
- `gender_outlier.csv`
- `pima.csv`
- `Asia_stock.csv`
- `winequality-red.csv` & `winequality-white.csv`
- `bank.csv`

Make sure these datasets are properly loaded before running the analyses.

---

## Data mining techniques used: 

Outlier detection, clustering, regression, and association rule mining, applied to real-world datasets. 

---
