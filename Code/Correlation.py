# -*- coding: utf-8 -*-
"""Correlation.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1pcQov7LHIyYEnm0t8FcnJhD5hXRgDETW
"""

import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt

merged_data = pd.read_excel('/content/output_data(1).xlsx')

merged_data.drop(columns=['avg_max_temperature','avg_min_temperature'], inplace=True)
merged_data

# Drop rows with missing values
merged_data.dropna(inplace=True)

print(merged_data.describe())

correlation_matrix = merged_data.corr(method='pearson')

from scipy.stats import pearsonr
p_values = pd.DataFrame(np.zeros_like(correlation_matrix), columns=correlation_matrix.columns, index=correlation_matrix.index)

# Calculate p-values for each pair of variables
for row in correlation_matrix.index:
    for col in correlation_matrix.columns:
        corr_coef, p_value = pearsonr(merged_data[row], merged_data[col])
        p_values.at[row, col] = p_value

# Display the p-values DataFrame
print("P-values:")
print(p_values)

plt.figure(figsize=(8, 6))
sns.heatmap(correlation_matrix, annot=True, cmap='coolwarm', fmt='.2f', linewidths=0.5)
plt.title('Correlation Matrix')
plt.show()

correlation_spearman = merged_data.corr(method='spearman')

p_values1 = pd.DataFrame(np.zeros_like(correlation_spearman), columns=correlation_spearman.columns, index=correlation_spearman.index)

# Calculate p-values for each pair of variables
for row in correlation_spearman.index:
    for col in correlation_spearman.columns:
        corr_coef, p_value = pearsonr(merged_data[row], merged_data[col])
        p_values1.at[row, col] = p_value

# Display the p-values DataFrame
print("P-values:")
print(p_values1)

plt.figure(figsize=(8, 6))
sns.heatmap(correlation_spearman, annot=True,  fmt='.2f', linewidths=0.5)
plt.title('Correlation Matrix')
plt.show()