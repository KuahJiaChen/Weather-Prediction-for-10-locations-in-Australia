# Weather-Prediction-for-10-locations-in-Australia

## Introduction
This repository contains the code and analysis for predicting whether tomorrow will be warmer than today based on weather data. The project explores different classifiers and models to predict the target variable, WarmerTomorrow.

## Background
The goal of this project is to develop a predictive model that can accurately determine whether the next day will be warmer than the current day using various weather attributes. The analysis involves exploring different classifiers, including Decision Trees, Bagging, Boosting, Random Forest, and Artificial Neural Networks, to identify the most effective model for this prediction task.

## Data Preparation
To perform the analysis, we used the following R libraries:
- `dplyr`: for data manipulation and transformation.
- `ggplot2`: for data visualization.
- `caret`: for machine learning algorithms.

## Conclusion
Based on the analysis conducted, it was found that the Bagging model produced the best results, achieving an accuracy of approximately 67.2% and an AUC of 0.698. This model outperformed other classifiers considered in Parts 4, 9, and 10, demonstrating its effectiveness in predicting the WarmerTomorrow attribute. The selected attributes for the model included MaxTemp, Sunshine, WindGustDir, WindDir9am, WindDir3pm, Humidity9am, Cloud9am, Cloud3pm, and Temp9am.

Feel free to explore the code and analysis in this repository to gain a better understanding of the predictive modeling process and the chosen classifiers.