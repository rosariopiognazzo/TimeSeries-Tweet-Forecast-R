# README - Statistical and Temporal Analysis of French-Language Tweets on the Russia-Ukraine War

## Project Description

The goal of this work is to conduct an in-depth statistical analysis of French-language tweets published between _February 24, 2023_ and _March 4, 2023_, concerning the Russia-Ukraine war. The study will initially focus on exploring the quantitative variables present in the dataset, with particular attention to identifying possible relationships between them. The objective is to develop statistical models capable of describing and explaining the interactions among the variables, analyzing how they influence each other. Subsequently, the focus will shift to the temporal analysis of the data. Not only will the trend of tweets over time be described, but statistical models will also be constructed to **represent the time series**, evaluating the potential of these models to make predictions (**forecasting**) based on the data. The developed models will then be tested and compared to identify the most effective one. Finally, the last part of the project will explore the use of large language models (LLMs) to generate **synthetic datasets**. The aim is to verify whether such models can produce _twin_ datasets that maintain the same structure and information as the original ones, but with a reduced number of observations. This is to assess whether their use could be helpful in drawing conclusions without processing a massive amount of data.

## Project Structure

The project is divided into the following sections:

1. **Introduction**: Overview of the analysis and objectives.
2. **Dataset Analysis**:
   - Description of variables.
   - Data cleaning and outlier treatment.
   - Analysis of distributions and data transformations.
3. **Relationships Between Variables**:
   - Calculation of correlations.
   - Linear regression for predicting followers and scores.
   - Multinomial regression for sentiment prediction.
4. **Time Series Analysis**:
   - Identification of seasonal patterns and tweet trends.
   - Time series decomposition.
5. **Forecasting with Machine Learning Models**:
   - _ARIMA_ models for forecasting scores and number of tweets.
   - _Auto-Regressive Neural Network_ for predictive analysis.
   - Model comparison and performance evaluation.
6. **Synthetic Dataset Generation**:
   - Use of language models to create alternative datasets.
   - Evaluation of the reliability of synthetic data compared to real data.

## Technical Requirements

To run the project code, the following tools are required:

- **Programming Language**: R
- **Main Libraries**:
  - `ggplot2`, `GGally` for data visualization.
  - `forecast`, `tseries` for time series analysis.
  - `nnet`, `caret` for neural networks and regressions.
  - `dplyr`, `tidyverse` for data manipulation.

## Execution Instructions

1. **Download the dataset**: The dataset is available in the repository.
2. **Install the required libraries**:
   ```r
   install.packages(c("ggplot2", "GGally", "forecast", "tseries", "nnet", "caret", "dplyr", "tidyverse"))
