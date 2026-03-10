# Statistical and Temporal Analysis of French-Language Tweets on the Russia-Ukraine War

Authors: Rosario Pio Gnazzo, Antonio Pauzano

---

## Project Description

The goal of this work is to conduct an in-depth statistical analysis of French-language tweets published between _February 24, 2023_ and _March 4, 2023_, concerning the Russia-Ukraine war. The study initially focuses on exploring the quantitative variables present in the dataset, with particular attention to identifying possible relationships between them. The objective is to develop statistical models capable of describing and explaining the interactions among the variables, analyzing how they influence each other.

Subsequently, the focus shifts to the temporal analysis of the data. Not only is the trend of tweets over time described, but statistical models are also constructed to **represent the time series**, evaluating their potential to make predictions (**forecasting**) based on the data. The developed models are then tested and compared to identify the most effective one.

Finally, the last part of the project explores the use of large language models (LLMs) to generate **synthetic datasets**. The aim is to verify whether such models can produce _twin_ datasets that maintain the same structure and statistical properties as the original, but with a reduced number of observations — assessing whether their use could support drawing conclusions without processing the full volume of data.

---

## Project Structure

1. **Dataset Analysis** — Description of variables, data cleaning and outlier treatment, analysis of distributions and data transformations.
2. **Relationships Between Variables** — Correlation analysis, linear regression for predicting `followers` and `score`, multinomial logistic regression for sentiment prediction.
3. **Time Series Analysis** — Identification of seasonal patterns and tweet trends, time series decomposition (STL, additive, multiplicative, Fourier).
4. **Forecasting with Statistical Models** — ARIMA models for forecasting scores and tweet counts, Auto-Regressive Neural Network (NNAR) for predictive analysis, model comparison and performance evaluation (RMSE, MAE, MAPE).
5. **Synthetic Dataset Generation** — Use of LLMs to create alternative datasets, evaluation of the reliability of synthetic data compared to the original.

---

## Repository Structure

```
.
├── Sentiment_fr_tweet_2023.csv       # Original dataset (not included — see data/README.md)
├── data/
│   ├── README.md                     # Dataset provenance and instructions
│   ├── dataset_sintetico_small.json  # Small synthetic dataset (LLM-generated)
│   └── dataset_sintetico_large.json  # Large synthetic dataset (LLM-generated)
├── img/                              # Output plots
├── descrizione-dataset.Rmd       # 1. Exploratory data analysis
├── ts_analysis.Rmd               # 2. Time series analysis — sentiment score
├── ts_analysis2.Rmd              # 3. Time series analysis — tweet counts
├── LLMs.Rmd                      # 4. Comparison with synthetic datasets
└── PROMPT-SAD.txt                # Prompt used to generate synthetic datasets
```

---

## Notebooks

Run the notebooks in the following order:

1. **`descrizione-dataset.Rmd`** — Loads and cleans the dataset. Performs descriptive statistics, outlier detection, normality tests, correlation analysis, linear regression, and multinomial logistic regression on the `sentiment` variable.

2. **`ts_analysis.Rmd`** — Aggregates the sentiment `score` variable into time series at 10-minute, 30-minute, and 1-hour granularity. Applies stationarity tests (KPSS, Ljung-Box), builds ARIMA and NNAR models, and evaluates forecast accuracy (RMSE, MAE, MAPE).

3. **`ts_analysis2.Rmd`** — Aggregates tweet counts per sentiment class into time series. Performs periodicity analysis (seasonal plots, ACF/PACF), applies decomposition models (STL, additive, multiplicative, Fourier + moving average), and evaluates model fit.

4. **`LLMs.Rmd`** — Applies the same EDA pipeline from notebook 1 to two LLM-generated synthetic datasets (small and large). Compares distributions and statistical properties against the original dataset.

---

## Prerequisites

- R version 4.0 or higher
- RStudio (recommended for knitting R Markdown files)

Install all required packages by running the following in an R console:

```r
install.packages(c(
  # Visualization
  "ggplot2", "GGally", "corrplot", "gridExtra", "ggpubr",
  # Data manipulation
  "dplyr", "tidyr", "readr", "reshape2", "tibble", "lubridate",
  # Statistical modelling
  "nnet", "psych", "nortest",
  # Time series
  "forecast", "highfrequency", "xts", "urca",
  # Synthetic data
  "jsonlite"
))
```

---

## Dataset

The original dataset (`Sentiment_fr_tweet_2023.csv`) is not included in this repository because redistribution of raw tweet data is prohibited under the Twitter/X Terms of Service.

See [data/README.md](data/README.md) for the full dataset description, variable definitions, and instructions on how to obtain and place the file.

---

## Synthetic Datasets

Two synthetic datasets were generated using ChatGPT based on the statistical summary of the original dataset. The prompt used is available in `Progetto_Gnazzo_Pauzano/PROMPT-SAD.txt`.

- `data/dataset_sintetico_small.json` — Reduced synthetic dataset
- `data/dataset_sintetico_large.json` — Full-scale synthetic dataset
