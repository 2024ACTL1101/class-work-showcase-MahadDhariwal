---
title: "ACTL1101 Assignment Part B"
author: "Mahad Hassan Dhariwal"
date: "2024 T2"
output:
  word_document: default
  pdf_document: default
  html_document:
    df_print: paged
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(quantmod)
library(ggplot2)
library(tidyverse)
```

# CAPM Analysis

## Introduction

In this assignment, you will explore the foundational concepts of the Capital Asset Pricing Model (CAPM) using historical data for AMD and the S&P 500 index. This exercise is designed to provide a hands-on approach to understanding how these models are used in financial analysis to assess investment risks and returns.

## Background

The CAPM provides a framework to understand the relationship between systematic risk and expected return, especially for stocks. This model is critical for determining the theoretically appropriate required rate of return of an asset, assisting in decisions about adding assets to a diversified portfolio.

## Objectives

1. **Load and Prepare Data:** Import and prepare historical price data for AMD and the S&P 500 to ensure it is ready for detailed analysis.
2. **CAPM Implementation:** Focus will be placed on applying the CAPM to examine the relationship between AMD's stock performance and the overall market as represented by the S&P 500.
3. **Beta Estimation and Analysis:** Calculate the beta of AMD, which measures its volatility relative to the market, providing insights into its systematic risk.
4. **Results Interpretation:** Analyze the outcomes of the CAPM application, discussing the implications of AMD's beta in terms of investment risk and potential returns.

## Instructions

### Step 1: Data Loading

- We are using the `quantmod` package to directly load financial data from Yahoo Finance without the need to manually download and read from a CSV file.
- `quantmod` stands for "Quantitative Financial Modelling Framework". It was developed to aid the quantitative trader in the development, testing, and deployment of statistically based trading models.
- Make sure to install the `quantmod` package by running `install.packages("quantmod")` in the R console before proceeding.

```{r load-data}
# Set start and end dates
start_date <- as.Date("2019-05-20")
end_date <- as.Date("2024-05-20")

# Load data for AMD, S&P 500, and the 1-month T-Bill (DTB4WK)
amd_data <- getSymbols("AMD", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
gspc_data <- getSymbols("^GSPC", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
rf_data <- getSymbols("DTB4WK", src = "FRED", from = start_date, to = end_date, auto.assign = FALSE)

# Convert Adjusted Closing Prices and DTB4WK to data frames
amd_df <- data.frame(Date = index(amd_data), AMD = as.numeric(Cl(amd_data)))
gspc_df <- data.frame(Date = index(gspc_data), GSPC = as.numeric(Cl(gspc_data)))
rf_df <- data.frame(Date = index(rf_data), RF = as.numeric(rf_data[,1]))  # Accessing the first column of rf_data

# Merge the AMD, GSPC, and RF data frames on the Date column
df <- merge(amd_df, gspc_df, by = "Date")
df <- merge(df, rf_df, by = "Date")
```

#### Data Processing 
```{r data}
colSums(is.na(df))
# Fill N/A RF data
df <- df %>%
  fill(RF, .direction = "down") 
```

### Step 2: CAPM Analysis

The Capital Asset Pricing Model (CAPM) is a financial model that describes the relationship between systematic risk and expected return for assets, particularly stocks. It is widely used to determine a theoretically appropriate required rate of return of an asset, to make decisions about adding assets to a well-diversified portfolio.

#### The CAPM Formula
The formula for CAPM is given by:

\[ E(R_i) = R_f + \beta_i (E(R_m) - R_f) \]

Where:

- \( E(R_i) \) is the expected return on the capital asset,
- \( R_f \) is the risk-free rate,
- \( \beta_i \) is the beta of the security, which represents the systematic risk of the security,
- \( E(R_m) \) is the expected return of the market.



#### CAPM Model Daily Estimation

- **Calculate Returns**: First, we calculate the daily returns for AMD and the S&P 500 from their adjusted closing prices. This should be done by dividing the difference in prices between two consecutive days by the price at the beginning of the period.
$$
\text{Daily Return} = \frac{\text{Today's Price} - \text{Previous Trading Day's Price}}{\text{Previous Trading Day's Price}}
$$

```{r return}
options(scipen = 999)

# Create two new columns and calculate the respective numbers for each column
df <- df %>%
  mutate(
    AMD_Return = (AMD - lag(AMD)) / lag(AMD),
    GSPC_Return = (GSPC - lag(GSPC)) / lag(GSPC)
  )

# Traverse through the new columns' row and change any NA to 0
for (i in 1:nrow(df)) {
  
  if (is.na(df$AMD_Return[i])) {
    df$AMD_Return[i] = 0
  }
  
  if (is.na(df$GSPC_Return[i])) {
    df$GSPC_Return[i] = 0
  }
}

```

- **Calculate Risk-Free Rate**: Calculate the daily risk-free rate by conversion of annual risk-free Rate. This conversion accounts for the compounding effect over the days of the year and is calculated using the formula:
$$
\text{Daily Risk-Free Rate} = \left(1 + \frac{\text{Annual Rate}}{100}\right)^{\frac{1}{360}} - 1
$$

```{r riskfree}
options(scipen = 999)

# Creates a new column and calculates the daily risk-free rate using the formula provided
df <- df %>%
  mutate(
    Daily_RF = (1 + RF / 100) ^ (1 / 360) - 1
  )
```


- **Calculate Excess Returns**: Compute the excess returns for AMD and the S&P 500 by subtracting the daily risk-free rate from their respective returns.

```{r excess return}
options(scipen = 999)

# Creates two new column and calculates the excess returns of AMD and S&P 500 by subtracting daily risk-free rate from their respective returns
df <- df %>%
  mutate(
    AMD_Excess_Returns = AMD_Return - Daily_RF,
    GSPC_Excess_Returns = GSPC_Return - Daily_RF
  )
```


- **Perform Regression Analysis**: Using linear regression, we estimate the beta (\(\beta\)) of AMD relative to the S&P 500. Here, the dependent variable is the excess return of AMD, and the independent variable is the excess return of the S&P 500. Beta measures the sensitivity of the stock's returns to fluctuations in the market.

```{r lm}
# Create a linear model of the excess returns of AMD and S&P 500 to estimate the beta of AMD relative to S&P 500
linear_model <- lm(AMD_Excess_Returns ~ GSPC_Excess_Returns, data = df)

# Print out data of the linear model
summary(linear_model)

# Get the value of 𝛽 and print it
beta <- coef(linear_model)[2]
cat("The value of 𝛽 is", beta, ".")
```


#### Interpretation

What is your \(\beta\)? Is AMD more volatile or less volatile than the market?

**Answer:**
The regression analysis indicates that AMD has a \(\beta\) of approximately 1.57. This implies that compared to the market, AMD's stock is more sensitive to market movements. Subsequently, the implication for the investment risk is higher volatility. Since AMD's \(\beta\) is greater than 1, it suggests the a higher volatility than the market. This signifies that AMD's stock is more susceptible to market movements, ultimately causing larger fluctuations in the stock price and according to the regression analysis, it signifies that it is 57% more volatile. Due to AMD's stock price being highly sensitive to market movement, it allows for higher increase in stock prices compared to competitors, if the market is performing well. Conversely, if the market is performing poorly, their stock price is likely to decrease greater than compared to competitors. Due to greater fluctuations in price and the associated risk with these fluctuations, it allows for investors who have a higher risk tolerance to find AMD a favourable investment, whereas, conservative investors to abstain from AMD. This occurs as the higher risk is associated with higher expected returns in the Capital Asset Pricing Model. As stated earlier, if the market is performing poorly, this will cause investor to lose more money due to the higher volatility. Overall, while the higher beta indicates the potential for greater returns, it also signifies the higher risk associated with investing in AMD.

#### Plotting the CAPM Line
Plot the scatter plot of AMD vs. S&P 500 excess returns and add the CAPM regression line.

```{r plot}
# Create a scatter plot of AMD vs. S&P 500 excess returns and add the CAPM regression line
ggplot(df, aes(x = GSPC_Excess_Returns, y = AMD_Excess_Returns)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "AMD vs. S&P 500 Excess Returns"
  ) + 
  theme_minimal()
```
The above scatter plot the relationship between the excess returns of AMD plotted against the excess returns of the S&P 500. The regression line which has slope that corresponds to AMD's \(\beta\) (approximately 1.57), indicates a positive correlation between AMD's and the market's excess returns. This suggests that AMD's returns are more volatile and react more sensitively to market movements compared to the overall market. The grey area in the graph represents the confidence interval for the regression line.


### Step 3: Predictions Interval
Suppose the current risk-free rate is 5.0%, and the annual expected return for the S&P 500 is 13.3%. Determine a 90% prediction interval for AMD's annual expected return.

*Hint: Calculate the daily standard error of the forecast ($s_f$), and assume that the annual standard error for prediction is $s_f \times \sqrt{252}$. Use the simple return average method to convert daily stock returns to annual returns if needed.*


**Answer:**

```{r pi}
# Get the value of 𝛽
beta <- coef(linear_model)[2]

# Convert the daily risk-free rate
current_rf <- 0.05
daily_rf <- (1 + current_rf) ^ (1 / 252) - 1

# Convert the annual expected return of S&P 500 to daily expected return
gspc_annual_expected_return <- 0.133
gspc_daily_expected_return <- (1 + gspc_annual_expected_return) ^ (1 / 252) - 1

# Calculate the daily and annual standard error of the forecast
sf_daily <- sd(residuals(linear_model))
sf_annual <- sf_daily * sqrt(252)

# Calculate the annual and daily expected return of AMD
amd_daily_expected_return <- daily_rf + beta * (gspc_daily_expected_return - daily_rf)
amd_annual_expected_return <- (1 + amd_daily_expected_return) ^ (252) - 1

# Calculate the z-score of a 90% prediction interval
z_score <- qnorm(0.95)
# Calculate the lower bound of the interval
lower_bound <- amd_annual_expected_return - z_score * sf_annual
# Calculate the upper bound of the interval
upper_bound <- amd_annual_expected_return + z_score * sf_annual

# Print the results
cat("The 90% prediction interval for AMD's annual expected return is [", lower_bound, ",", upper_bound, "]")
```
This means that there is a 90% chance that AMD's annual return will fall within this range. This interval highlights the high volatility and uncertainty associated with AMD's stock returns, reflecting the higher risk for potential investors.
