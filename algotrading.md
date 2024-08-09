---
title: "ACTL1101 Assignment Part A"
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
```

## Algorithmic Trading Strategy

## Introduction

In this assignment, you will develop an algorithmic trading strategy by incorporating financial metrics to evaluate its profitability. This exercise simulates a real-world scenario where you, as part of a financial technology team, need to present an improved version of a trading algorithm that not only executes trades but also calculates and reports on the financial performance of those trades.

## Background

Following a successful presentation to the Board of Directors, you have been tasked by the Trading Strategies Team to modify your trading algorithm. This modification should include tracking the costs and proceeds of trades to facilitate a deeper evaluation of the algorithm’s profitability, including calculating the Return on Investment (ROI).

After meeting with the Trading Strategies Team, you were asked to include costs, proceeds, and return on investments metrics to assess the profitability of your trading algorithm.

## Objectives

1. **Load and Prepare Data:** Open and run the starter code to create a DataFrame with stock closing data.

2. **Implement Trading Algorithm:** Create a simple trading algorithm based on daily price changes.

3. **Customize Trading Period:** Choose your entry and exit dates.

4. **Report Financial Performance:** Analyze and report the total profit or loss (P/L) and the ROI of the trading strategy.

5. **Implement a Trading Strategy:** Implement a trading strategy and analyze the total updated P/L and ROI. 

6. **Discussion:** Summarise your finding.


## Instructions

### Step 1: Data Loading

Start by running the provided code cells in the "Data Loading" section to generate a DataFrame containing AMD stock closing data. This will serve as the basis for your trading decisions. First, create a data frame named `amd_df` with the given closing prices and corresponding dates. 

```{r load-data}

# Load data from CSV file
amd_df <- read.csv("AMD.csv")

# Convert the date column to Date type and Adjusted Close as numeric
amd_df$date <- as.Date(amd_df$Date)
amd_df$close <- as.numeric(amd_df$Adj.Close)

amd_df <- amd_df[, c("date", "close")]
```


##Plotting the Data
Plot the closing prices over time to visualize the price movement.
```{r plot}
plot(amd_df$date, amd_df$close,'l')
```


## Step 2: Trading Algorithm
Implement the trading algorithm as per the instructions. You should initialize necessary variables, and loop through the dataframe to execute trades based on the set conditions.

- Initialize Columns: Start by ensuring dataframe has columns 'trade_type', 'costs_proceeds' and 'accumulated_shares'.
- Change the algorithm by modifying the loop to include the cost and proceeds metrics for buys of 100 shares. Make sure that the algorithm checks the following conditions and executes the strategy for each one:
  - If the previous price = 0, set 'trade_type' to 'buy', and set the 'costs_proceeds' column to the current share price multiplied by a `share_size` value of 100. Make sure to take the negative value of the expression so that the cost reflects money leaving an account. Finally, make sure to add the bought shares to an `accumulated_shares` variable.
  - Otherwise, if the price of the current day is less than that of the previous day, set the 'trade_type' to 'buy'. Set the 'costs_proceeds' to the current share price multiplied by a `share_size` value of 100.
  - You will not modify the algorithm for instances where the current day’s price is greater than the previous day’s price or when it is equal to the previous day’s price.
  - If this is the last day of trading, set the 'trade_type' to 'sell'. In this case, also set the 'costs_proceeds' column to the total number in the `accumulated_shares` variable multiplied by the price of the last day.



```{r trading}
# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- "NA"
amd_df$costs_proceeds <- 0  # Corrected column name
amd_df$accumulated_shares <- 0  # Initialize if needed for tracking

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

# Traverse through each day and calculate and input the numbers and types for the corresponding day
for (i in 1:nrow(amd_df)) {
  
  # Calculate the difference between the close price of the current day and the previous one
  if (i > 1) {
    previous_price <- amd_df$close[i] - amd_df$close[i - 1]
  }
  
  # Buys on the first day and on everyday where the close cost is cheaper than the day before except for the last day
  if (i == 1 || previous_price < 0 && i != nrow(amd_df)) {
    amd_df$trade_type[i] <- "buy" # Sets the current day's trade type to buy
    amd_df$costs_proceeds[i] <- -share_size * amd_df$close[i] # Calculate how much the purchase will cost and input into the portfolio
    accumulated_shares <- accumulated_shares + share_size # Calculate how many of the shares are currently being held in the portfolio
  }
  
  amd_df$accumulated_shares[i] <- accumulated_shares # Input number of share holding into the current day
  
  # Sell all the shares on the last day
  if (i == nrow(amd_df)) {
    amd_df$trade_type[i] <- "sell" # Change the trade type to sell as the shares are being sold since it's the last day
    amd_df$costs_proceeds[i] <- accumulated_shares * amd_df$close[i] # Calculate how much the total shares sold for and input into the portfolio
    amd_df$accumulated_shares[i] <- 0 # Set the number of shares holding on the last day to zero as all the shares have been sold
  }
}
```

This was achieved by using the previous_price and having finding the difference of the close price between the current day and previous day, however, it ignores the first day since there is no previous day. Then buys a share whether its the first day and check if the previous_day variable is negative indicating the current day close price is lower value than the previous day as current day close price was cheaper than the previous day and ignores the last day as no shares are going to be bought on days that they are sold. Finally, once it reaches the last day it would sell all the shares and changes their respective entries to new values.

## Step 3: Customize Trading Period
- Define a trading period you wanted in the past five years 
```{r period}
# Load data from CSV file
amd_df <- read.csv("AMD.csv")

# Convert the date column to Date type and Adjusted Close as numeric
amd_df$date <- as.Date(amd_df$Date)
amd_df$close <- as.numeric(amd_df$Adj.Close)

amd_df <- amd_df[, c("date", "close")]

# Initialize dates
start_date <- as.Date("2023-01-03") # Initializing start date
end_date <- as.Date("2024-04-1") # Initializing end date

# Restrict the dates in the table to start at the start_date and end at the end_date
amd_df <- amd_df[amd_df$date >= start_date & amd_df$date <= end_date, ]

# Reset the row index to start from 1
row.names(amd_df) <- NULL

# Rerun step 2 with new dates to get new results
```

This was achieved using the as.Date function then having the dates restricted between the start and end dates, as well as resetting the index to start from 1.

## Step 4: Run Your Algorithm and Analyze Results
After running your algorithm, check if the trades were executed as expected. Calculate the total profit or loss and ROI from the trades.

- Total Profit/Loss Calculation: Calculate the total profit or loss from your trades. This should be the sum of all entries in the 'costs_proceeds' column of your dataframe. This column records the financial impact of each trade, reflecting money spent on buys as negative values and money gained from sells as positive values.
- Invested Capital: Calculate the total capital invested. This is equal to the sum of the 'costs_proceeds' values for all 'buy' transactions. Since these entries are negative (representing money spent), you should take the negative sum of these values to reflect the total amount invested.
- ROI Formula: $$\text{ROI} = \left( \frac{\text{Total Profit or Loss}}{\text{Total Capital Invested}} \right) \times 100$$

```{r}

# Run step 3 then step 2 to calculate for chosen trading period
# Run step 1 then step 2 to calculate over entire period

# Initialise the variables for required calculating profit/loss and ROI
total_profit_or_loss <- 0
total_capital_invested <- 0 # Required to calculate the ROI
roi <- 0

# Traverse through each day and add up each respective numbers in the portfolio
for (i in 1:nrow(amd_df)) {
  
  # Check if the column isn't listed as "NA" then add up each days cost proceeds (the amount earned or lost each time shares were bought and sold)
  total_profit_or_loss <- total_profit_or_loss + amd_df$costs_proceeds[i] # Once the for loop is finished that will be total profit/loss as it added up each cost proceed

  
  # Checks whether the column isn't listed as 'NA' and is listed as buy as we only want to find capital invested which is all the cost proceed that occur when you buy
  if (length(amd_df$trade_type[i]) > 0 && amd_df$trade_type[i] == "buy") {
    total_capital_invested <- total_capital_invested - amd_df$costs_proceeds[i] # Once the for loop is finished it will be the total capital invested. The '-' accounts for the cost proceeds being listed as negative
  }
}

roi <- (total_profit_or_loss / total_capital_invested) * 100 # Formula for ROI as given in step 4 instructions

# Prints the results
cat("The total profit or loss was, $", total_profit_or_loss, ".\n")
cat("The ROI is", roi, "%")
```

## Step 5: Profit-Taking Strategy or Stop-Loss Mechanisum (Choose 1)
- Option 1: Implement a profit-taking strategy that you sell half of your holdings if the price has increased by a certain percentage (e.g., 20%) from the average purchase price.
- Option 2: Implement a stop-loss mechanism in the trading strategy that you sell half of your holdings if the stock falls by a certain percentage (e.g., 20%) from the average purchase price. You don't need to buy 100 stocks on the days that the stop-loss mechanism is triggered.


```{r option}
# Profit-Taking Strategy
# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- "NA"
amd_df$costs_proceeds <- 0  # Corrected column name
amd_df$accumulated_shares <- 0  # Initialize if needed for tracking
amd_df$average_purchase_price <- 0

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0
accumulated_purchase <- 0 # Used to calculate average share price, it's the cost of all the purchases

# Traverse through each day and calculate and input the numbers and types for the corresponding day
for (i in 1:nrow(amd_df)) {
  
  # Calculate the difference between the close price of the current day and the previous one
  if (i > 1) {
    previous_price <- amd_df$close[i] - amd_df$close[i - 1]
  }
  
  # Determines whether to sell half shares early if the close price is 1.5 times the average purchase price unless if it's the first or last day
  if (i != 1 && amd_df$accumulated_shares[i - 1] != 0 && i != nrow(amd_df) && amd_df$close[i] >= 1.5 * amd_df$average_purchase_price[i - 1]) {
    accumulated_shares = 0.5 * accumulated_shares # The new number of shares after half are sold
    amd_df$costs_proceeds[i] = accumulated_shares * amd_df$close[i] # Calculate the amount the earned by selling half the shares and stores it in the portfolio
    amd_df$trade_type[i] <- "sell" # Sets the trade to sell in the portfolio
    accumulated_purchase <- 0.5 * accumulated_purchase # The new total cost of purchase of all the current shares
    
  } else if (i == 1 || previous_price < 0 && i != nrow(amd_df)) { # Buys on the first day and on everyday where the close cost is cheaper than the day before except for the last day
    amd_df$trade_type[i] <- "buy" # Sets the current day's trade type to buy
    amd_df$costs_proceeds[i] <- -share_size * amd_df$close[i] # Calculate how much the purchase will cost and input into the portfolio
    accumulated_shares <- accumulated_shares + share_size # Calculate how many of the shares are currently being held in the portfolio
    accumulated_purchase <- accumulated_purchase + share_size * amd_df$close[i] # Adds up the new shares purchased to the to the total cost of purchase
  }
  
  # Calculates the average purchase price which is accumulated purchases (the cost of purchasing the shares) divided by the total number of shares currently being held
  amd_df$average_purchase_price[i] = accumulated_purchase / accumulated_shares
  
  amd_df$accumulated_shares[i] <- accumulated_shares # Input number of share holding into the current day
  
  # Sell all the shares on the last day
  if (i == nrow(amd_df)) {
    amd_df$trade_type[i] <- "sell" # Change the trade type to sell as the shares are being sold since it's the last day
    amd_df$costs_proceeds[i] <- accumulated_shares * amd_df$close[i] # Calculate how much the total shares sold for and input into the portfolio
    amd_df$accumulated_shares[i] <- 0 # Set the number of shares holding on the last day to zero as all the shares have been sold
  }
}
```

This uses a similar strategy when buying and selling (only at the end), but has been adjusted to sell half the stock when it close price has increased by 50%. This was achieved by first calculating the accumulated purchase when the shares were being bought where it added the cost it took to purchase the new lot of share and when they were being sold, it was noticed since half the shares were being sold the accumulated purchase was also halved using then calculating the accumulated shares by adding up every share purchased before and halving the shares when sold. Then total average purchase price was accumulated purchase / accumulated shares. Since, only either buy or sell could happen it was so that it sells and not buys even if it can buy. And only sells when it's not the first day, there are shares and not the last day as all the shares will be sold.

## Step 6: Summarize Your Findings
- Did your P/L and ROI improve over your chosen period?
- Relate your results to a relevant market event and explain why these outcomes may have occurred.


```{r}
# Calculate ROI and profit/loss using profit-taking strategy
# Run step 3 then step 5 to calculate for chosen trading period
# Run step 1 then step 5 to calculate over entire period

# Initialise the variables for required calculating profit/loss and ROI
total_profit_or_loss <- 0
total_capital_invested <- 0 # Required to calculate the ROI
roi <- 0

# Traverse through each day and add up each respective numbers in the portfolio
for (i in 1:nrow(amd_df)) {
  
  # Check if the column isn't listed as "NA" then add up each days cost proceeds (the amount earned or lost each time shares were bought and sold)
  if (!is.na(amd_df$costs_proceeds[i])) {
    total_profit_or_loss <- total_profit_or_loss + amd_df$costs_proceeds[i] # Once the for loop is finished that will be total profit/loss as it added up each cost proceed
  }
  
  # Checks whether the column isn't listed as 'NA' and is listed as buy as we only want to find capital invested which is all the cost proceed that occur when you buy
  if (!is.na(amd_df$trade_type[i]) && amd_df$trade_type[i] == "buy") {
    total_capital_invested <- total_capital_invested - amd_df$costs_proceeds[i] # Once the for loop is finished it will be the total capital invested. The '-' accounts for the cost proceeds being listed as negative
  }
}

roi <- (total_profit_or_loss / total_capital_invested) * 100 # Formula for ROI as given in step 4 instructions

# Prints the results
cat("The total profit or loss was, $", total_profit_or_loss, ".\n")
cat("The ROI is", roi, "%")
```

My Discussion:
In May 2023, AMD introduced their first ever AI engine on an x86 window process, which reflected in an increase in their stocks in May suggesting that investors believed in the potential this product would provide potential growth to AMD's AI performance against the expensive Nvidia. This would allow AMD to gain a competitve advantage in the near future with prospects of increased profit margins. This was reflected in December 2023 where they had another rapid increase in the share price, as they introduced a new graphics processor that would be used in conjunction with their AI servers with Microsoft and Meta as devoted clients, leading potential investor to believe that their initial investment was a good idea causing them to invest further into the business in hopes of increased ROI in the near future (Leswing, 2023). This lead to AMD achieving an all-time closing stock price of 211.38 on March 07, 2024. In accordance with this information I set my period from the start of 2023 to start of April the following year. This allowed first strategy earned 984612.9 dollars with an ROI of 56.35528%, whereas, my second strategy (profit-taking strategy) earned 703744.2 dollars with an ROI of 55.66707%. The higher profit using the first strategy was obtained since the accumulative shares were all sold at a higher price that occurred due to the market event above compared to selling shares as time passes since some of the shares were sold at a lower prices, similarly this resulted in a sightly ROI compared to the second strategy. The ROI in the second strategy was a result of being sold when the close price has been increased by 50% compared to the average purchase price, as shares are sold at this value they are being sold in order to achieve an ROI of about 50%


Sample Discussion: On Wednesday, December 6, 2023, AMD CEO Lisa Su discussed a new graphics processor designed for AI servers, with Microsoft and Meta as committed users. The rise in AMD shares on the following Thursday suggests that investors believe in the chipmaker's upward potential and market expectations; My first strategy earned X dollars more than second strategy on this day, therefore providing a better ROI.


## References
- Leswing, K. (2023, December 6). Meta and Microsoft say they will buy AMD’s new AI chip as an alternative to Nvidia’s. CNBC. https://www.cnbc.com/2023/12/06/meta-and-microsoft-to-buy-amds-new-ai-chip-as-alternative-to-nvidia.html



