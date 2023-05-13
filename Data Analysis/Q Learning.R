# Load the required libraries
library(quantmod)
library(fBasics)
library(ReinforcementLearning)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(knitr)
library(dplyr)
library(cowplot)

#Q_learning function
Q_learning <- function(prices) {
  # Define the initial values of the Q-matrix and the learning parameters
  n <- nrow(prices)
  n_stocks <- ncol(prices)
  Q_list <- vector("list", n_stocks)
  for (j in 1:n_stocks) {
    Q_list[[j]] <- matrix(0, nrow = n, ncol = 3) # Q-matrix for stock j with 3 actions
  }
  alpha <- 0.2 # Learning rate
  gamma <- 0.9 # Discount factor
  
  # Initialize an empty vector to store the actions taken
  actions_taken <- vector("numeric", n-1)
  
  # Loop through each time step
  for (i in 2:n) {
    # Loop through each stock
    for (j in 1:n_stocks) {
      # Define the state at time t-1
      state <- prices[i-1, j]
      
      # Select the action with the highest Q-value
      q_values <- Q_list[[j]][i-1, ]
      action <- sample(which(q_values == max(q_values)), 1)
      
      # Store the action taken at time t-1
      actions_taken[i-1] <- action
      
      # Define the reward and the next state at time t
      reward <- prices[i, j] - prices[i-1, j] # The reward is the change in the price of stock j
      next_state <- prices[i, j]
      
      # Update the Q-matrix using the Q-learning algorithm
      Q_list[[j]][i, action] <- Q_list[[j]][i-1, action] + alpha * (reward + gamma * max(Q_list[[j]][i-1, ]) - Q_list[[j]][i-1, action])
    }
  }
  
  # Create a list of Q-tables for each stock
  Q_table <- lapply(Q_list, function(x) {
    buy_action <- x[, 1] > x[, 2] & x[, 1] > x[, 3] # Buy if Q-value of buy is greater than Q-value of hold/sell
    sell_action <- x[, 2] > x[, 1] & x[, 2] > x[, 3] # Sell if Q-value of sell is greater than Q-value of hold/buy
    hold_action <- !buy_action & !sell_action # Hold if none of the above is true
    data.frame(Buy = buy_action, Sell = sell_action, Hold = hold_action)
  })
  
  # Return the list of Q-matrices (one for each stock), actions taken, and Q-tables
  return(list(Q_list = Q_list, actions_taken = actions_taken, Q_table = Q_table))
}



# Load the stock data
data <- getSymbols(c("AAPL", "AMZN", "JNJ", "NFLX"), src = "yahoo", from = "2010-01-01", to = "2022-12-31", auto.assign = TRUE)
# Extract adjusted prices
AAPL_prices <- Ad(AAPL)
AMZN_prices <- Ad(AMZN)
JNJ_prices <- Ad(JNJ)
NFLX_prices <- Ad(NFLX)
# Combine the stock price vectors into a matrix
prices <- cbind(AAPL_prices, AMZN_prices, JNJ_prices,NFLX_prices)
colnames(prices) <- gsub("\\..*", "", colnames(prices))
prices <- na.omit(prices)
prices_df <- as.data.frame(prices)





# generate summary statistics for the prices data
summary_prices <- as.data.frame(basicStats(prices_df))
# create the table using kable
results <- kable(summary_prices, format = "latex", align = "c")



# Load the stock data
library(quantmod)
data <- getSymbols(c("AAPL", "AMZN", "JNJ", "NFLX"), src = "yahoo", from = "2010-01-01", to = "2022-12-31", auto.assign = TRUE)
# Extract adjusted prices
AAPL_prices <- Ad(AAPL)
AMZN_prices <- Ad(AMZN)
JNJ_prices <- Ad(JNJ)
NFLX_prices <- Ad(NFLX)
# Combine the stock price vectors into a matrix
prices <- cbind(AAPL_prices, AMZN_prices, JNJ_prices,NFLX_prices)
colnames(prices) <- gsub("\\..*", "", colnames(prices))
prices <- na.omit(prices)
prices_df <- as.data.frame(prices)
library(ggplot2)
library(reshape2)
# Plot the distribution of prices for each stock
ggplot(melt(prices_df), aes(value, fill = variable)) + 
  geom_density(alpha = 0.5) + 
  scale_fill_manual(values = c("red", "blue", "green", "orange")) + 
  labs(x = "Adjusted Prices", y = "Density", title = "Distribution of Stock Prices")+theme_classic()







library(kableExtra)
#Q_learning function
Q_learning <- function(prices) {
  # Define the initial values of the Q-matrix and the learning parameters
  n <- nrow(prices)
  n_stocks <- ncol(prices)
  Q_list <- vector("list", n_stocks)
  for (j in 1:n_stocks) {
    Q_list[[j]] <- matrix(0, nrow = n, ncol = 3) # Q-matrix for stock j with 3 actions
  }
  alpha <- 0.2 # Learning rate
  gamma <- 0.9 # Discount factor
  
  # Initialize an empty vector to store the actions taken
  actions_taken <- vector("numeric", n-1)
  
  # Loop through each time step
  for (i in 2:n) {
    # Loop through each stock
    for (j in 1:n_stocks) {
      # Define the state at time t-1
      state <- prices[i-1, j]
      
      # Select the action with the highest Q-value
      q_values <- Q_list[[j]][i-1, ]
      action <- sample(which(q_values == max(q_values)), 1)
      
      # Store the action taken at time t-1
      actions_taken[i-1] <- action
      
      # Define the reward and the next state at time t
      reward <- prices[i, j] - prices[i-1, j] # The reward is the change in the price of stock j
      next_state <- prices[i, j]
      
      # Update the Q-matrix using the Q-learning algorithm
      Q_list[[j]][i, action] <- Q_list[[j]][i-1, action] + alpha * (reward + gamma * max(Q_list[[j]][i-1, ]) - Q_list[[j]][i-1, action])
    }
  }
  
  # Create a list of Q-tables for each stock
  Q_table <- lapply(Q_list, function(x) {
    buy_action <- x[, 1] > x[, 2] & x[, 1] > x[, 3] # Buy if Q-value of buy is greater than Q-value of hold/sell
    sell_action <- x[, 2] > x[, 1] & x[, 2] > x[, 3] # Sell if Q-value of sell is greater than Q-value of hold/buy
    hold_action <- !buy_action & !sell_action # Hold if none of the above is true
    data.frame(Buy = buy_action, Sell = sell_action, Hold = hold_action)
  })
  
  # Return the list of Q-matrices (one for each stock), actions taken, and Q-tables
  return(list(Q_list = Q_list, actions_taken = actions_taken, Q_table = Q_table))
}
# Apply the Q-learning function to each stock
Q_list <- Q_learning(prices_df)
#==================================================
#presentation of actions taken
#==================================================
# Generate summary Action table
Q_learning_summary <- function(prices) {
  # Define the names of the actions
  action_names <- c("Buy","Sell","Hold")
  
  # Run the Q_learning function and extract the actions taken
  Q_result <- Q_learning(prices)
  actions_taken <- Q_result$actions_taken
  
  # Convert the action codes to action names
  actions_taken <- factor(actions_taken, levels = 1:3, labels = action_names)
  
  # Create a summary table of the actions taken for each stock
  summary_table <- data.frame(
    Stock = rep(colnames(prices), each = nrow(prices)-1),
    Action_Taken = actions_taken,
    stringsAsFactors = FALSE
  )
  
  # Return the summary table
  return(summary_table)
}
summary_table <- Q_learning_summary(prices_df)
summary_table

# Split summary_table by stock
stock_summary <- split(summary_table, summary_table$Stock)

#tables for action for each stock
table(stock_summary$AAPL)
table(stock_summary$AMZN)
table(stock_summary$JNJ)
table(stock_summary$NFLX)

results1 <- kable(table(stock_summary$AAPL), format = "latex", align = "c")
results2 <- kable(table(stock_summary$AMZN), format = "latex", align = "c")
results3 <- kable(table(stock_summary$JNJ), format = "latex", align = "c")
results4 <- kable(table(stock_summary$NFLX), format = "latex", align = "c")





predict_prices <- function(Q, stock_prices) {
  # Define the initial values of the state and the predicted prices vector
  state <- stock_prices[1, ]
  predicted_prices <- numeric(nrow(stock_prices))
  
  # Loop through each time step
  for (i in 1:nrow(stock_prices)) {
    # Define the action as the one with the highest Q-value for the current state
    action <- which.max(Q[i, ])
    
    # Define the predicted price as the price of the chosen action
    predicted_prices[i] <- stock_prices[i, action]
    
    # Update the state to the next state based on the chosen action
    state <- stock_prices[i, ]
  }
  
  # Return the predicted prices vector
  return(predicted_prices)
}
# Predict the closing prices for each stock using the corresponding Q-matrix
AAPL_predicted_prices <- predict_prices(Q_list$Q_list[[1]], prices_df)
AMZN_predicted_prices <- predict_prices(Q_list$Q_list[[2]], prices_df)
JNJ_predicted_prices <- predict_prices(Q_list$Q_list[[3]], prices_df)
NFLX_predicted_prices <- predict_prices(Q_list$Q_list[[4]], prices_df)


# AAPL Plot
AAPL.df <- cbind(AAPL_predicted_prices, AAPL_prices[,1])
names(AAPL.df)[2] <- "Adjusted Prices"
names(AAPL.df)[1] <- "Closing_Price"
AAPL.plot <- ggplot(data = AAPL.df, aes(x = index(AAPL.df), y = Closing_Price)) + 
  geom_line() +
  geom_line(aes(y = `Adjusted Prices`, color = "Adjusted")) +
  xlab("Date") + ylab("AAPL Prices") +
  ggtitle("AAPL Predicted Closing and Adjusted Prices") + theme_tq()

# AMZN Plot
AMZN.df <- cbind(AMZN_predicted_prices, AMZN_prices[,1])
names(AMZN.df)[1] <- "Close"
names(AMZN.df)[2] <- "Adjusted"
AMZN.plot <- ggplot(data = AMZN.df, aes(x = index(AMZN.df), y = Close)) + 
  geom_line() +
  geom_line(aes(y = Adjusted, color = "Adjusted")) +
  xlab("Date") + ylab("AMZN Prices") +
  ggtitle("AMZN Predicted Closing and Adjusted Prices")+ theme_tq()

# JNJ Plot
JNJ.df <- cbind(JNJ_predicted_prices, JNJ_prices[,1])
names(JNJ.df)[1] <- "Close"
names(JNJ.df)[2] <- "Adjusted"
JNJ.plot <- ggplot(data = JNJ.df, aes(x = index(JNJ.df), y = Close)) + 
  geom_line() +
  geom_line(aes(y = Adjusted, color = "Adjusted")) +
  xlab("Date") + ylab("JNJ Prices") +
  ggtitle("JNJ Predicted Closing and Adjusted Prices")+ theme_tq()

# NFLX Plot
NFLX.df <- cbind(NFLX_predicted_prices, NFLX_prices[,1])
names(NFLX.df)[1] <- "Close"
names(NFLX.df)[2] <- "Adjusted"
NFLX.plot <- ggplot(data = NFLX.df, aes(x = index(NFLX.df), y = Close)) + 
  geom_line() +
  geom_line(aes(y = Adjusted, color = "Adjusted")) +
  xlab("Date") + ylab("NFLX Prices") +
  ggtitle("NFLX Predicted Closing and Adjusted Prices")+ theme_tq()

# Combine all plots
plots <- list(AAPL.plot, AMZN.plot, JNJ.plot, NFLX.plot)
gridExtra::grid.arrange(grobs = plots, ncol = 2)


#===============================================================#
#Train/test
#==============================================================#
# Split data into training and testing sets
# Convert rownames to a column 'Date'
prices_df$Date <- rownames(prices_df)
# Convert 'Date' column to Date format
prices_df$Date <- as.Date(prices_df$Date)
# Subset the data for the training set (2010-2016)
train_prices <- subset(prices_df, Date >= as.Date("2010-01-04") & Date <= as.Date("2016-12-31"))

# Subset the data for the test set (2017-2022)
test_prices <- subset(prices_df, Date >= as.Date("2017-01-01") & Date <= as.Date("2022-05-07"))

# Apply the Q-learning function to each stock using the training data
Q_list_train <- Q_learning(train_prices)

# Access Q matrix for each stock
AAPL_Q_train <- Q_list_train$Q_list[[1]]
AMZN_Q_train <- Q_list_train$Q_list[[2]]
JNJ_Q_train <- Q_list_train$Q_list[[3]]
NFLX_Q_train <- Q_list_train$Q_list[[4]]

# Use the Q matrices to predict prices for each stock using the testing data
AAPL_predicted_prices <- predict_prices(AAPL_Q_train, test_prices)
AMZN_predicted_prices <- predict_prices(AMZN_Q_train, test_prices)
JNJ_predicted_prices <- predict_prices(JNJ_Q_train, test_prices)
NFLX_predicted_prices <- predict_prices(NFLX_Q_train, test_prices)

# Evaluate the performance of the predictions using mean squared error
AAPL_mse <- mean((AAPL_predicted_prices - test_prices[, "AAPL"])^2)
AMZN_mse <- mean((AMZN_predicted_prices - test_prices[, "AMZN"])^2)
JNJ_mse <- mean((JNJ_predicted_prices - test_prices[, "JNJ"])^2)
NFLX_mse <- mean((NFLX_predicted_prices - test_prices[, "NFLX"])^2)



