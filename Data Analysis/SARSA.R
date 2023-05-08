# load necessary libraries
library(tidyverse)
library(ggplot2)

set.seed(2650) # for reproducibility

# download Netflix stock data

train <- read.csv("Data/train.csv") 

train <- data.frame(train) %>% na.omit()

test <- read.csv("Data/test.csv") 

test <- data.frame(test) %>% na.omit()

names(train)
#------------------------------------------------------------------------------#

# extract closing prices from data
closing_prices <- train$close

# define state based on previous 3 closing prices
define_state <- function(closing_prices, i) {
  if (i < 4) {
    # if not enough data points yet, use the first one as the state
    state <- closing_prices[1]
  } else {
    # use previous 3 closing prices as the state
    state <- closing_prices[(i-3):i]
  }
  return(state)
}
################################################################################


# define SARSA function with buy, sell, and hold actions

sarsa <- function(data, alpha = 0.1, gamma = 0.9, epsilon = 0.1) {
  
  # input validation
  if (!is.data.frame(data)) {
    stop("Data must be a data frame.")
  }
  if (!is.numeric(alpha) || alpha < 0 || alpha > 1) {
    stop("Alpha must be a number between 0 and 1.")
  }
  if (!is.numeric(gamma) || gamma < 0 || gamma > 1) {
    stop("Gamma must be a number between 0 and 1.")
  }
  if (!is.numeric(epsilon) || epsilon < 0 || epsilon > 1) {
    stop("Epsilon must be a number between 0 and 1.")
  }
  
  # initialize Q table with row and column names
  Q <- matrix(0, nrow = 3, ncol = nrow(data), 
              dimnames = list(c("buy", "sell", "hold"), paste0("state", 1:nrow(data))))
  
  # initialize state
  state <- define_state(data$close, 1)
  
  # initialize portfolio value and trades list
  portfolio_value <- 1
  trades <- list()
  
  # define epsilon-greedy policy function
  policy <- function(state, Q, epsilon) {
    
    if (runif(1) < epsilon) {
      # choose a random action
      action <- sample(c("buy", "sell", "hold"), 1)
    } else {
      # choose the action with highest Q-value
      action <- names(which.max(Q[, state]))
    }
    return(action)
  }
  
  # loop through stock data
  for (t in 2:length(data$close)) {
    
    # choose action using epsilon-greedy policy
    action <- policy(state, Q, epsilon)
    
    # take action and observe next state and reward
    if (action == "buy") {
      portfolio_value <- portfolio_value * (1 + (data$close[t] - data$close[t-1])/data$close[t-1])
      
      next_state <- t
      reward <- log(portfolio_value)
      
    } else if (action == "sell") {
      portfolio_value <- portfolio_value * (1 - (data$close[t] - data$close[t-1])/data$close[t-1])
      
      next_state <- t
      reward <- log(portfolio_value)
      
    } else {
      # hold action has no effect on portfolio value
      next_state <- t
      reward <- 0
    }
    
    # choose next action using epsilon-greedy policy
    next_action <- policy(next_state, Q, epsilon)
    
    # update Q-value for current state and action
    Q[action, state] <- Q[action, state] + alpha*(reward + gamma*Q[next_action, next_state] - Q[action, state])
    
    # update state and record trade
    state <- next_state
    trades[[t-1]] <- list(state = state, action = action, reward = reward)
  }
  
  # return trades and Q table
  return(list(trades = trades, Q = Q))
}

#------------------------------------------------------------------------------#

# function that extracts the hyperparameters associated with the best result and returns them as a list

tune <- function(data, alpha_range, gamma_range, epsilon_range) {
  # initialize results list
  results <- list()
  
  # loop through alpha, gamma, and epsilon values
  for (alpha in alpha_range) {
    for (gamma in gamma_range) {
      for (epsilon in epsilon_range) {
        
        # run SARSA algorithm with current hyperparameters
        trades_and_Q <- sarsa(data, alpha = alpha, gamma = gamma, epsilon = epsilon)
        
        # calculate total reward and store results
        total_reward <- sum(unlist(lapply(trades_and_Q$trades, function(x) x$reward)))
        results[[length(results)+1]] <- list(alpha = alpha, gamma = gamma, epsilon = epsilon, total_reward = total_reward)
      }
    }
  }
  
  # return hyperparameters with highest total reward
  best_result_idx <- which.max(sapply(results, function(x) x$total_reward))
  best_result <- results[[best_result_idx]]
  
  return(list(alpha = best_result$alpha, gamma = best_result$gamma, epsilon = best_result$epsilon))
}

# ranges of hyperparameters to test
alpha_range <- seq(0.1, 1, by = 0.1)
gamma_range <- seq(0.1, 1, by = 0.1)
epsilon_range <- seq(0.1, 1, by = 0.1)

# This will return the hyperparameters with the highest total reward, 
# which we can then use to train your SARSA algorithm on the full stock data set.
best_hyperparameters <- tune(train, alpha_range, gamma_range, epsilon_range)

# $alpha [1] 0.2  $gamma [1] 0.4 $epsilon [1] 0.2

################################################################################

# run SARSA on train data
sarsa_result <- sarsa(train, alpha = 0.8, gamma = 0.3, epsilon = 0.1)

# extract Q table from SARSA result
Q_values <- sarsa_result$Q


# combine Q-values for buy and sell actions into one data frame
Q_df <- as.data.frame(cbind(Q_values[1,], Q_values[2,]))
colnames(Q_df) <- c("buy", "sell")

# add state column to data frame
Q_df$state <- 1:length(Q_values[1,])

# plot Q-values for buy and sell actions
ggplot(Q_df, aes(x = state)) +
  geom_line(aes(y = buy), color = "blue") +
  geom_line(aes(y = sell), color = "red") +
  xlab("State") +
  ylab("Q-value") +
  ggtitle("Q-values for Buy and Sell Actions") +
  theme_minimal()




# extract trades from SARSA output
trades <- sarsa_result$trades


# convert trades list to data frame
trades_df <- data.frame(do.call(rbind, trades))
trades_df$date <- train[2:nrow(train), 2]

# create portfolio value column
trades_df$portfolio_value <- c(exp(cumsum(trades_df$reward)))

# REDO this!

# plot portfolio value with vertical lines for trades
#ggplot(trades_df, aes(x = date, y = portfolio_value)) +
 # geom_line() +
  #geom_vline(data = subset(trades_df, action == "buy"), aes(xintercept = date), color = "green") +
  # geom_vline(data = subset(trades_df, action == "sell"), aes(xintercept = date), color = "red") +
  #xlab("Date") + ylab("Portfolio Value") +
  #ggtitle("Portfolio Value with Trades Marked") +
  #theme_minimal()

#------------------------------------------------------------------------------#
# THIS WORKS!

# extract buy/sell decisions from trades vector

# Code decision column
trades_df$decision <- ifelse(trades_df$action == "buy", 1,
                              ifelse(trades_df$action == "sell", -1, 0))
# extract the  closing prices

trades_df$prices <- train[2:nrow(train), 6]
trades_df$symbol <- train[2:nrow(train), 1]

# plot the closing prices with hold, buy and sell signals
ggplot(trades_df, aes(x = as.Date(date), y = prices)) +
  geom_line() +
  facet_wrap(~ symbol, ncol = 2) +
  geom_point(aes(color = factor(decision)), size = 1) +
  scale_color_manual(values = c("red", "darkgreen", "blue"), labels = c("Sell", "Hold", "Buy")) +
  labs(title = "Stock Prices with Decision Indicators", x = "Date", y = "Price", color = "Decision") +
  theme_tq_dark()


# TO DO: Plot portfolio value over time

#-----------------------------------------------------------------------------#

sarsa_test <- function(data, Q) {
  
  # input validation
  if (!is.data.frame(data)) {
    stop("Data must be a data frame.")
  }
  if (!is.matrix(Q) || nrow(Q) != 3 || ncol(Q) != length(data$close)) {
    stop("Q must be a 3xN matrix, where N is the number of data points.")
  }
  
  # initialize state
  state <- define_state(data$close, 1)
  
  # initialize portfolio value and trades list
  portfolio_value <- 1
  portfolio_values <- numeric(length(data$close))
  trades <- list()
  
  # loop through stock data
  for (t in 2:length(data$close)) {
    
    # choose action based on Q-table
    action <- names(which.max(Q[, state]))
    
    # take action and observe next state and reward
    if (action == "buy") {
      portfolio_value <- portfolio_value * (1 + (data$close[t] - data$close[t-1])/data$close[t-1])
      
      next_state <- t
      reward <- log(portfolio_value)
      
    } else if (action == "sell") {
      portfolio_value <- portfolio_value * (1 - (data$close[t] - data$close[t-1])/data$close[t-1])
      
      next_state <- t
      reward <- log(portfolio_value)
      
    } else {
      # hold action has no effect on portfolio value
      next_state <- t
      reward <- 0
    }
    
    # record trade
    trades[[t-1]] <- list(state = state, action = action, reward = reward)
    
    # update state
    state <- next_state
    
    # record portfolio value
    portfolio_values[t] <- portfolio_value
  }
  
  # return trades and portfolio value
  return(list(trades = trades, portfolio_value = portfolio_values))
}

#------------------------------------------------------------------------------#

# works great!


# retrain Q matrix on test data
Q_values_test <- sarsa(test)$Q

# test SARSA function on test data
sarsa_results_test <- sarsa_test(test, Q_values_test)



# extract trades and portfolio value from test results
test_trades <- sarsa_results_test$trades
test_portfolio_value <- sarsa_results_test$portfolio_value


# plot portfolio value over time
ggplot(data.frame(date = index(test), value = test_portfolio_value), aes(x = date, y = value)) +
  geom_line() +
  labs(title = "Portfolio Value over Time (Test Data)", x = "Date", y = "Portfolio Value")


# TO DO: plot closing prices with trading decisions

#------------------------------------------------------------------------------#

