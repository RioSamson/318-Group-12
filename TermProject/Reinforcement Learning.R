library(ReinforcementLearning)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(cowplot)
library(fmsb)


states <- as.character(seq(1, 100))
actions <-
  c("Stocks",
    "Real_Estates",
    "Commodities",
    "Cryptocurrencies",
    "Forex")

# profit is a portion of the invested money
profit <- function(investment)
{
  floor(2*runif(1) * investment)
  
}


market.env <- function(state, action)
{
  # agent's budgets
  budget <- as.numeric(state)
  
  # reward defined as a function below to calculate the new budget
  calculate_reward <- function(budget, investment, expected_profit)
  {
    # for affordable items
    if ((budget - investment) > 0)
    {
      reward = log(budget - investment + expected_profit)
    }
    
    # and if the agent wants to invest on something out of budget
    else
    {
      reward = budget - investment + expected_profit
    }
    
  } # Function R
  
  # initial new budget
  next_state <- budget
  
  if (state == state(budget) && action == "Stocks")
  {
    expected_profit <- profit(5)
    next_state <-
      state(budget - 5 + expected_profit)
    reward <- calculate_reward(budget, 5, expected_profit)
  }
  
  if (state == state(budget) && action == "Real_Estates")
  {
    expected_profit <- profit(17)
    next_state <-
      state(budget - 17 + expected_profit)
    reward <- calculate_reward(budget, 17, expected_profit)
  }
  
  if (state == state(budget) && action == "Commodities")
  {
    expected_profit <- profit(11)
    next_state <-
      state(budget - 11 + expected_profit)
    reward <- calculate_reward(budget, 11, expected_profit)
  }
  
  if (state == state(budget) && action == "Cryptocurrencies")
  {
    expected_profit <- profit(9)
    next_state <-
      state(budget - 9 + expected_profit)
    reward <- calculate_reward(budget, 9, expected_profit)
  }
  
  if (state == state(budget) && action == "Forex")
  {
    expected_profit <- profit(7)
    
    next_state <-
      state(budget - 7 + expected_profit)
    reward <- calculate_reward(budget, 7, expected_profit)
  }
  
  
  out <- list(NextState = as.character(next_state), Reward = reward)
  return(out)
}

data <- sampleExperience(
  N = 3000,
  env = market.env,
  states = states,
  actions = actions,
  actionSelection = "random",
)


#### UNSEEN DATA ####

data_unseen <- data.frame(State = as.character(seq(15, 45)),
                          stringsAsFactors = FALSE)

#################################################################################

# reinforcement learning parameters
control <- list(alpha = 0.1, gamma = 0.6, epsilon = 0.1)

# reinforcement learning
model <- ReinforcementLearning(data, 
                               s = "State", 
                               a = "Action", 
                               r = "Reward", 
                               s_new = "NextState", 
                               control = control, iter = 10)

q_table <- model$Q

#making the policy
policy <- computePolicy(model)
policy <- data.frame(policy)

summary(model)

# applying the model to unseen data
data_unseen$OptimalAction <- predict(model, data_unseen$State)
 


