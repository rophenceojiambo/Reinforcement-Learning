---
title: "Reinforcement Learning for Stock Trading"
author: "Rophence Ojiambo & Anusha Kumar"
output:
  bookdown::html_document2:
  #bookdown::pstocks_data_document2:
    number_sections: no
    keep_md: true
always_allow_html: true
link-citations: yes
colorlinks: yes
linkcolor: blue
urlcolor: blue
bibliography: references.bib
csl: statistics.csl
---






<center>
**Reading Time: 11 minute(s)**
</center>
<br>

<figure>
  <center>
    <img src="Figures/fig1.jpg" style="width:100%; height:50%">
    <figcaption>Image Source:[CNET](https://www.cnet.com/personal-finance/investing/its-been-a-wild-ride-stock-market-predictions-for-the-next-year/) </figcaption>
  </center>
</figure>

# Learning by Reward: Monkey Basketball and Robot Laundry Examples






```{=html}
<div style="display: flex;" align="top">
<div style="flex: 1;">
<p>Imagine you're trying to train a group of monkeys to play basketball. Every time a monkey makes a successful shot, you give it a banana. If the monkey misses the shot, you don't give it a banana. Over time, the monkeys start to associate making successful shots with receiving a banana and start to develop strategies to improve their chances of making a successful shot, such as passing the ball to a teammate or practicing their aim. Better still, Imagine you are training a robot to do laundry. You set up a reinforcement learning algorithm where the robot receives a positive reward (high five) when it successfully sorts the clothes, adds the correct amount of detergent, and washes the clothes without damaging them. </p>
<p>The robot receives a negative reward (no high five) if it damages the clothes, spills detergent, or makes a mistake in the sorting process. At first, the robot has no idea how to do laundry. It may accidentally mix whites and colors or add too much detergent, resulting in negative rewards. However, through trial and error, the robot gradually learns the correct sequence of actions to take when doing laundry. As the robot continues to do laundry over time, the robot starts to associate successfully washing and drying laundry with receiving a high-five and starts to optimize its laundry skills to maximize the chances of getting a high-five. It refines its strategy and learns to adapt to different types of fabrics and stains. It may adjust the water temperature or the amount of detergent based on the type of fabric being washed or selecting the correct cycle.</p>
</div>
<div style="flex: 1; text-align: center;">
<a href="https://tenor.com/en-GB/view/robot-laundry-overdrying-ez2on-happy-gif-25415746" target="_blank">Source: Robot Laundry GIF</a>
<img src="Figures/robot-laundry.gif" width="100%"/>
</div>
</div>
```


Over time, the robot becomes your skilled laundry assistant, capable of doing laundry efficiently and effectively. Now imagine that you change the scenario by adding a new type of fabric or stain that the robot has never encountered before. The robot may initially struggle to determine the correct cycle or water temperature or detergent amount to use. However, by experimenting with different settings and observing the results, the robot can learn to adjust its strategy to handle the new fabric or stain type. These are just but a few examples of reinforcement learning. The robot/monkeys are the agents, the shot/laundry task are the actions, the banana/high-five are the rewards, and the association between the action and the reward is the policy. By using a reward signal to guide the agents' behavior, you can train them to learn a specific task or behavior. This also demonstrates how reinforcement learning algorithms can adapt to new and unexpected situations, even in complex tasks like doing laundry.

In the world of stock trading, timing is everything. It's no secret that the stock market is unpredictable, with prices rising and falling based on everything from global events to social media trends. Recently, stocks like Bitcoin, Dogecoin, GameStop and Tesla have captured the attention of investors and the general public alike [1,2]. Whether it's buying low and selling high or knowing when to hold onto a stock for the long term, making the right decision can mean the difference between profit and loss. But how can traders stay on top of market trends and make informed decisions in such a volatile environment? One answer is reinforcement learning, a form of machine learning that trains algorithms to learn from past actions and outcomes and make better decisions over time [3, 4]. In this project, we'll explore how reinforcement learning is revolutionizing stock trading and potentially even changing the way we think about financial investment decisions.

# What is Reinforcement Learning?

Reinforcement learning (RL) is a type of machine learning algorithm that enables an agent to learn how to make decisions based on rewards and punishments. It involves an agent interacting with an environment, taking actions, receiving feedback, and adjusting its behavior based on that feedback [3, 4]. The goal of RL is to optimize the agent's behavior to maximize the cumulative reward it receives over time. RL algorithms are used in various applications such as robotics, gaming, finance, and natural language processing [5, 6, 7, 8]. RL is based on the concept of trial and error. Consider an individual placed in an unfamiliar environment. Initially, mistakes may occur, but through learning from them, they can avoid repeating them in the future when faced with similar circumstances. RL employs a similar approach in training its model, whereby the agent tries different actions and observes the resulting rewards or punishments, and then adjusts its behavior to maximize the expected reward in the future. 

## Key Elements of Reinforcement Learning

<figure>
  <center>
    <img src="Figures/fig2.jpg" style="width:50%">
    <figcaption>The agent-environment interaction in reinforcement learning framework (Source: Sutton and Barto, 1998)</figcaption>
  </center>
</figure>
<br>


- **Agent:** The agent in RL is the decision-making entity that interacts with the environment. In the stock trading analogy, the agent could be the trader who decides when or which stocks to buy and sell.

- **Environment:** The environment in RL is the world in which the agent operates and interacts with. In the stock trading analogy, the environment would be the stock market, which includes all the stocks, their prices, and the various events that affect the market.

- **State (S):** A state in RL refers to the current situation of the environment, as observed by the agent. In the stock trading analogy, a state would include the current prices of the stocks the trader is interested in, the current state of the economy, and any other relevant factors that may influence the decision to buy or sell.

- **Policy ($\pi$):** A policy in RL is a set of rules or instructions that the agent uses to determine its actions in a given state. In the stock trading analogy, a policy could be the algorithm that the agent uses to determine which stocks to buy and sell based on the current state.

- **Action (A):** In RL, the action is the decision made by the agent based on the current state and the policy. In the stock trading analogy, the action would be the trader's decision to hold, buy or sell a specific stock.

- **Reward (R):** A reward in RL is a feedback signal that the agent receives after taking an action. It indicates how good or bad the action was in achieving the agent's objective. In the stock trading analogy, the reward would be the profit or loss made from the trade.

- **Penalty:** A penalty in RL is a negative feedback signal that the agent receives after taking an action that is not desired or expected. In the stock trading analogy, the penalty would be the loss incurred from a bad trade or missed opportunity..


## Advantages of Reinforcement Learning

- Learning through trial and error: Unlike supervised learning, where the algorithm is provided with labeled data, RL algorithms learn by trial and error, without explicit supervision thus making it well-suited for applications where it is difficult or impractical to provide labeled data.

- Flexibility: RL can be used in both single-agent and multi-agent settings. In a single-agent setting, the agent learns to optimize its behavior by interacting with the environment while in a multi-agent setting, multiple agents learn to interact with each other and optimize their behavior collectively. 

- Adaptability: RL algorithms can adapt to changes in the environment and adjust their behavior accordingly. This makes RL well-suited for dynamic and uncertain environments where traditional machine learning approaches may struggle.

- Exploration: RL algorithms are designed to explore new strategies and actions in order to maximize their rewards. This can lead to the discovery of novel solutions and approaches that may not have been considered otherwise.

## Challenges of Reinforcement Learning

- Exploration-exploitation tradeoff: RL algorithms need to balance between exploring the environment to find new and potentially better actions, and exploiting the knowledge they already have to maximize rewards in the past. This tradeoff can be challenging, especially in complex environments with many possible actions.

- Generalization: RL algorithms may struggle to generalize their learned policies to new, unseen environments or tasks that were not encountered during training, which can limit their usefulness in practice.
Data efficiency: RL algorithms typically require a large amount of data to learn a good policy, which can be time-consuming and expensive to obtain.

- Reward engineering: The quality of the learned policy depends heavily on the reward function used, which can be difficult to design in a way that accurately reflects the desired behavior.

- Safety and ethics: RL agents can learn to take actions that are harmful or unethical, especially if the reward function is not carefully designed or the agent's behavior is not appropriately constrained.

- Interpretability: RL algorithms can be difficult to interpret and explain, especially when they use complex models or operate in high-dimensional state and action spaces.


# Reinforcement Learning Algorithms

There are several categories of reinforcement learning algorithms that can be used for stock trading, including:

- **Model-Based Reinforcement Learning Algorithms:** These algorithms learn the model from data and use it to optimize the agent's behavior and plan actions that maximize the expected cumulative reward to predict the outcomes of actions. Model-based algorithms are computationally efficient and require less data to learn than model-free algorithms. However, they may suffer from errors in the learned model, which can lead to sub optimal behavior. Examples of model-based reinforcement learning algorithms for stock trading include: Dynamic Programming, Monte Carlo methods and Temporal Difference Learning

- **Value-Based Reinforcement Learning Algorithms:** These are model-free that  learn an estimate of the optimal value function and use it to derive an optimal policy. These  algorithms are more robust to errors in the environment model, but they require more data to learn and can be computationally expensive. Examples of value-based reinforcement learning algorithms for stock trading include: Q-Learning, Deep Q-Networks (DQNs), and Double DQNs.

- **Policy-Based Reinforcement Learning Algorithms:** These algorithms learn the optimal policy directly, without estimating the value function. Examples of policy-based reinforcement learning algorithms for stock trading include:REINFORCE, SARSA, Proximal Policy Optimization (PPO) and Actor-Critic

- **Hybrid Reinforcement Learning Algorithms:** These algorithms combine elements of value-based and policy-based reinforcement learning. Examples of hybrid reinforcement learning algorithms for stock trading include: Trust Region Policy Optimization (TRPO) and Asynchronous Advantage Actor-Critic (A3C)

In practice, the choice of reinforcement learning algorithm for stock trading depends on the specific task and the characteristics of the environment. For example, a value-based algorithm like DQN may be well-suited for a simple trading environment with discrete actions, while a policy-based algorithm like PPO may be better for a more complex environment with continuous actions. Each of these algorithms has different advantages and disadvantages, and their performance can vary depending on the specific problem being addressed and on several factors, such as: (1)Efficiency and speed of convergence, (2) Performance on historical data, (3) Robustness to market changes, (4)Ability to handle high-dimensional state and action space, and (5) Interpretability: how easily the algorithm's decisions can be interpreted and understood. Ultimately, the choice of algorithm will depend on the specific goals of the trader, the particular characteristics of the stock market being traded, and the desired performance metrics.


Next, we expand more on the algorithms we will use for our data

## 1. Q-Learning

Q-learning is a popular value-based reinforcement learning algorithm based on the well known Bellman equation:

<figure>
  <center>
    <img src="Figures/fig3.jpg" style="width:50%">
</figure
<br>

Where; V(s) is the value of the current state s,  E refers to the expectation, while λ refers to the discount factor that determines the importance of future rewards.  From the above definition of the Bellman’s equation, the action value function can be expressed as:

<figure>
  <center>
    <img src="Figures/fig4.jpg" style="width:50%">
</figure
<br>

In Q-learning, the agent uses an iterative approach to update the Q-function estimates and learn an estimate of the optimal action-value based on the rewards obtained from each function. The optimal Q-value, denoted as Q* can be expressed as:

<figure>
  <center>
    <img src="Figures/fig5.jpg" style="width:50%">
</figure
<br>

In the context of stock trading, Q-learning can be used to learn the optimal buying and selling decisions for a given stock, based on historical price data and other market indicators. The agent can learn to maximize its expected profit over a given time horizon, taking into account the risks and uncertainties associated with stock trading. The Q-learning algorithm can be described by the following update rule:

<figure>
  <center>
    <img src="Figures/fig6.jpg" style="width:100%">
</figure
<br>
Where α is the learning rate, which determines the extent to which new information overrides old information and how fast we approach our goal. 

The algorithm is summarized as below:



<center>
![[Q-learning pseudo code](https://martin-thoma.com/images/2016/07/q-learning.png)](Figures/q-learning.png)


## 2. State-Action-Reward-State-Action (SARSA)

SARSA (State-Action-Reward-State-Action) is a RL algorithm that is used for online and on-policy learning. It is similar to Q-learning, but instead of updating the Q-value of the current state-action pair using the maximum Q-value of the next state, SARSA updates the Q-value using the Q-value of the next state-action pair, that is, it learns the Q-value of the policy being followed. The SARSA algorithm can be described by the following update rule:

<figure>
  <center>
    <img src="Figures/fig7.jpg" style="width:100%">
</figure
<br>

where: $Q(s_t, a_t)$ is the Q-value of the current state-action pair $(s_t, a_t)$; $\alpha$ is the learning rate that determines the impact of the new information on the existing Q-value; $r_{t+1}$ is the reward obtained after taking action $a_t$ in state $s_t$; $\gamma$ is the discount factor that determines the importance of future rewards; $Q(s_{t+1}, a_{t+1})$ is the Q-value of the next state-action pair $(s_{t+1}, a_{t+1})$ obtained using the current policy.

The algorithm is summarized as below:



<center>
![[SARSA pseudo code](https://martin-thoma.com/images/2016/07/sarsa-lambda.png)](Figures/sarsa.png)


## 3. Deep Q-network (DQN)

Deep Q-Network (DQN), is a type of RL algorithm that was introduced in 2015 and uses deep neural networks to approximate the Q-values of state-action pairs in a Markov decision process [@mnih2015human]. 

<figure>
  <center>
    <img src="Figures/DRL.pNg" style="width:100%">
    <figcaption>The agent-environment interaction in Deep Reinforcement Learning [@mao2016resource]
    </figcaption>
  </center>
</figure
<br>

The DQN algorithm uses experience replay and target networks to improve the stability and efficiency of the learning process. Experience replay involves storing the agent's experiences (i.e., state, action, reward, next state) in a replay buffer and sampling mini-batches of experiences randomly from the buffer to train the neural network. 

Target networks involve creating a separate neural network with the same architecture as the Q-network but with frozen weights to estimate the target Q-values used in the Bellman equation. This reduces the correlation between the target and predicted Q-values, which improves the stability of the learning process. 

The update equation for Deep Q-Network (DQN) algorithm can be written as follows:

$$
Q(s,a) = Q(s,a) + \alpha [r + \gamma \max_{a'} Q'(s',a') - Q(s,a)]
$$
Where: $Q(s,a)$ is the estimated Q-value for state s and action a; $r$ is the reward obtained after taking action a in state $s; s'$ is the next state after taking action $a$ in state $s; a'$ is the action with the highest Q-value in state $s'; Q'(s',a')$ is the target Q-value for the next state-action pair, computed using a separate target network that is periodically updated with the main network; $\alpha$ is the learning rate; $\gamma$ is the discount factor, which determines the weight given to future rewards.

The algorithm is summarized as below:




<center>
![DQN pseudo code[@mnih2015human]](Figures/dqn.jpg)

# Application to Stock Data

In this section, we will discover and explore data from the stock market, downloaded from yahoo finance. Below are the variable descriptions:

> **symbol:**  The name or ticker symbol of the stock, Apple (AAPL), Amazon (AMZN), Johnson & Johnson stock (JNJ), and NFLX. 
>
> **date:** The date of the stock price.
>
> **open:** The opening price of the stock on a given day.
>
> **high:** The highest price that the stock traded at during the day.
>
> **low:** The lowest price that the stock traded at during the day.
>
> **volume:**The number of shares of the stock that were traded on a given day.
>
> **adjusted:** The adjusted closing price of the stock on a given day, which takes into account any corporate actions, such as stock splits or dividends, that affect the stock price.


To download stock data for Apple, Amazon, Johnson & Johnson, and Netflix in R, we can use the `tidyquant` package. 


```r
library(tidyquant) # for stock data

# Define a vector with the stock symbols of interest
symbols <- c("AAPL", "AMZN", "JNJ", "NFLX")

# Use the tq_get() function to download the stock data for each symbol and bind them into one data frame
stocks_data <- symbols %>%
  tq_get(get = "stock.prices", from = "2010-01-01", to = "2022-12-31") %>%
  group_by(symbol) %>%
  mutate(date = as.Date(date)) %>%
  ungroup() %>%
  select(symbol, date, everything())
```



```{=html}
<div class="datatables html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-719f8404263a80715915" style="width:100%;height:auto;"></div>
```


![](Reinforcement-Learning_files/figure-html/unnamed-chunk-8-1.png)<!-- -->




# References

<div id= "refs"></div>