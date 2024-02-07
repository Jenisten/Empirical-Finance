rm(list = ls())

lapply(c("car", "lmtest", "sandwich", "tseries", "quantmod", "PortfolioAnalytics", "ROI.plugin.quadprog", "readxl", "zoo"), library, character.only = TRUE)

# Read excel file
ex_1 <- read_excel("exercise 1 data.xlsx")
print(head(ex_1))
ts.plot(ex_1$energy)
# Exersice 1.a
# Should be 0 on average.

# Exersice 1.b
#Abnormal returns are thosee that does not depend on risk factors. 

# Exersice 1.c
# That the shccks are zero mean uncorrelated, meaning white noise.

# Exersice 2

# Excersice 3.1
# Calculate excess return
ex_1$excess_return <- ex_1$energy - ex_1$rf
print(head(ex_1))

# Calculate mean of excess return
mean(ex_1$energy)
mean_excess_return <- mean(ex_1$excess_return)
print(mean_excess_return)

# Exercice 3.2
Box.test(ex_1$excess_return, lag = 26, type = "Ljung-Box")
# we can reject the null hypothesis that the data is white noise. (p-value < 0.05).

# Exercice 3.3

r_it <- ex_1$energy
f1 <- ex_1$f1
f2 <- ex_1$f2
f3 <- ex_1$f3


# Contructing the linear regression models
model <- lm (r_it ~ f1 + f2 + f3)

summary(model)

Ab_ret = model$coefficients[1] + model$residuals
print(mean(Ab_ret))

# Extract the intercept
intercept <- model$coefficients[1]
print(intercept)

# Abnormal returns is the sum of the intercept and the shocks (residuals). 
