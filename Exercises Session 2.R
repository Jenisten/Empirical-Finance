rm(list = ls())
#install.packages("vrtest")
#install.packages("forecast")
lapply(c("car", "lmtest", "sandwich", "tseries", "quantmod", "PortfolioAnalytics", "ROI.plugin.quadprog", "readxl", "zoo", "vrtest", "forecast"), library, character.only = TRUE)

# EXERCIES SESSION 2

# Read excel file
ex_2 <- read_excel("Exercise 2 data SP500.xls")
ts.plot(ex_2$Index)
print(head(ex_2))

#Exercise 1
#1a
#1b

#RW1 assumes that returns are i.i.d variables, i.e. independent and identically distributed.
#RW2 states that returns are not necessarily independent, but must follow the same distribution.
#RW3 RW3 states that returns must be uncorrelated through time, but may be non-linearly related and come from a different distribution. 

#1c


#Exercise 2
#2a
#2b
#2c
#2d
#2e


#Exercise 3
#3a
ex_2_ts = ts(ex_2$Index)
print(ex_2_ts)

# 3b
log_ret = diff(log(ex_2_ts))
ts.plot(log_ret)
print(log_ret)

#3c # Capital A removes the lag-0 which will be 1 everytime. 
Acf(log_ret, lag=24, main="ACF of log returns")

#3d
Box.test(log_ret, lag = 24, type = "Ljung-Box")
#p-value 0.02051: Can reject the null there is at least one autocorrelation different from 0

#3e
Lo.Mac(log_ret, 2)
Lo.Mac(log_ret, 5)
Lo.Mac(log_ret, 10)

# Or written as a vector
kvec <- c(2,5,10)
Lo.Mac(log_ret,kvec)

#                           M1        M2
#Variance ratio = 2  2.9107230 2.2596853 - We reject the null here because > 1.96
#Variance ratio = 5  1.0578006 0.8848846 - We do not reject the null here because < 1.96 
#Variance ratio = 10 0.1339781 0.1124738 - We do not reject the null here because < 1.96 

# Most appropriate would be to reject RW1 and RW3 because we also reject in the Ljung test and we have limited data (261 observations)

#3f
model = arima(log_ret, order = c(1,0,0) # (1,0,0) We only change first and third. Middle is always 0.
print(model)
#Coefficients:
 #     intercept
  #       0.0017
#s.e.     0.0009

#sigma^2 estimated as 0.0001957:  log likelihood = 743.98,  aic = -1483.96
# Agrees with other tests which indicates that there is predictability. Intercept is not significant which means that the unconditional mean fluctuate around 0.


#3g Testing residuals with LB Test: They should not have autocorrelation. Because the model itself is built on autocorrelation with returns. We don't want to reject. 
residuals <- residuals(model)
lb_test_result <- Box.test(residuals, type = "Ljung-Box", lag = 24)
print(lb_test_result)
#X-squared = 40.169, df = 24, p-value = 0.02051
#Can reject