rm(list = ls())

#install.packages("vrtest")
#install.packages("forecast")
#install.packages("FinTS")
#install.packages("rugarch")

lapply(c("car", "lmtest", "sandwich", "tseries", "quantmod", "PortfolioAnalytics", "ROI.plugin.quadprog", "readxl", "zoo", "vrtest", "forecast", "FinTS", "rugarch"), library, character.only = TRUE)

ex_5 <- read_excel("exercise 5 data OMXC25.xlsx")
ts_5 <- ts(ex_5)
ts.plot(ts_5, start = 1, end = 1003, frequency = 1)
#Printing the head and tail of the data
print(head(ts_5))
print(tail(ts_5))
# Assuming the data is log returns.

#creating GARCH model specs
spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), mean.model = list(armaOrder = c(0,0), include.mean = TRUE), distribution.model = "norm")

#Testing the model
fit <- ugarchfit(spec, ts_5)
print(fit)
# mu is the mean
# omega is the constant
# alpha1 is the garch coefficient or the reaction to shocks
# beta1 is the arch coefficient or the importance of persistency. 
# sigma is the standard deviation of the residuals.

# sign bias test is to test for leverage effect.
# The null hypothesis is that there is no leverage effect.

# It is important to focus on the following tests in the results: 
# Weighted Ljung-Box Test on Standardized Residuals
# Weighted Ljung-Box Test on Standardized Squared Residuals
# Weighted ARCH LM Test
# Sign Bias Test

# 3.1
Acf(ex_4, main = "ACF of log returns") # lag 6 is above the CI. But could be an error.
Pacf(ex_4, main = "PACF of log returns")
# 3.2
Box.test(ex_4, type = "Ljung-Box")
#p-value 0.135: Cannot reject the null there is no autocorrelation. No predictability. 
#Variance Ratio test
kvec <- c(2,3,5)
Lo.Mac(ex_4,kvec)
#                           M1        M2
# Variance ratio = 2  1.4839378 1.2589998 - We do not reject the null here because < 1.96
# Variance ratio = 3  1.1117969 0.9360020 - We do not reject the null here because < 1.96
# Variance ratio = 5  0.4686537 0.3922138 - We do not reject the null here because < 1.96

# 3.3
ex_4_s <- ex_4^2 # (4) from answer sheet
Acf(ex_4_s, main = "ACF of log returns squared")
Pacf(ex_4_s, main = "PACF of log returns squared")
# 3.4
Box.test(ex_4_s, type = "Ljung-Box")
# p-value 0.0006652: Can reject the null. There is evidence of autocorrelation.
# Variance Ratio test
Lo.Mac(ex_4_s, kvec)

#                           M1        M2
# Variance ratio = 2  3.395908 3.306509 - We do reject the null here because > 1.96
# Variance ratio = 3  4.986695 3.746806 - We do reject the null here because > 1.96
# Variance ratio = 5  6.961240 4.567463 - We do reject the null here because > 1.96

#3.5 
ArchTest(ex_4, lags = 4, demean = TRUE)
ArchTest(ex_4, lags = 8, demean = TRUE)
ArchTest(ex_4, lags = 12, demean = TRUE)

# 3.6

# 3.7

# 3.8

# 3.9

# 3.10