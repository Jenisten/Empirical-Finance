rm(list = ls())

#install.packages("vrtest")
#install.packages("forecast")
#install.packages("FinTS")

lapply(c("car", "lmtest", "sandwich", "tseries", "quantmod", "PortfolioAnalytics", "ROI.plugin.quadprog", "readxl", "zoo", "vrtest", "forecast", "FinTS"), library, character.only = TRUE)

ex_5 <- read_excel("exercise 5 data OMXC25.xlsx")
ex_5 <- ts(ex_5)
ts.plot(ex_5)
#Printing the head and tail of the data
print(head(ex_5))
print(tail(ex_5))
# Assuming the data is log returns.

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
