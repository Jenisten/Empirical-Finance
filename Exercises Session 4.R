rm(list = ls())

#install.packages("vrtest")
#install.packages("forecast")
#install.packages("FinTS")

lapply(c("car", "lmtest", "sandwich", "tseries", "quantmod", "PortfolioAnalytics", "ROI.plugin.quadprog", "readxl", "zoo", "vrtest", "forecast", "FinTS"), library, character.only = TRUE)

ex_4 <- read_excel("Exercise Session 4 data.xlsx")
ts.plot(ex_4)
print(head(ex_4))
# Assuming the data is log returns.

# 3.1
Acf(ex_4, lag = 24, main = "ACF of log returns")
Pacf(ex_4, lag = 24, main = "PACF of log returns")
# 3.2
Box.test(ex_4, lag = 24, type = "Ljung-Box")
#p-value 0.02051: Can reject the null there is at least one autocorrelation different from 0
#Variance Ratio test
kvec <- c(2,3,5)
Lo.Mac(ex_4,kvec)
#                           M1        M2
#Variance ratio = 2  1.4839378 1.2589998 - We do not reject the null here because < 1.96
#Variance ratio = 3  1.1117969 0.9360020 - We do not reject the null here because < 1.96 
#Variance ratio = 5  0.4686537 0.3922138 - We do not reject the null here because < 1.96 

# 3.3
ex_4_s <- ex_4^2
Acf(ex_4_s, lag = 24, main = "ACF of log returns squared")
Pacf(ex_4_s, lag = 24, main = "PACF of log returns squared")
# 3.4
Box.test(ex_4_s, lag = 24, type = "Ljung-Box")
# p-value 0.7908: Can't reject the null. There is no evidence of autocorrelation.
# Variance Ratio test
Lo.Mac(ex_4_s, kvec)

#                           M1        M2
#Variance ratio = 2  0.1776086 0.740031 - We do not reject the null here because < 1.96
#Variance ratio = 3  0.9130378 1.345675 - We do not reject the null here because < 1.96 
#Variance ratio = 5  1.4281379 1.703849 - We do not reject the null here because < 1.96 
# 3.5
ArchTest(ex_4)
