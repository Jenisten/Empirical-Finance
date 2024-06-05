rm(list = ls())
lapply(c("car", "lmtest", "sandwich", "tseries", "quantmod", "PortfolioAnalytics", "ROI.plugin.quadprog", "readxl", "zoo", "vrtest", "forecast", "FinTS", "moments", "rugarch"), library, character.only = TRUE)

# Predictability of Asset Returns

# Question 1.6
# Reading the data
exchange <- read_excel("exchange.xls")
# Printing and plotting the data
print(head(exchange))
ts.plot(exchange)

# Converting the data into a time series
exchange_ts = ts(exchange$logreturn, start = 1, end = 262, frequency = 1)

# Determining the amount of lags
exchange_lags=10*log10(length(exchange_ts))
print(exchange_lags)

# Testing linear predictability with a Ljung Box test
Box.test(exchange_ts, lag = exchange_lags, type = "Ljung-Box")
# Since the p-value (0.08707) is greater than 0.05, we do not reject the null hypothesis. 
# This suggests that there is no significant autocorrelation up to the specified number of lags. 
# Further on, this indicates that the time series is not linearly predictable. 

# Question 1.7
# Testing for predictability using Lo-MacKinlay variance ratio test for 2 and day log returns
kvec <- c(2,3)
Lo.Mac(exchange_ts,kvec)
#                             M1         M2
# Variance ratio = 2  -0.2197445 -0.1922782 - We do not reject the null here because < 1.96
# Variance ratio = 3  -1.0414343 -0.9143965 - We do not reject the null here because < 1.96

# Question 1.8
# Plotting the ACF of the exchange rate
Acf(exchange_ts, lag= exchange_lags, main="ACF of Exchange Rate")
# The ACF plot shows that the autocorrelation is significant at lag 18 and 23.

# Question 1.9
# Plotting the PACF of the exchange rate
pacf(exchange_ts, lag= exchange_lags, main="PACF of Exchange Rate")
# The PACF plot shows that the partial autocorrelation is significant at lag 23. 

# Question 1.10
# Determining the best time series model using the auto.arima function
auto.arima(exchange_ts, d=0, D=0, ic=c("aic"), stepwise=FALSE, approximation=FALSE)
# Based on the BIC criterion, the best model is ARIMA(3,0,1) with a zero mean.
auto.arima(exchange_ts, d=0, D=0, ic=c("bic"), stepwise=FALSE, approximation=FALSE)
# Based on the BIC criterion, the best model is ARIMA(0,0,0) with a zero mean.

#vQuestion 1.11
# Fitting the ARIMA model
BM_fit <- arima(exchange_ts, order=c(0,0,0))
BM_fit

# Testing for White Noise in the residuals of the ARMA(0,0) model
Box.test(BM_fit$residuals, lag = exchange_lags, type = "Ljung-Box")
# Since the p-value (0.08707) is more than 0.05, we do not reject the null hypothesis. 
# This suggests that the residuals are white noise.


# Question 2.7
# Reading the data
logrets <- read_excel("logreturns.xlsx")

# Converting the data into a time series
logrets_ts = ts(logrets$logreturns, start = 1, end = 1000, frequency = 1)

# Squaring Log returns
logrets_sq <- logrets_ts^2

# Determining the amount of lags
logrets_lenght = length(logrets_ts)
logrets_lenght
logrets_lags=10*log10(logrets_lenght)
print(logrets_lags)

# Testing for linear predictability with a Ljung Box test
Box.test(logrets_sq, lag = logrets_lags, type = "Ljung-Box")
# The p-value is less than 0.05, so we reject the null hypothesis.
# This suggests that the time series is linearly predictable.

# Question 2.8
# Testing for predictability using Lo-MacKinlay variance ratio test for 2 and day log returns
kvec_2 <- c(2)
Lo.Mac(logrets_sq,kvec_2)
#                          M1       M2
# Variance ratio = 2 6.649091 2.188877 - We do reject the null here because > 1.96
# The time series does not follow a random walk.

# Question 2.9
# Conducting the ARCH-LM test
ArchTest(logrets_sq, lags = 4, demean = TRUE)
ArchTest(logrets_sq, lags = 8, demean = TRUE)
ArchTest(logrets_sq, lags = 12, demean = TRUE)
# The p-values are less than 0.05, so we reject the null hypothesis.
# This suggests that the time series is not homoscedastic. 

# Questiion 2.10
# Plotting the ACF of the log returns squared
Acf(logrets_sq, lag= logrets_lags, main="ACF of Log Returns Squared")

# Questiion 2.11
#Creating the first model: ARMA(0,0) with Gaussian distribution and non-zero mean
# create specification
spec1 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                    mean.model = list(armaOrder = c(0, 0), include.mean = TRUE ), 
                    distribution.model = "norm")
# fit the model using data
model1 <- ugarchfit(spec1, logrets_ts)
#print results
print(model1)
plot(model1@fit$sigma, type='l')

# Second model: ARMA(0,0) with student-t distribution and zero mean
spec2 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                    mean.model = list(armaOrder = c(0, 0), include.mean = TRUE ), 
                    distribution.model = "std")
# fit the model using data
model2 <- ugarchfit(spec2, logrets_ts)
print(model2)

#Question 2.12
#Comparing the in fit sample of both models
infocriteria(model1)
infocriteria(model2)
#             Model1    Model2
#Akaike       5.449075  5.415063
#Bayes        5.468706  5.439602
#Shibata      5.449044  5.415013
#Hannan-Quinn 5.456537  5.424389
#The second model is better than the first model because it has lower information criteria values. 

# Set up rolling forecast for the first model
roll1 <- ugarchroll(spec1, data = logrets_ts, n.ahead = 1, forecast.length = (length(logrets_ts) - 300), refit.window = "recursive", solver = "hybrid")
roll1

# Extract VaR forecasts for the first model at 5%
VaR1 <- roll1@forecast$VaR[,1]  # Extract the 5% VaR forecasts
VaR1

# Set up rolling forecast for the second model
roll2 <- ugarchroll(spec2, data = logrets_ts, n.ahead = 1, forecast.length = (length(logrets_ts) - 300), refit.window = "recursive", solver = "hybrid")
roll2

# Extract VaR forecasts for the second model at 5%
VaR2 <- roll2@forecast$VaR[,1]  # Extract the 5% VaR forecasts
VaR2

# Perform VaR test for the first model
actual_returns <- tail(logrets_ts, length(VaR1))  # Get actual returns corresponding to VaR forecasts
VaRTest(alpha = 0.05, actual = actual_returns, VaR = VaR1)
# The unconditional coverage null hypothesis (H0) that the model correctly predicts the frequency of exceedances is rejected. 
# The conditional coverage null hypothesis (H0) that the model correctly predicts the frequency and independence of exceedances is also rejected.
# This suggests that the model does not provide a well-specified VaR forecast.

# Perform VaR test for the second model
VaRTest(alpha = 0.05, actual = actual_returns, VaR = VaR2)
# The unconditional coverage null hypothesis (H0) that the model correctly predicts the frequency of exceedances is rejected. 
# The conditional coverage null hypothesis (H0) that the model correctly predicts the frequency and independence of exceedances is also rejected.
# This suggests that the model does not provide a well-specified VaR forecast.