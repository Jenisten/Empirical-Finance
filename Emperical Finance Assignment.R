rm(list = ls())
lapply(c("car", "lmtest", "sandwich", "tseries", "quantmod", "PortfolioAnalytics", "ROI.plugin.quadprog", "readxl", "zoo", "vrtest", "forecast", "plt"), library, character.only = TRUE)

#Predictability of Asset Returns

#Question 1.6
#Reading the data
exchange <- read_excel("exchange.xls")
#Printing and plotting the data
print(head(exchange))
ts.plot(exchange)

#Converting the data into a time series
exchange_ts = ts(exchange$logreturn, start = 1, end = 262, frequency = 1)

#Determining the amount of lags
exchange_lags=10*log10(length(exchange_ts))
print(exchange_lags)

#Testing linear predictability with a Ljung Box test
Box.test(exchange_ts, lag = exchange_lags, type = "Ljung-Box")
# Since the p-value (0.08707) is greater than 0.05, we do not reject the null hypothesis. 
# This suggests that there is no significant autocorrelation up to the specified number of lags. 
# Further on, this indicates that the time series is not linearly predictable. 

#Question 1.7
#Testing for predictability using Lo-MacKinlay variance ratio test for 2 and day log returns
kvec <- c(2,3)
Lo.Mac(exchange_ts,kvec)
#                             M1         M2
# Variance ratio = 2  -0.2197445 -0.1922782 - We do not reject the null here because < 1.96
# Variance ratio = 3  -1.0414343 -0.9143965 - We do not reject the null here because < 1.96

#Question 1.8
#Plotting the ACF of the exchange rate
Acf(exchange_ts, lag= exchange_lags, main="ACF of Exchange Rate")
# The ACF plot shows that the autocorrelation is significant at lag 18 and 23.

#Question 1.9
#Plotting the PACF of the exchange rate
pacf(exchange_ts, lag= exchange_lags, main="PACF of Exchange Rate")
# The PACF plot shows that the partial autocorrelation is significant at lag 23. 

#Question 1.10
# Determining the best time series model using the auto.arima function
auto.arima(exchange_ts, d=0, D=0, ic=c("aic"), stepwise=FALSE, approximation=FALSE)
# Based on the AIC criterion, the best model is ARIMA(3,0,1) with a zero mean.

#Question 1.11
# Fitting the ARIMA model
BM_fit <- arima(exchange_ts, order=c(3,0,1))
BM_fit

# Testing for White Noise in the residuals of gthe ARMA(3,1) model
Box.test(BM_fit$residuals, lag = exchange_lags, type = "Ljung-Box")
# Since the p-value (0.86) is more than 0.05, we do not reject the null hypothesis. 

#TEST
BM_fit_t <- arima(exchange_ts, order=c(1,0,0))
BM_fit_t

# Testing for White Noise in the residuals of gthe ARMA(3,1) model
Box.test(BM_fit_t$residuals, lag = exchange_lags, type = "Ljung-Box")
# Since the p-value (0.86) is more than 0.05, we do not reject the null hypothesis. 