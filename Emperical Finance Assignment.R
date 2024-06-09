rm(list = ls())
#install.packages("GRS.test")
lapply(c("car", "lmtest", "sandwich", "tseries", "quantmod", "PortfolioAnalytics", "ROI.plugin.quadprog", "readxl", "zoo", "vrtest", "forecast", "FinTS", "moments", "rugarch", "GRS.test"), library, character.only = TRUE)

# Predictability of Asset Returns

# Question 1.6
# Reading the data. 
exchange <- read_excel("exchange.xls")
# Printing and plotting the data. 
print(head(exchange))
ts.plot(exchange)

# Converting the data into a time series. 
exchange_ts <- ts(exchange$logreturn, start = 1, end = 262, frequency = 1)

# Determining the amount of lags. 
exchange_lags <- 10*log10(length(exchange_ts))
print(exchange_lags)

# Testing linear predictability with a Ljung Box test. 
Box.test(exchange_ts, lag = exchange_lags, type = "Ljung-Box")
# Since the p-value (0.08707) is greater than 0.05, we do not reject the null hypothesis. 
# This suggests that there is no significant autocorrelation up to the specified number of lags. 
# Further on, this indicates that the time series is not linearly predictable. 

# Question 1.7
# Testing for predictability using Lo-MacKinlay variance ratio test for 2 and day log returns. 
kvec <- c(2,3)
Lo.Mac(exchange_ts,kvec)
#                             M1         M2
# Variance ratio = 2  -0.2197445 -0.1922782 - We do not reject the null here because < 1.96. 
# Variance ratio = 3  -1.0414343 -0.9143965 - We do not reject the null here because < 1.96. 

# Question 1.8
# Plotting the ACF of the exchange rate. 
Acf(exchange_ts, lag= exchange_lags, main="ACF of Exchange Rate")
# The ACF plot shows that the autocorrelation is significant at lag 18 and 23.

# Question 1.9
# Plotting the PACF of the exchange rate. 
Pacf(exchange_ts, lag= exchange_lags, main="PACF of Exchange Rate")
# The PACF plot shows that the partial autocorrelation is significant at lag 23. 

# Question 1.10
# Determining the best time series model using the auto.arima function. 
auto.arima(exchange_ts, d=0, D=0, ic=c("aic"), stepwise=FALSE, approximation=FALSE)
# Based on the AIC criterion, the best model is ARIMA(3,0,1) with a zero mean.
auto.arima(exchange_ts, d=0, D=0, ic=c("bic"), stepwise=FALSE, approximation=FALSE)
# Based on the BIC criterion, the best model is ARIMA(0,0,0) with a zero mean.

# Question 1.11
# Fitting the ARIMA model. 
BM_fit <- arima(exchange_ts, order=c(0,0,0))
BM_fit
# Testing for White Noise in the residuals of the ARMA(0,0) model. 
Box.test(BM_fit$residuals, lag = exchange_lags, type = "Ljung-Box")
# Since the p-value (0.08707) is more than 0.05, we do not reject the null hypothesis. 
# This suggests that the residuals are white noise.


# Question 2.7
# Reading the data. 
logrets <- read_excel("logreturns.xlsx")

# Converting the data into a time series. 
logrets_ts <- ts(logrets$logreturns, start = 1, end = 1000, frequency = 1)

# Squaring Log returns. 
logrets_sq <- logrets_ts^2

# Determining the amount of lags. 
logrets_lenght <- length(logrets_ts)
logrets_lenght
logrets_lags <- 10*log10(logrets_lenght)
print(logrets_lags)

# Testing for linear predictability with a Ljung Box test. 
Box.test(logrets_sq, lag = logrets_lags, type = "Ljung-Box")
# The p-value is less than 0.05, so we reject the null hypothesis.
# This suggests that the time series is linearly predictable.

# Question 2.8
# Testing for predictability using Lo-MacKinlay variance ratio test for 2 and day log returns. 
kvec_2 <- c(2)
Lo.Mac(logrets_sq,kvec_2)
#                          M1       M2
# Variance ratio = 2 6.649091 2.188877 - We do reject the null here because > 1.96. 
# The time series does not follow a random walk.

# Question 2.9
# Conducting the ARCH-LM test. 
ArchTest(logrets_sq, lags = 12, demean = TRUE)
# The p-values are less than 0.05, so we reject the null hypothesis.
# This suggests that there are ARCH effects present in squared log returns. 

# Questiion 2.10
# Plotting the ACF of the log returns squared. 
Acf(logrets_sq, lag= logrets_lags, main="ACF of Log Returns Squared")

# Questiion 2.11
# Creating the first model: ARMA(0,0)-GARCH(1,1) with Gaussian distribution and non-zero mean. 
# Creating specification. 
spec1 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                    mean.model = list(armaOrder = c(0, 0), include.mean = TRUE ), 
                    distribution.model = "norm")
# Fitting the model using data. 
model1 <- ugarchfit(spec1, logrets_ts)
# Printing results. 
print(model1)
plot(model1@fit$sigma, type='l', main="ARMA(0,0)-GARCH(1,1) with Gaussian distribution and zero mean.")

# Creating the Second model: ARMA(0,0)-GARCH(1,1) with student-t distribution and zero mean. 
spec2 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                    mean.model = list(armaOrder = c(0, 0), include.mean = TRUE ), 
                    distribution.model = "std")
# Fitting the model using data. 
model2 <- ugarchfit(spec2, logrets_ts)
print(model2)
plot(model2@fit$sigma, type='l', main="ARMA(0,0)-GARCH(1,1) with student-t distribution and zero mean.")

# Extracting squared standardized residuals. 
resid1_sq <- residuals(model1, standardize = TRUE)^2
resid2_sq <- residuals(model2, standardize = TRUE)^2

# Plotting the ACF and PACF for the squared standardized residuals of the first model. 
par(mfrow = c(2, 2))
Acf(resid1_sq, main="ACF of Squared Residuals - Gaussian")
Pacf(resid1_sq, main="PACF of Squared Residuals - Gaussian")

# Plotting the ACF and PACF for the squared standardized residuals of the second model. 
Acf(resid2_sq, main="ACF of Squared Residuals - Student-t")
Pacf(resid2_sq, main="PACF of Squared Residuals - Student-t")

# Conducting Ljung-Box test for the first model. 
lb_model1 <- Box.test(resid1_sq, lag = 12, type = "Ljung-Box")
print(lb_model1)
# The p-value (0.9566) is above 0.05, so we do not reject the null hypothesis.

# Conducting Ljung-Box test for the second model. 
lb_model2 <- Box.test(resid2_sq, lag = 12, type = "Ljung-Box")
print(lb_model2)
# The p-value (0.9527) is above 0.05, so we do not reject the null hypothesis.

# Question 2.12
# Comparing the in fit sample of both models. 
infocriteria(model1)
infocriteria(model2)
#             Model1    Model2
# Akaike       5.449075  5.415063
# Bayes        5.468706  5.439602
# Shibata      5.449044  5.415013
# Hannan-Quinn 5.456537  5.424389
# The second model is better than the first model because it has lower information criteria values. 

# Question 2.13
# Setting up rolling forecast for the first model. 
roll1 <- ugarchroll(spec1, data = logrets_ts, n.ahead = 1, forecast.length = (length(logrets_ts) - 300), refit.window = "recursive", solver = "hybrid")
roll1

# Extracting VaR forecasts for the first model at 5%. 
VaR1 <- roll1@forecast$VaR[,1] 
VaR1

# Setting up rolling forecast for the second model. 
roll2 <- ugarchroll(spec2, data = logrets_ts, n.ahead = 1, forecast.length = (length(logrets_ts) - 300), refit.window = "recursive", solver = "hybrid")
roll2

# Extracting VaR forecasts for the second model at 5%. 
VaR2 <- roll2@forecast$VaR[,1]
VaR2

# Getting actual returns corresponding to VaR forecasts. 
actual_returns <- tail(logrets_ts, length(VaR1))
# Performing VaR test for the first model. 
VaRTest(alpha = 0.05, actual = actual_returns, VaR = VaR1)
# The unconditional coverage null hypothesis (H0) that the model correctly predicts the frequency of exceedances is rejected. 
# The conditional coverage null hypothesis (H0) that the model correctly predicts the frequency and independence of exceedances is also rejected.
# This suggests that the model does not provide a well-specified VaR forecast.

# Perform VaR test for the second model. 
VaRTest(alpha = 0.05, actual = actual_returns, VaR = VaR2)
# The unconditional coverage null hypothesis (H0) that the model correctly predicts the frequency of exceedances is rejected. 
# The conditional coverage null hypothesis (H0) that the model correctly predicts the frequency and independence of exceedances is also rejected.
# This suggests that the model does not provide a well-specified VaR forecast.


# Question 3.7
# Reading the data. 
factor <- read_excel("factordata.xlsx")

# Converting all data into time series. 
asset1_ts = ts(factor$asset1, start = 1, end = 1000, frequency = 1)
asset2_ts = ts(factor$asset2, start = 1, end = 1000, frequency = 1)
asset3_ts = ts(factor$asset3, start = 1, end = 1000, frequency = 1)
factor1_ts = ts(factor$factor1, start = 1, end = 1000, frequency = 1)
factor2_ts = ts(factor$factor2, start = 1, end = 1000, frequency = 1)

# Plotting the data. 
ts.plot(asset1_ts)
ts.plot(asset2_ts)
ts.plot(asset3_ts)
ts.plot(factor1_ts)
ts.plot(factor2_ts)

# Fitting the linear regression model for asset1. 
model1 <- lm(asset1_ts ~ factor1_ts + factor2_ts)
summary(model1)
# The p-value is less than 0.05, so we reject the null hypothesis. 

# Question 3.8
# Conducting diagnostics. 

# Testing for autocorrelation in the residuals. 
# Ljung-Box test. 
Box.test(model1$residuals, lag = 10*log10(length(asset1_ts)), type = "Ljung-Box")
# The p-value(0.6848) is greater than 0.05, so we do not reject the null hypothesis. 
# This suggests that the residuals are white noise. 

# Breusch-Godfrey test.  
bgtest(model1, order = 12)
# The p-value(0.5776) is above 0.05, so we do not reject the null hypothesis.  
# This suggests that there is no autocorrelation in the model. 

# Testing for normality in the residuals.
# Jarque-Bera test.
jarque.bera.test(model1$residuals)
# The p-value(0.5336) is above 0.05, so we do not reject the null hypothesis.
# This suggests that the residuals are normally distributed.

# Testing for ARCH effects in the residuals. 
# ARCH-LM test.
ArchTest(model1$residuals, lags = 12) 
# The p-value(0.7839) is above 0.05, so we do not reject the null hypothesis. 
# This suggest ARCH effects in the residuals. 

# Question 3.10
# Conducting the GRS test. 
GRS.test(factor[,1], factor[,4:5])
# The p-value(0.4092949) is above 0.05, so we can not reject the null hypothesis.
# This suggests that the factors are jointly significant in explaining the returns of asset1.

# Question 3.11 
# Building the linear regression models for asset2 and asset3. 
model2 <- lm(asset2_ts ~ factor1_ts + factor2_ts)
model3 <- lm(asset3_ts ~ factor1_ts + factor2_ts)

# Extracting the residuals from each model. 
residuals1 <- model1$residuals
residuals2 <- model2$residuals
residuals3 <- model3$residuals

# Creating a matrix of betas from each model, removing the intercepts from the matrix and transposing it. 
betas_matrix <- t(sapply(list(model1, model2, model3), function(x) coef(x)[-1]))
betas_matrix

# Creating a variance-covariance matrix of the factors. 
var_cov_matrix_f <- cov(factor[, 4:5])
var_cov_matrix_f

# Creating the variance-covariance matrix of model1, model2, and model3 residuals and removing the off-diagonal elements from the variance-covariance matrix. 
D_matrix <- diag(c(var(residuals1), var(residuals2), var(residuals3)))
D_matrix

# Calculating the omega matrix. 
omega <- betas_matrix %*% var_cov_matrix_f %*% t(betas_matrix)+D_matrix
omega

# Question 3.12. 
# Calculating the minimum variance portfolio weights. 
# Creating a vector of ones (ι). 
ones <- rep(1, ncol(omega))

# Calculating the inverse of the variance-covariance matrix (Ω^-1). 
inv_omega <- solve(omega)

# Calculating the minimum variance portfolio weights (w). 
weights <- inv_omega %*% ones / (t(ones) %*% inv_omega %*% ones)
weights

# Calculating portfolio returns. 
portfolio_returns <- as.matrix(factor[, 1:3]) %*% weights
portfolio_returns

portfolio_returns_t <- weights[1] %*% factor$asset1 + weights[2] %*% factor$asset2 + weights[3] %*% factor$asset3
portfolio_returns_t

# Calculating the mean and standard deviation of the portfolio returns. 
mu_p <- mean(portfolio_returns)
sigma_p <- sd(portfolio_returns)
mu_p
sigma_p

# Calculating the 5% VaR. 
VaR_5 <- mu_p + qnorm(0.05) * sigma_p
VaR_5
