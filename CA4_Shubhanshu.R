# Shubhanshu Sharma
# CA4 Predictive modelling
# Dataset - Business Sectors in Ireland

# We are Reading CSV file into R

business_data <- read.csv("CA Data2.csv", header = TRUE)
business_data

new_data <- ts(business_data$Business.Value.in.K, start=c(2004, 1), end=c(2017, 1), frequency=4)
new_data

# We can use functions to determine various properties
# of the tiem series

start(new_data)
end(new_data)
frequency(new_data)

# Plotting the data

plot(new_data)

install.packages("forecast")
library("forecast")

opar <- par(no.readonly=TRUE)
par(mfrow=c(2,2))
ylim <- c(min(new_data), max(new_data))
plot(new_data, main="Raw time series")
plot(ma(new_data, 3), main="Simple Moving Averages (k=3)", ylim=ylim)
plot(ma(new_data, 7), main="Simple Moving Averages (k=7)", ylim=ylim)
plot(ma(new_data, 15), main="Simple Moving Averages (k=15)", ylim=ylim)


# Acf() plot
acf_result <- Acf(new_data) # autocorrelation
acf_result

# partial autocorrelation plot
pacf_result <- Pacf(new_data)

# Testing to check if time series is stationary
library(tseries)
# p-value < 0.05 shows that the time series is stationary
# Using the below code to check the stationarity
adf.test(new_data)

# Using example of ARIMA model 
# ARIMA and new_data forecasting example
# First plotting the time series and assessing its stationarity
library(forecast)
library(tseries)
plot(new_data)

# Chart data does not appear to be stationary
# So we assess presence of a trend in dataset using ndiffs
ndiffs(new_data)

# Since there is a trend, the series is differenced 
# once (lag=1 is the default) and saved as diff_new_data
# Note we had to difference the data by 1
diff_new_data <- diff(new_data, lag = 1)

# Plot the differenced time series
plot(diff_new_data)

# Show both side-by-side f comparison
opar <- par(no.readonly=TRUE)
par(mfrow=c(1,2))
plot(new_data)
plot(diff_new_data)
par(opar)

# And we'll assess the presence of a trend in the data
ndiffs(diff_new_data)

# Applying the ADF test to the differenced series 
# suggest that it’s now stationary, so we can 
# proceed to the next step
# Null hypothesis of ADF test = data needs to 
# be differenced to make t stationary
adf.test(diff_new_data)

# Identifying one or more reasonable models
# here we examine autocorrelation and partial
# autocorrelation plots for the differenced
# Nile time series
# autocorrelatioin plot
Acf(diff_new_data, main = "Autocorrelation plot for differeced time series")
# partial autocorrelation plot
Pacf(diff_new_data, main = "Partial autocorrelation plot for differeced time series")

# Fitting an ARIMA model -------------------------------------
# Note we use the original dataset for the ARIMA model
# and modify the d value to suit our earlier findings
# and d = 1
# We apply the model to the original time series
# and not diff_new_data
library(forecast)
arima_model <- Arima(new_data, order = c(0, 1, 2))
arima_model
# Accuracy measures
# The mean absolute percentage error (MAPE)
# measures prediction of accuracy
# so this is the forecast accuracy of the error

accuracy(arima_model)

# Evaluating model fit ---------------------------------------
# qqnorm produces a normal QQ plot of the values in y. 
# qqline adds a line to a “theoretical”, quantile-quantile plot 
# which passes through the probs quantiles, 
# by default the first and third quartiles
help("qqnorm")
qqnorm(arima_model$residuals)
qqline(arima_model$residuals)
# Box.test() function provides a test that autocorrelations 
# are all zero (H0). The results are significant, suggesting 
# the autocorrelations don ’t differ from zero.
# This ARIMA model appears to fit the data well.
Box.test(arima_model$residuals, type = "Ljung-Box")

# Forecast 3 years ahead for the given data time series
forecast(arima_model, 3)

# Plot function shows the forecast. Point estimates are given by the blue dots, 80 % and 95 % confidence bands 
# are represented by dark and light bands, respectively

plot(forecast(arima_model, 3), xlab = "Year", ylab = "Annual Flow")

# Automated ARIMA forecasting
# Comparing the automatic test against our manual method above

library(forecast)
auto_arima_model <- auto.arima(new_data)
auto_arima_model

accuracy(auto_arima_model)

# Comparing with manual selected model

accuracy((arima_model))
qqnorm(auto_arima_model$residuals)
qqline(auto_arima_model$residuals)
Box.test(auto_arima_model$residuals, type = "Ljung-Box")

forecast(auto_arima_model)
plot(forecast(auto_arima_model, 3), xlab = "Year", ylab = "Annual Flow")

