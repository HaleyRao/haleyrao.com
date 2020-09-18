# load all relevant libraries
library(tseries)
library(forecast)
library(strucchange)
library(timeSeries)
library(vars)
library(tidyverse)
library(rugarch)

#Read in the data and adjust the length of houses to match the same timespan as vehicles
houses <- read.csv("HSN1FNSA.csv")
houses <- houses[-c(1:156), ]

#Create a time series object for each data set, specifying the start and frequency
houses_ts <- ts(houses$HSN1FNSA, start = c(1976, 1), frequency = 12)

# Plot the time series, ACF, and PACF for houses sold
tsdisplay(houses_ts, main="Number of One Family Houses Sold in the U.S.")

# Take the seasonal difference and first difference of the data and then display the stationary series
houses_ts %>% diff(lag=12) %>% diff() %>% ggtsdisplay()

# Fit an ARIMA model that has non-seasonal AR and MA components, a seasonal MA component, and both differences
house_model <- Arima(houses_ts, order = c(4,1,1), seasonal = c(0,1,1))

# Plot the residuals against the fitted values for the house model
plot(house_model$residuals ~ house_model$fitted, main="House Model Residuals", 
     ylab="Residuals", xlab="Fitted Values")

# Plot the ACF and PACF of the house model residuals
acf(house_model$residuals, main="ACF of House Model Residuals")
pacf(house_model$residuals, main="PACF of House Model Residuals")

# Compute and then plot the empirical fluctuation process based on recursive residuals and cumulative 
# sums for the house model
plot(efp(house_model$residuals~1, type = "Rec-CUSUM"))

# Compute and then plot the recursive residuals for the house model
plot(recresid(house_model$residuals~1), pch=16,ylab="Recursive Residuals", main = "House Model Residuals")

# Perform a Ljung-Box test on the residuals to check for autocorrelation, and plot the residuals, their ACF plot, and a histogram to show their distribution
checkresiduals(house_model)

# Forecast the house model 12-steps ahead and plot it along with the time series from 2010 to 2021
forecast(house_model, h=12)
plot(forecast(house_model, h=12), main="12-step Ahead Forecast for Houses Sold", xlab="Year", ylab="Units Sold (in thousands)", xlim=c(2010, 2021))

# Split data into training and testing sets (the testing set is the last 12 observation of the data)
houses_ts_train <- window(houses_ts, end = c(2019, 1))
houses_ts_test <- window(houses_ts, start = c(2019, 2))

# Fit the house model again with training data
# Calculate MAPE for the forecasts of the house_model on the testing data
house_model_2 <- Arima(houses_ts_train, order = c(4,1,1), seasonal = c(0,1,1))
accuracy(forecast(house_model_2, houses_ts_test, h=12))[5]

# Fit an ARIMA model with the ttrainig data
# Calculate MAPE for the forecasts of the ARIMA model on the testing data
house_arima_model <- auto.arima(houses_ts_train)
accuracy(forecast(house_arima_model, houses_ts_test, h=12))[5]

# Fit a Holt-Winters model with the training data
# Calculate MAPE for the forecasts of the Holt-Winters model on the testing data
accuracy(forecast(HoltWinters(houses_ts_train), houses_ts_test, h=12))[5]

# Fit an ETS model with the training data
# Calculate MAPE for the forecasts of the ETS model on the testing data
accuracy(forecast(ets(houses_ts_train), houses_ts_test, h=12))[5]

# Take an average of the individual forecasts
# Calculate the MAPE of the average of the forecasts using the testing data
house_combo <- (forecast(house_model_2, houses_ts_test, h=12)[["mean"]] + forecast(house_arima_model, houses_ts_test, h=12)[["mean"]] + forecast(HoltWinters(houses_ts_train), houses_ts_test, h=12)[["mean"]] + forecast(ets(houses_ts_train), houses_ts_test, h=12)[["mean"]])/4

accuracy(house_combo, houses_ts_test)[5]








